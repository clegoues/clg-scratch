open Global
open Cil
open Cilinstr

let compiler_command = ref ""
let diff_files = ref ""

let _ = options := !options @ 
[
  "--compcmd", Arg.Set_string compiler_command, "X use X as compiler command";
  "--diffs", Arg.Set_string diff_files, "X diff files (one, or a list) to indicate lines modified by human."
]

let process_diffs () =
  let diff_suff = Str.regexp "-diff" in
  let diff_files = (* actual diff files, assumed naming format: fname.c-diff *)
    let _, ext = split_ext !diff_files in
      match ext with
        "txt" -> get_lines !diff_files 
      | _ -> [!diff_files]
  in
  (* get the actual files, paired with associated diff file *)
  let diff_files = 
    lmap (fun fname -> 
      let loc = Str.search_backward diff_suff fname (String.length fname) in
        String.sub fname 0 loc,fname) diff_files 
  in
  (* normal diff output (expected for mp scenarios) commands have the form RcT,
     where R and T are ranges and c is the command.  R and T can either be
     single numbers or ranges of the form n1,n2.  The first match checks if this
     is a change command.  The second gets the range before the command.  The
     third figures out if there's a comma.  *)
  let change_comm = Str.regexp "^[0-9]+\(,[0-9]+\)?\(a\|c\|d\)[0-9]+\(,[0-9]+\)?$" in
  let range = Str.regexp "^[0-9]+\(,[0-9]+\)?" in
  let comma = Str.regexp_string "," in
    lfoldl (fun acc (fname,diffile) ->
      let linepairs = 
        lfoldl (fun acc line -> 
          if Str.string_match change_comm line 0 then begin
            let rangem = Str.string_match range line 0 in
            let matched = Str.matched_string line in 
              if Str.string_match comma matched 0 then begin
                let char = Str.search_forward comma matched 0 in
                let frst = String.sub matched 0 char in 
                let scnd = String.sub matched (char + 1) (String.length matched) in 
                  (int_of_string frst, int_of_string scnd) :: acc
              end else 
                let line = int_of_string matched in
                  (line,line) :: acc
          end else acc) [] (get_lines diffile)
      in
        StringMap.add fname linepairs acc 
    ) (StringMap.empty) diff_files 
    

let get_gloc = function 
| GType(_,loc) | GCompTag(_,loc) | GCompTagDecl(_,loc)
| GEnumTag(_,loc) | GEnumTagDecl(_,loc) | GVarDecl(_,loc)
| GVar(_,_,loc) | GFun(_,loc) | GAsm(_,loc) | GPragma(_,loc) -> loc
(* not handling GText, so hopefully it never happens.  Famous last words *)

(* precondition: ranges sorted by where they end *)
let findFunctions cfile ranges = begin
  (* drop ranges that end before a location starts.  Acceptable because globals
     are in the order they appear in the file and ranges are sorted by where
     they end.  May not need this? *)
  let rec dropranges loc = function
    | [] -> []
    | (x,y) :: rs -> 
      if y < loc then dropranges loc rs
      else rs
  in
  (* return whether we should retain this function, and any ranges that are
     unhandled afterwards *)
  let rec doOverlap (fstart,fend) keep ranges  =
    match ranges with
      (x,y) :: rs -> 
        if x > fend then keep, ranges
        else 
          doOverlap (fstart,fend) (x >= fstart || y <= fend) rs
    | [] -> keep, []
  in
  let rec iterGlobals ranges globs fnames = 
    if (llen ranges) == 0 then fnames
    else begin
      match globs with
    (* if it's the last function and we have ranges left, assume that they fall in
       that last function *)
      | [GFun(fd,loc)] -> fd.svar.vname :: fnames 
      | [x] -> fnames
      | GFun(fd,loc) :: g :: gs -> 
      (* this stupidity is due to the fact that we can tell where things start,
         but not where they end. *)
        begin        
          let ranges = dropranges loc.line ranges in
          let keep,rest = 
            doOverlap (loc.line,((get_gloc g).line - 1)) false ranges 
          in
          let fnames = if keep then (fd.svar.vname :: fnames) else fnames in
            iterGlobals rest (g :: gs) fnames
        end
      | _ :: gs -> iterGlobals ranges gs fnames
    end
  in
    iterGlobals ranges cfile.globals []
end

class instrumentVisitor prototypes instr_outname fns fname = object
  inherit nopCilVisitor
  val cstmt = fst (!fill_va_table ())
  val mutable declared = false

  method vglob g =
    let missing_proto n vtyp =
      match vtyp with
      | TFun(_,_,_,tattrs) ->
        List.exists (fun tattr ->
          match tattr with
          | Attr("missingproto",_) -> true
          | _ -> false
        ) tattrs
      | _ -> false
    in
      if not declared then begin
        declared <- true;
        prototypes := snd (!fill_va_table ());
      end;
      (* Replace CIL-provided prototypes (which are probably wrong) with the
         ones we extracted from the system headers; but keep user-provided
         prototypes, since the user may have had a reason for specifying them *)
      match g with
      | GVarDecl(vi,_) when (StringMap.mem vi.vname !prototypes) ->
        if missing_proto vi.vname vi.vtype then begin
          ChangeDoChildrenPost([], fun gs -> gs)
        end else begin
          prototypes := StringMap.remove vi.vname !prototypes;
          ChangeDoChildrenPost([g], fun gs -> gs)
        end
      | _ -> ChangeDoChildrenPost([g], fun gs -> gs)

  method vfunc fd = 
    (* In a world without diffs, we would only instrument functions in the
       actual file under consideration (and not headers).  For now I'm assuming
       that we have diffs indicating where the humans modified things, so we're
       only trying to instrument functions touched by the actual human *)
    if lmem fd.svar.vname fns then begin
      let print_str = "if (fout == 0) {\n fout = fopen(%g:fout_g,%g:wb_arg);\n}\n fprintf(fout, %g:str);\nfflush(fout);\n"
      in
      let newstmt = cstmt print_str 
        [("fout_g",Fg instr_outname);("str",Fg(fname)); ]
      in
        ChangeDoChildrenPost(fd, (fun fd ->
          fd.sbody.bstmts <- newstmt :: fd.sbody.bstmts;
          fd))
    end else SkipChildren
end

(* diffiles maps filanems to lists of line ranges *)
let instrument_files (fmap) coverage_outname source_dir diffiles = begin
  let prototypes = ref StringMap.empty in
  let instrv = new instrumentVisitor prototypes coverage_outname in
  StringMap.fold
    (fun fname cfile accum ->  
      let outname = Filename.concat source_dir fname  in
        (* assuming all files have diffs, for now *)
      let ranges = StringMap.find fname diffiles in
      let ranges = lsort (fun (x1,y1) (x2,y2) -> y1 - y2) ranges in 
      let fns = findFunctions cfile ranges in
      debug "Functions modified:\n"; 
      liter (fun fname -> debug "\t%s\n" fname) fns;
        visitCilFile (instrv fns fname) cfile;
        cfile.globals <- 
          StringMap.fold (fun _ protos accum ->
            protos @ accum
          ) !prototypes cfile.globals;
        begin
          try
            cfile.globals <- toposort_globals cfile.globals;
          with MissingDefinition(s) ->
            debug "fileprocess: toposorting failure (%s)!\n" s;
      end;
      ensure_directories_exist outname;
      output_cil_file outname cfile;
      outname :: accum
    ) fmap []
end

let from_source (filename : string) = 
  let _,ext = split_ext filename in 
    match ext with
      "txt" -> lfoldl 
        (fun map fname ->
          StringMap.add fname (cil_parse fname) map)
        (StringMap.empty) (get_lines filename)
    | "c" | "i" ->
      let parsed = cil_parse filename in
        StringMap.add filename parsed (StringMap.empty)
    | _  ->  abort "Unexpected file extension %s in fileProcess#from_source.  Permitted: .c, .i,, .txt" ext

let compile src_outname exec_outname =
  let base_command = 
    match !compiler_command with
    | "" -> 
      "__COMPILER_NAME__ -o __EXE_NAME__ __SOURCE_NAME__ __COMPILER_OPTIONS__ "^
        "2>/dev/null >/dev/null"
    |  x -> x
  in
  let cmd = Global.replace_in_string base_command
    [
      "__COMPILER_NAME__", !compiler_name ;
      "__COMPILER_OPTIONS__", !compiler_options ;
      "__SOURCE_NAME__", src_outname ;
	  "__EXE_NAME__", exec_outname
    ]
  in
    match Unix.system cmd with
    | Unix.WEXITED(0) -> true
    | _ -> debug "\t%s %s fails to compile\n" src_outname exec_outname ;
      false
