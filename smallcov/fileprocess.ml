open Global
open Cil
open Cilinstr

let compiler_command = ref ""
let diff_files = ref ""
let dprefix = ref "diffs"
let funcs = ref ""

let _ = options := !options @ 
[
  "--compcmd", Arg.Set_string compiler_command, "X use X as compiler command";
  "--diffs", Arg.Set_string diff_files, "X diff files (one, or a list) to indicate lines modified by human.";
  "--dprefix", Arg.Set_string dprefix, "X prefix to find diff files.  Default: diffs/";
  "--fns", Arg.Set_string funcs, "X functions to be instrumented (either a file listing, or a pair of a filename;comma-separated fn-list).";
]

let get_available_fns () = 
  match !funcs with
    "" -> StringMap.empty
  | _ ->  
    lfoldl
      (fun acc line ->
        match Str.split semicolon line with
          [file; fns] -> 
            let fns = Str.split comma fns in
              StringMap.add file fns acc
        | _ -> acc
      ) (StringMap.empty) (get_files !funcs)

let get_available_diffs () = 
  (* return a map between regular filenames and difffile names, as available *)
  match !diff_files with
    "" -> StringMap.empty
  | _ ->
    (* assumed naming format: fname.c-diff *)
    let diff_suff = Str.regexp "-diff" in
      lfoldl (fun acc fname -> 
        let dfile = 
          if Sys.file_exists fname then fname 
          else Filename.concat !dprefix fname 
        in 
        let loc = Str.search_backward diff_suff fname (String.length fname) in
        let actual_fname = String.sub fname 0 loc in
          debug "fname: %s\n" actual_fname;
          StringMap.add actual_fname dfile acc) (StringMap.empty) (get_files !diff_files)

let process_diff fname dfile = (* returns a list of ranges *)
  (* normal diff output (expected for mp scenarios) commands have the form RcT,
     where R and T are ranges and c is the command.  R and T can either be
     single numbers or ranges of the form n1,n2.  The first match checks if this
     is a change command.  The second gets the range before the command.  The
     third figures out if there's a comma.  *)
  let change_comm = Str.regexp "^[0-9]+\(,[0-9]+\)?\(a\|c\|d\)[0-9]+\(,[0-9]+\)?$" in
  let range = Str.regexp "^[0-9]+\(,[0-9]+\)?" in
    lfoldl (fun acc line -> 
      if Str.string_match change_comm line 0 then begin
        let _ = Str.string_match range line 0 in
        let matched = Str.matched_string line in 
          try 
            let char = Str.search_forward comma matched 0 in
            let frst = String.sub matched 0 char in 
            let slen = (String.length matched) - char - 1 in 
            let scnd = String.sub matched (char + 1) slen in 
              (int_of_string frst, int_of_string scnd) :: acc
          with Not_found -> begin
            let line = int_of_string matched in
              (line,line) :: acc
          end
      end else acc) [] (get_lines dfile)

let get_gloc = function 
| GType(_,loc) | GCompTag(_,loc) | GCompTagDecl(_,loc)
| GEnumTag(_,loc) | GEnumTagDecl(_,loc) | GVarDecl(_,loc)
| GVar(_,_,loc) | GFun(_,loc) | GAsm(_,loc) | GPragma(_,loc) -> loc
(* not handling GText, so hopefully it never happens.  Famous last words *)

(* precondition: ranges sorted by where they end *)
let findFnByRanges fname cfile ranges = begin
    let fname_regexp = Str.regexp_string fname in 
  (* drop ranges that end before a location starts.  Acceptable because globals
     are in the order they appear in the file and ranges are sorted by where
     they end.  May not need this? *)
  let rec dropranges loc ranges = 
    match ranges with 
    | [] -> []
    | (x,y) :: rs -> 
      if y < loc then dropranges loc rs
      else ranges
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
      | GFun(fd,loc) :: g :: gs -> begin
           try
         (* ignore globals in files we're not actually considering *)
             let _ = Str.search_forward fname_regexp loc.file 0 in
      (* this stupidity is due to the fact that we can tell where things start,
         but not where they end. *)
          let ranges = dropranges loc.line ranges in
          let keep,rest = 
            doOverlap (loc.line,((get_gloc g).line - 1)) false ranges 
          in
          let fnames = if keep then (fd.svar.vname :: fnames) else fnames in
            iterGlobals rest (g :: gs) fnames
       with Not_found -> 
    iterGlobals ranges (g :: gs) fnames
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

let instrument_files fmap coverage_outname source_dir = begin
  debug "a\n";
  let avail_dfiles = get_available_diffs () in
    debug "b\n";
  let avail_fspecs = get_available_fns () in
debug "c\n";
  let get_fns fname cfile = 
    debug "fname: %s\n" fname ;
    try
      StringMap.find fname avail_fspecs
    with Not_found ->
      let dfile = StringMap.find fname avail_dfiles in
      let ranges = process_diff fname dfile in
      let ranges = lsort (fun (x1,y1) (x2,y2) -> y1 - y2) ranges in 
        findFnByRanges fname cfile ranges 
  in
  let prototypes = ref StringMap.empty in
  let instrv = new instrumentVisitor prototypes coverage_outname in
    debug "d\n";
    StringMap.fold
      (fun fname cfile acc ->  
        let outname = Filename.concat source_dir fname  in
          debug "e\n";
        let fns = get_fns fname cfile in 
          debug "Functions modified:\n"; 
          liter (fun fname -> debug "\t%s\n" fname) fns;
          visitCilFile (instrv fns fname) cfile;
          cfile.globals <- 
            StringMap.fold (fun _ protos acc ->
              protos @ acc
            ) !prototypes cfile.globals;
          begin
            try
              cfile.globals <- toposort_globals cfile.globals;
            with MissingDefinition(s) ->
              debug "fileprocess: toposorting failure (%s)!\n" s;
          end;
          ensure_directories_exist outname;
          output_cil_file outname cfile;
          outname :: acc
      ) fmap []
end

let from_source (filename : string) = 
  let getfname fname = 
    if Sys.file_exists fname then fname else Filename.concat !prefix fname
  in
  let _,ext = split_ext filename in 
    match ext with
      "txt" -> lfoldl 
        (fun map fname ->
          StringMap.add fname (cil_parse (getfname fname)) map)
        (StringMap.empty) (get_lines filename)
    | "c" | "i" ->
      let parsed = cil_parse (getfname filename) in
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
