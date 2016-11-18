open Global
open Cil

let instr_outdir = ref "instrumented" 
let _ = options := !options @ 
[
  "--instrdir", Arg.Set_string instr_outdir, "X output instrumented code to X.  Default: \"instrumented/\""
]

let from_source (filename : string) = begin
  let one_file fname = Frontc.parse filename () in
  let _,ext = split_ext filename in 
    match ext with
      "txt" ->
        lfoldl 
          (fun map fname ->
            StringMap.add fname (one_file fname) map)
          (StringMap.empty) (get_lines filename)
    | "c" | "i" ->
      let parsed = one_file filename in
        StringMap.add filename parsed (StringMap.empty)
    | _  ->  abort "Unexpected file extension %s in CilRep#from_source.  Permitted: .c, .i, .cu, .cu, .txt" ext
end

(* Only instrument functions in the actual file under consideration. *)
let should_instrument_loc fname loc = true (* fixme *)


let va_table = Hashtbl.create 10

let fill_va_table = ref 
  ((fun () -> failwith "fill_va_table uninitialized") : unit -> 
   (string -> (Global.StringMap.key * Cil.formatArg) list -> Cil.stmt) *
    Cil.global list Global.StringMap.t)

class instrumentVisitor prototypes fname instr_outname = object
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
    if should_instrument_loc fname fd.svar.vdecl then begin
      let stmt_str = "if (fout == 0) {\n fout = fopen(%g:fout_g,%g:wb_arg);\nfprintf(fout, %g:str);\nfflush(fout);\n"
      in
      let newstmt = cstmt print_str 
        [("fout_g",Fg coverage_outname);("str",Fg(str)); ]
      in
        ChangeDoChildrenPost(f,
                             (fun f ->
                               f.sbody.bstmts <- ifstmt :: f.sbody.bstmts;
                               f))
    end else SkipChildren
end

let instrument_files (fmap) = begin
  let prototypes = ref StringMap.empty in
  let instrv = new instrumentVisitor prototypes in

  StringMap.iter 
    (fun fname cfile ->  
      let source_dir,_,_ = split_base_subdirs_ext !instr_outdir in 
      let outname = Filename.concat source_dir fname  in
        visitCilFile (instrv fname outname) cfile;
        file.globals <- 
          StringMap.fold (fun _ protos accum ->
            protos @ accum
          ) !prototypes file.globals;
        begin
          try
            file.globals <- toposort_globals file.globals;
          with MissingDefinition(s) ->
            debug "cilRep: toposorting failure (%s)!\n" s;
      end;
      ensure_directories_exist outname;
      output_cil_file outname cfile
    ) fmap
end


(* For most functions, we would like to use the prototypes as defined in the
   standard library used for this compiler. We do this by preprocessing a simple
   C file and extracting the prototypes from there. Since this should only
   happen when actually computing fault localization, the prototypes are lazily
   cached in the va_table. fill_va_table is called by the coverage visitor to
   fill the cache (if necessary) and retrieve the cstmt function along with the
   list of function declarations. *) 
let _ = 
  fill_va_table := (fun () -> 
  let vnames =
    [ "fclose"; "fflush"; "fopen"; "fprintf"; "memset" ]
  in
  if Hashtbl.length va_table = 0 then begin
    let void_t = Formatcil.cType "void *" [] in
    let source_file, chan = Filename.open_temp_file "tmp" ".c" in
    Printf.fprintf chan "#include <stdio.h>\n";
    Printf.fprintf chan "#include <string.h>\n";
    Printf.fprintf chan "FILE * _coverage_fout;\n";
    Printf.fprintf chan "int main() { return 0; }\n"; 
    close_out chan;
    let temp_variant = (new patchCilRep) in 

    let preprocessed = Filename.temp_file "tmp" ".c" in
    debug "fileprocess: preprocessing IO function signatures: %s %s\n" 
      source_file preprocessed;
    let cleanup () =
      if Sys.file_exists source_file then
        Sys.remove source_file;
      if Sys.file_exists preprocessed then
        Sys.remove preprocessed
    in
    if temp_variant#preprocess source_file preprocessed then begin
      try
        let cilfile = 
          try cil_parse preprocessed 
          with e -> 
            debug "cilrep: fill_va_table: Frontc.parse: %s\n" 
              (Printexc.to_string e) ; raise e 
        in
        if !Errormsg.hadErrors then
          Errormsg.parse_error
            "cilRep: fill_va_table: failure while preprocessing stdio header file declarations\n";
        iterGlobals cilfile (fun g ->
          match g with
          | GVarDecl(vi,_) | GVar(vi,_,_) when lmem vi.vname vnames ->
            let decls = ref [] in
            let visitor = object (self)
              inherit nopCilVisitor

              method private depend t =
                match t with
                | TComp(ci,_) -> decls := GCompTagDecl(ci,locUnknown) :: !decls
                | TEnum(ei,_) -> decls := GEnumTagDecl(ei,locUnknown) :: !decls
                | _ -> ()

              method vtype t =
                match t with
                | TNamed(_) ->
                  ChangeDoChildrenPost(unrollType t, fun t -> self#depend t; t)
                | _ ->
                  self#depend t; DoChildren
            end in
            let _ = visitCilGlobal visitor g in
            let decls = g :: !decls in
            Hashtbl.add va_table vi.vname (vi,decls,true)
          | _ -> ()
        )
      with Frontc.ParseError(msg) ->
      begin
        debug "cilRep: fill_va_table: %s\n" msg;
        Errormsg.hadErrors := false
      end
    end;
    debug "cilRep: done preprocessing IO function signatures\n";
    cleanup()
  end;
  let static_args = lfoldl (fun lst x ->
      let is_fout = x = "_coverage_fout" in
      if not (Hashtbl.mem va_table x) then begin
        let vi = makeVarinfo true x void_t in
        Hashtbl.add va_table x (vi, [GVarDecl(vi,locUnknown)], is_fout)
      end;
      let name = if is_fout then "fout" else x in
      let vi, _, _ = Hashtbl.find va_table x in
      (name, Fv vi) :: lst
    ) [("wb_arg", Fg("wb")); ("a_arg", Fg("a"));] vnames
  in
  let cstmt stmt_str args = 
    Formatcil.cStmt ("{"^stmt_str^"}") (fun _ -> failwith "no new varinfos!")  !currentLoc 
    (args@static_args)
  in
  let global_decls = lfoldl (fun decls x ->
    match Hashtbl.find va_table x with
    | (_, gs, true) -> StringMap.add x gs decls
    | _             -> decls
    ) StringMap.empty vnames
  in
  cstmt, global_decls
  ) 
