(* narrow down relevant tests, particularly for a ManyBugs scenario.
 * borrows heavily from GenProg machinery *)

open Global
open Fileprocess
open Runtests

let coverage_outname = ref "covered.out"
let instr_outdir = ref "instrumented" 

let _ =
  options := !options @
    [ 
      "--instrdir", Arg.Set_string instr_outdir, "X output instrumented code to X.  Default: \"instrumented/\"";
      "--covout", Arg.Set_string coverage_outname, "X write coverage info to X."; 
    ]

let time_at_start = Unix.gettimeofday () 

let main () = begin
  let aligned = Arg.align !options in
    Arg.parse aligned (usage_function aligned usage_msg) usage_msg;
  debug_out := open_out !debug_str ; 

  (* Bookkeeping information to print out whenever we're done ... *) 
  at_exit (fun () -> 
    debug "Wall-Clock Seconds Elapsed: %g\n" 
      ((Unix.gettimeofday ()) -. time_at_start) ;
      Stats2.print !debug_out "smallcov" ; 
    close_out !debug_out ;
    debug_out := stdout ; 
  ) ; 

  let coverage_outname = Filename.concat !instr_outdir !coverage_outname in
  let coverage_outname =
    if Filename.is_relative coverage_outname then 
      Filename.concat (Sys.getcwd ()) coverage_outname
    else coverage_outname
  in  
    debug "coverage_outname: %s\n" coverage_outname;
  (* step 1: instrument files *)
    (* next step: if available, read in diff file to only instrument functions modified by human developer *)
  let filemap = from_source !program in
  let instrumented_filenames = instrument_files filemap coverage_outname !instr_outdir in
  let coverage_srcname = 
    lfoldl (fun acc src -> acc^src^" ") "" instrumented_filenames
  in
  let coverage_exename =  Filename.concat !instr_outdir "compiled.out" in
  (* step 2: compile instrumented files *)
  let compiled = compile coverage_srcname coverage_exename in
    if compiled then
     (* step 3: run instrumented files on test cases *)
      run_tests coverage_outname coverage_exename coverage_srcname "coveringtests.txt"
    else 
      debug "failed to compile instrumented code; giving up.\n"
end ;;


try 
  main ()  
with 
  (* try to echo system errors to the debug file *) 
| Unix.Unix_error(e,s1,s2) as exc -> begin 
  let msg = Unix.error_message e in 
    debug "%s aborting: Unix error: %S %S %S\n" 
      Sys.argv.(0) msg s1 s2 ;
    raise exc 
end 
| e -> begin 
  debug "%s aborting: %s\n" Sys.argv.(0) (Printexc.to_string e) ;
  raise e 
end 
