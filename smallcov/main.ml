(* narrow down relevant tests, particularly for a ManyBugs scenario.
 * borrows heavily from GenProg machinery *)

open Global
open Fileprocess
open Runtests

let coverage_outname = ref "covered.out"
let instr_outdir = ref "instrumented" 
let report_outname = ref "smallcov-report.txt"

let _ =
  options := !options @
    [ 
      "--instrdir", Arg.Set_string instr_outdir, "X output instrumented code to X.  Default: \"instrumented/\"";
      "--covout", Arg.Set_string coverage_outname, "X write coverage info to X."; 
      "--report", Arg.Set_string report_outname, "X report output file.  Default: smallcov-report.txt"
    ]

let time_at_start = Unix.gettimeofday () 

let main () = begin
  let aligned = Arg.align !options in
    Arg.parse aligned (usage_function aligned usage_msg) usage_msg;
    debug_out := open_out !debug_str ; 
  debug ~force:true "Welcome to smallcov...\n";
  (* Bookkeeping information to print out whenever we're done ... *) 
  at_exit (fun () -> 
    debug ~force:true "Wall-Clock Seconds Elapsed: %g\n" 
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
    (* step 1: load and instrument files *)
  let filemap = from_source !program in
  let instrumented_filenames,function_names = instrument_files filemap coverage_outname !instr_outdir in
  let coverage_srcname = 
    lfoldl (fun acc src -> acc^src^" ") "" instrumented_filenames
  in
  let coverage_exename =  Filename.concat !instr_outdir "compiled.out" in
    (* step 2: compile instrumented files *)
    if compile coverage_srcname coverage_exename then (begin
      debug ~force:true "Files instrumented and compiled, about to run tests...\n";
      (* step 3: run instrumented files on test cases *)
      let covering_tests, unexpecteds = run_tests coverage_outname coverage_exename coverage_srcname in
        debug ~force:true "Tests run, generating report to %s\n" !report_outname;
        let fout = open_out !report_outname in
          Printf.fprintf fout "::REPORT::\n";
          Printf.fprintf fout "Functions modified:\n"; 
          liter (fun fname -> Printf.fprintf fout "\t%s\n" fname) function_names;
          Printf.fprintf fout "Covering tests:\n";
          liter
            (fun test ->
              Printf.fprintf fout "%s\n" (test_name test)) covering_tests;
          Printf.fprintf fout "Unexpected test results observed on:\n";
          liter 
            (fun test ->
              Printf.fprintf fout "%s\n" (test_name test)) unexpecteds;
          close_out fout
    end)
    else 
      abort "failed to compile instrumented code; giving up.\n"
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
