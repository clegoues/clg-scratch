(* narrow down relevant tests, particularly for a ManyBugs scenario.
 * borrows heavily from GenProg machinery *)

open Global
open Fileprocess

let time_at_start = Unix.gettimeofday () 

let main () = begin
  let aligned = Arg.align !options in
  let usage_msg = "smallcov: figure out which test cases touch a file/function of interest" in
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

  let filemap = from_source !program in
    instrument_files filemap
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
