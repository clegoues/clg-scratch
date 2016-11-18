(* narrow down relevant tests, particularly for a ManyBugs scenario.
 * borrows heavily from GenProg machinery *)

open Global
open Fileprocess


let main () = begin
(* FIXME: parse options *)

  debug_out := open_out !debug_str ; 
  let filemap = from_source !program in
()    


end
