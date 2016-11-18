open Str
open Printf
open Unix
open Pervasives

let debug_out = ref stdout 
(**/**)
(** we copy all debugging output to a file and to stdout *)
let debug ?force_gui:(force_gui=false) fmt = 
  let k result = begin
      output_string !debug_out result ; 
      output_string stdout result ; 
      flush stdout ; 
      flush !debug_out;
  end in
    Printf.kprintf k fmt 

let abort fmt = 
  let k result = begin
      output_string !debug_out result ; 
      output_string stdout result ; 
      flush stdout ; 
      flush !debug_out;
    exit 1 
  end in
    debug "\nABORT:\n\n" ; 
    Printf.kprintf k fmt 

let split_ext name =
  try 
    let base = Filename.chop_extension name in
    let ext = String.sub name ((String.length base)+1)
      ((String.length name) - ((String.length base)+1))
    in 
      base,ext
  with _ -> name,""

let split_base_subdirs_ext name =
  try 
    let base = Filename.basename name in
    let basename,ext = split_ext base in
      Filename.dirname name,basename,ext
  with _ -> "",name,""

let lfoldl = List.fold_left
let liter = List.iter
let lmem = List.mem
let llen = List.length

let (--) i j = 
  let rec aux n acc =
    if n < i then acc else aux (n-1) (n :: acc)
  in aux j []

let replace_in_string base_string list_of_replacements = 
  List.fold_left (fun acc (literal,replacement) ->
    let regexp = Str.regexp (Str.quote literal) in
      Str.global_replace regexp replacement acc 
  ) base_string list_of_replacements 

(** given "a/b/c.txt", create "a/" and then "a/b/" if they don't already exist *)
let rec ensure_directories_exist filename = 
  match split_base_subdirs_ext filename with
  | "",_,_ | ".",_,_ | "/",_,_ -> () 
  | dirname,_,_ -> 
    ensure_directories_exist dirname ; 
    (try Unix.mkdir dirname 0o755 with _ -> ())

let get_lines (filename : string) : string list = 
  let fin = open_in filename in
  let res = ref [] in
    (try
       while true do
         res := (input_line fin) :: !res
       done
     with End_of_file -> close_in fin);
    List.rev !res

module OrderedString =
struct
  type t = string
  let compare = compare
end

module StringMap = Map.Make(OrderedString)

let program = ref ""
let debug_str = ref "debug.txt"

let options = ref [
  "--program", Arg.Set_string program, "X repair X";
  "--debug", Arg.Set_string debug_str, "X print debug to X";
] 

let usage_function aligned usage_msg x = 
  debug "usage: unknown option %s\n" x;
  Arg.usage aligned usage_msg; abort "usage"

