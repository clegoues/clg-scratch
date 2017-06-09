open Cil

let debug_out = ref Pervasives.stdout 
let verbose = ref false

let debug ?force:(force=false) fmt = 
  let k result = begin
    if !verbose || force then begin
      output_string !debug_out result ; 
      output_string Pervasives.stdout result ; 
      flush Pervasives.stdout ; 
      flush !debug_out;
    end
  end in
    Printf.kprintf k fmt 

let abort fmt = 
  let k result = begin
      output_string !debug_out result ; 
      output_string Pervasives.stdout result ; 
      flush Pervasives.stdout ; 
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
let lsort = List.sort
let lmap = List.map
let llen = List.length
let lfilt = List.filter

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

let get_files fname = 
  let _, ext = split_ext fname in
    match ext with
      "txt" -> get_lines fname
    | _ -> [fname]

module OrderedString =
struct
  type t = string
  let compare = compare
end

module StringMap = Map.Make(OrderedString)
let space = Str.regexp "[ \t]+" 
let comma = Str.regexp ","
let semicolon = Str.regexp ";"

let program = ref ""
let debug_str = ref "debug.txt"
let compiler_name = ref "gcc" 
let compiler_options = ref "" 
let prefix = ref ""

let options = ref [
  "--program", Arg.Set_string program, "X repair X";
  "--debug", Arg.Set_string debug_str, "X print debug to X";
  "--cc", Arg.Set_string compiler_name, "X compiler (default: gcc)";
  "--prefix", Arg.Set_string prefix, "X prefix to location of files";
  "--verbose", Arg.Set verbose, "Verbose debug output.";
] 

let usage_msg = "smallcov: figure out which test cases touch a file/function of interest"

let parse_options_in_file (file : string) : unit =
  let args = ref [ Sys.argv.(0) ] in 
    ( try
        let fin = open_in file in 
          (try while true do
              let line = input_line fin in
                if line <> "" && line.[0] <> '#' then begin 
        (* allow #comments *) 
        let words = Str.bounded_split space line 2 in 
        args := !args @ words 
      end 
    done with _ -> close_in fin) ;
  with e -> ()) ; 
  Arg.current := 0 ; 
  Arg.parse_argv (Array.of_list !args) 
    (Arg.align !options) 
    (fun str -> debug "%s: unknown option %s\n"  file str ; exit 1) usage_msg ;
  () 

let usage_function aligned usage_msg x = 
  parse_options_in_file x

let cil_parse fname = Frontc.parse fname () 

let output_cil_file fname cfile = 
  let fout = open_out fname in
  let old_directive_style = !Cil.lineDirectiveStyle in
    Cil.lineDirectiveStyle := None ; 
    iterGlobals cfile (dumpGlobal defaultCilPrinter fout);
    Cil.lineDirectiveStyle := old_directive_style;
    close_out fout

