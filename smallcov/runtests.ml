open Global

let num_pos_tests = ref 1
let num_neg_tests = ref 1
let test_script = ref "sh test.sh"
let test_command = ref ""

let _ = 
  options := !options @ 
    [ 
      "--testscript", Arg.Set_string test_script, "X test script." ;
      "--testcomm", Arg.Set_string test_command, "X test command."; 
      "--postests", Arg.Set_int num_pos_tests, "X number of positive tests.";
      "--negtests", Arg.Set_int num_neg_tests, "X number of negative tests."
    ]
type test = 
  | Positive of int 
  | Negative of int 

let test_name t = match t with
  | Positive x -> Printf.sprintf "p%d" x
  | Negative x -> Printf.sprintf "n%d" x

let internal_test_case exe_name source_name test =
  let cmd = 
    let base_command = 
      match !test_command with 
      | "" -> 
        "__TEST_SCRIPT__ __EXE_NAME__ __TEST_NAME__ "^
          "__SOURCE_NAME__ 2>/dev/null >/dev/null"
    |  x -> x
      in
      let cmd = Global.replace_in_string base_command 
        [ 
          "__TEST_SCRIPT__", !test_script ;
          "__EXE_NAME__", exe_name ;
          "__TEST_NAME__", (test_name test) ;
          "__SOURCE_NAME__", (source_name) ;
        ] 
      in 
        cmd
  in
    (* Run our single test. *) 
    let status = Stats2.time "test" Unix.system cmd in
      match status with 
      | Unix.WEXITED(0) -> true 
      | _ -> false

let run_tests coverage_outname coverage_exename coverage_sourcename out_path = 
  let internal_run_tests  test_maker max_test expected = 
    lfoldl
      (fun acc test ->
        let _ = 
          try Unix.unlink coverage_outname with _ -> ()
        in
        let cmd = Printf.sprintf "touch %s\n" coverage_outname in
        let _ = ignore(Unix.system cmd) in
        let actual_test = test_maker test in 
	debug "\t%s\n" (test_name actual_test);
        let res = 
          internal_test_case coverage_exename coverage_sourcename actual_test
        in 
          if res <> expected then 
            debug "smallCov: unexpected result on %s\n" (test_name actual_test);
          let lines = get_lines coverage_outname in 
            if (llen lines) > 0 then 
              actual_test :: acc
            else acc
          )  [] (1 -- max_test) 
  in
  let neg_touch = debug "coverage negative:\n"; 
      internal_run_tests (fun t -> Negative t) !num_neg_tests false 
  in
  let pos_touch = debug "coverage positive:\n"; 
        internal_run_tests (fun t -> Positive t) !num_pos_tests true in
  let overall_touch = neg_touch @ pos_touch in
  let fout = open_out out_path in
    liter
      (fun test ->
        let str = Printf.sprintf "%s\n" (test_name test) in
          output_string fout str) overall_touch;
    close_out fout
