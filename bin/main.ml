open Base
module Parsers = Advent_of_fpga_input_parsers
module Solutions = Advent_of_fpga
module Bench = Solutions.Bench.With_interface

let generate_run (module Design : Solutions.Solution.Design) =
  let module B = Bench (Design) in
  B.run
;;

exception Key_not_found

let get collection key =
  match List.find ~f:(fun (name, _) -> String.equal name key) collection with
  | None -> raise Key_not_found
  | Some (_, value) -> value
;;

let programms =
  [ "adder", (generate_run (module Solutions.Adder), [ "sample", [ 0; 2; 4 ] ])
  ; ( "day01"
    , ( generate_run (module Solutions.Day01)
      , [ "sample", Parsers.Day01.sample; "input", Parsers.Day01.input ] ) )
  ; ( "day03"
    , ( generate_run (module Solutions.Day03)
      , [ "sample", Parsers.Day03.sample; "input", Parsers.Day03.input ] ) )
  ; ( "day07"
    , ( generate_run (module Solutions.Day07)
      , [ "sample", Parsers.Day07.sample; "input", Parsers.Day07.input ] ) )
  ]
;;

exception Exit

let () =
  try
    let argv = Sys.get_argv () in
    if Array.length argv < 3
    then (
      Stdio.print_endline "Not enough arguments";
      raise Exit);
    let programm_name = argv.(1) in
    let run, inputs =
      try get programms programm_name with
      | Key_not_found ->
        Stdio.print_endline ("Didn't find programm named " ^ programm_name);
        raise Exit
    in
    let input_name = argv.(2) in
    let input =
      try get inputs input_name with
      | Key_not_found ->
        Stdio.print_endline ("Didn't find input named " ^ input_name);
        raise Exit
    in
    let print_outputs = Array.length argv > 3 in
    let part1, part2, extra = run ~print_outputs input in
    let result =
      String.concat
        [ "Part1: "
        ; Int.to_string part1
        ; "; Part2: "
        ; Int.to_string part2
        ; "; "
        ; "extra = "
        ; Int.to_string extra
        ]
    in
    Stdio.print_endline result
  with
  | Exit -> ()
;;
