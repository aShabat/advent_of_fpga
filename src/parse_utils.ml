open Base

let read_file filename = In_channel.with_open_bin filename In_channel.input_all

let parse_line line =
  let reg = Re.Perl.compile_pat "(R|L)(\\d+)" in
  let match_ = Re.Group.all (List.hd_exn (Re.all reg line)) in
  [ (if String.equal match_.(1) "R" then 0 else 1); Int.of_string match_.(2) ]

let parse_input input =
  String.split_lines input |> List.map ~f:parse_line |> List.concat

let parse_file filename = parse_input (read_file filename)

let day_inputs day =
  [
    ("sample", parse_file ("inputs/sample/" ^ day));
    ("exercise", parse_file ("inputs/exercise/" ^ day));
  ]
