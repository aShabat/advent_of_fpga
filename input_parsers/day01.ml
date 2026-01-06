open Base

let parse_line line =
  let open Re in
  let regexp = compile (Perl.re "(R|L)(\\d+)") in
  let groups = List.hd_exn (Re.all regexp line) in
  let letter = Group.get groups 1 in
  let num = Group.get groups 2 in
  [ (if String.equal letter "R" then 1 else -1) * Int.of_string num ]
;;

let parse_input input = String.split_lines input |> List.map ~f:parse_line |> List.concat
let sample = parse_input "L68\nL30\nR48\nL5\nR60\nL55\nL1\nL99\nR14\nL82"
let input = parse_input [%embed_file_as_string "input_parsers/day01.txt"]
