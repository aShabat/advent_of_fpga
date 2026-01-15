open Base

let parse_line line =
  let open Re in
  let regexp = compile (Perl.re "(\\d+)") in
  let out = Re.all regexp line |> List.map ~f:(fun g -> Int.of_string (Group.get g 1)) in
  if List.length out = 0 then [ 0 ] else out
;;

let parse_input input = String.split_lines input |> List.map ~f:parse_line |> List.concat
let sample = parse_input "3-5\n10-14\n16-20\n12-18\n\n1\n5\n8\n11\n17\n32"
let input = parse_input [%embed_file_as_string "input_parsers/day05.txt"]
