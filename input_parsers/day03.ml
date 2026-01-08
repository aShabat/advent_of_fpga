open Base

let digits_count = Advent_of_fpga.Day03.digits_count

let rec partition l n ~acc =
  let open List in
  match l with
  | [] -> acc
  | _ -> partition (drop l n) n ~acc:(List.take l n :: acc)
;;

let partition = partition ~acc:[]

let parse_line line =
  partition (String.to_list line) digits_count
  |> List.map ~f:(fun w -> Int.of_string (String.of_char_list w))
  |> List.append [ 0 ]
  |> List.rev
;;

let parse_input input = String.split_lines input |> List.map ~f:parse_line |> List.concat

let sample =
  parse_input "987654321111111\n811111111111119\n234234234234278\n818181911112111"
;;

let input = parse_input [%embed_file_as_string "input_parsers/day03.txt"]
