open Base

let digits_count = Advent_of_fpga.Day07.digits_count
let dial_width = Advent_of_fpga.Day07.dial_width

let rec partition l n ~acc =
  let open List in
  match l with
  | [] -> acc
  | _ -> partition (drop l n) n ~acc:(List.take l n :: acc)
;;

let pad l len filler = l @ List.init (len - List.length l) ~f:(fun _ -> filler)
let partition = partition ~acc:[]

let parse_line line =
  partition (pad (String.to_list line) dial_width '.') digits_count
  |> List.map
       ~f:
         (List.map ~f:(fun c ->
            match c with
            | '.' -> "1"
            | '^' | 'S' -> "2"
            | _ -> ""))
  |> List.map ~f:(fun l -> Int.of_string (String.concat l))
  |> List.append [ 0 ]
  |> List.rev
;;

let parse_input input = String.split_lines input |> List.map ~f:parse_line |> List.concat

let sample =
  parse_input
    ".......S.......\n\
     ...............\n\
     .......^.......\n\
     ...............\n\
     ......^.^......\n\
     ...............\n\
     .....^.^.^.....\n\
     ...............\n\
     ....^.^...^....\n\
     ...............\n\
     ...^.^...^.^...\n\
     ...............\n\
     ..^...^.....^..\n\
     ...............\n\
     .^.^.^.^.^...^.\n\
     ..............."
;;

let input = parse_input [%embed_file_as_string "input_parsers/day07.txt"]
