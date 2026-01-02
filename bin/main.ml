open Base

let () =
  let args = Sys.get_argv () in
  let command = args.(1) in
  let variant = args.(2) in
  let part1, part2 = Advent_of_fpga.Build.run command variant in
  Stdio.printf "Part1: %i; Part2 %i" part1 part2
