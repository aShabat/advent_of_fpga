open Hardcaml

let generate_input _ = [ 0; 2; 4 ]

let create (i : Signal.t Solution.I.t) =
  let _done = Signal.vdd in
  let spec = Signal.Reg_spec.create ~clock:i.clock () in
  let part1 = Signal.reg spec ~enable:i.write_enable i.write_data in
  let part2 = Signal.(part1 +: part1) in
  { Solution.O._done; part1; part2 }
