open Base
open Hardcaml

let designs = [ ("echo", (module Echo : Solution.Design)) ]

module Sim = Cyclesim.With_interface (Solution.I) (Solution.O)

let create_sim (module Design : Solution.Design) = Sim.create Design.create

let clear_sim (sim : Sim.t) =
  let inputs = Cyclesim.inputs sim in
  inputs.clear := Bits.vdd;
  Cyclesim.cycle sim;
  inputs.clear := Bits.gnd

let input_sim (sim : Sim.t) input_data =
  let inputs = Cyclesim.inputs sim in
  inputs.write_enable := Bits.vdd;
  List.iter input_data ~f:(fun input ->
      inputs.write_data := Bits.of_int_trunc ~width:Solution.int_size input;
      Cyclesim.cycle sim);
  inputs.write_enable := Bits.gnd

let run_sim (sim : Sim.t) =
  let inputs = Cyclesim.inputs sim in
  let outputs = Cyclesim.outputs sim in
  inputs.start := Bits.vdd;
  Cyclesim.cycle sim;
  inputs.start := Bits.gnd;
  while not (Bits.to_bool !(outputs._done)) do
    Cyclesim.cycle sim
  done;
  Cyclesim.cycle sim;
  let part1 = Bits.to_unsigned_int !(outputs.part1) in
  let part2 = Bits.to_unsigned_int !(outputs.part2) in
  (part1, part2)

let run design_name variant =
  match
    List.find ~f:(fun (name, _) -> String.equal name design_name) designs
  with
  | None -> (0, 0)
  | Some (_, (module D)) ->
      let input_data = D.generate_input variant in
      let sim = create_sim (module D) in
      clear_sim sim;
      input_sim sim input_data;
      run_sim sim
