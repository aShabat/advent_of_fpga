open Base
open Hardcaml

let int_size = Solution.int_size

module WithFIFO (Design : Solution.Design) = struct
  module I = struct
    type 'a t =
      { clock : 'a
      ; clear : 'a
      ; fifo_write : 'a [@bits int_size]
      ; fifo_write_enable : 'a
      ; start : 'a
      }
    [@@deriving hardcaml]
  end

  module O = struct
    type 'a t =
      { done_ : 'a
      ; part1 : 'a [@bits int_size]
      ; part2 : 'a [@bits int_size]
      ; extra : 'a [@bits int_size]
      }
    [@@deriving hardcaml]
  end

  let create ({ clock; clear; fifo_write; fifo_write_enable; start } : _ I.t) =
    let fifo_read_enable_wire = Signal.wire 1 in
    let ({ q = fifo_read; empty = fifo_empty; _ } : Signal.t Fifo.t) =
      Fifo.create
        ~showahead:true
        ~capacity:Design.fifo_depth
        ~clock
        ~clear
        ~d:fifo_write
        ~wr:fifo_write_enable
        ~rd:fifo_read_enable_wire
        ()
    in
    let ({ done_; fifo_read_enable; part1; part2; extra } : _ Solution.O.t) =
      Design.create ({ clock; clear; fifo_read; fifo_empty; start } : _ Solution.I.t)
    in
    Signal.(fifo_read_enable_wire <-- fifo_read_enable);
    { O.done_; part1; part2; extra }
  ;;
end

module With_interface (Design : Solution.Design) = struct
  module Top = WithFIFO (Design)
  module Sim = Cyclesim.With_interface (Top.I) (Top.O)

  let ( <--. ) (input : Bits.t ref) (value : int) =
    input := Bits.of_int_trunc ~width:(Bits.width !input) value
  ;;

  let clear_core (sim : Sim.t) =
    let inputs = Cyclesim.inputs sim in
    inputs.clear <--. 1;
    Cyclesim.cycle sim;
    inputs.clear <--. 0;
    ()
  ;;

  let load_inputs (sim : Sim.t) (values : int list) =
    let inputs = Cyclesim.inputs sim in
    inputs.fifo_write_enable <--. 1;
    List.iter values ~f:(fun value ->
      inputs.fifo_write <--. value;
      Cyclesim.cycle sim);
    inputs.fifo_write_enable <--. 0
  ;;

  let run_core ?(print_outputs = false) (sim : Sim.t) =
    let inputs = Cyclesim.inputs sim in
    let outputs = Cyclesim.outputs sim in
    inputs.start <--. 1;
    Cyclesim.cycle sim;
    inputs.start <--. 0;
    while not (Bits.to_bool !(outputs.done_)) do
      Cyclesim.cycle sim;
      if print_outputs
      then (
        Cyclesim.out_ports sim
        |> List.iter ~f:(fun (name, bit) ->
          Stdio.print_string
            (String.concat
               [ "("; name; "; "; Int.to_string (Bits.to_int_trunc !bit); ") " ]));
        Stdio.print_string "\n")
    done;
    let part1 = Bits.to_int_trunc !(outputs.part1) in
    let part2 = Bits.to_int_trunc !(outputs.part2) in
    let extra = Bits.to_int_trunc !(outputs.extra) in
    part1, part2, extra
  ;;

  let run ?(print_outputs = true) inputs =
    let sim = Sim.create Top.create in
    clear_core sim;
    load_inputs sim inputs;
    run_core ~print_outputs sim
  ;;
end
