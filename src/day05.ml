open Base
open Hardcaml

let int_size = Solution.int_size

module States = struct
  type t =
    | Waiting_for_start
    | Reading_intervals
    | Reading_numbers
    | Done
  [@@deriving sexp_of, compare ~localize, enumerate]
end

let create ({ clock; clear; fifo_read; fifo_empty; start } : _ Solution.I.t) =
  let spec = Signal.Reg_spec.create ~clock ~clear () in
  let open Always in
  let done_ = Variable.wire ~default:Signal.gnd () in
  let fifo_read_enable = Variable.wire ~default:Signal.gnd () in
  let part1 = Variable.reg ~width:int_size spec in
  let part2 = Variable.reg ~width:int_size spec in
  let extra = Variable.reg ~width:int_size spec in
  let sm = State_machine.create (module States) spec in
  compile
    [ sm.switch [ Waiting_for_start, [ when_ start [ sm.set_next Reading_intervals ] ] ] ];
  { Solution.O.done_ = done_.value
  ; part1 = part1.value
  ; part2 = part2.value
  ; extra = extra.value
  ; fifo_read_enable = fifo_read_enable.value
  }
;;
