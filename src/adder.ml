open Hardcaml
open Signal

let width = Solution.int_size
let fifo_depth = 10

module State = struct
  type t =
    | Wait_for_start
    | Running
    | Done
  [@@deriving sexp_of, compare ~localize, enumerate]
end

let create ({ clock; clear; fifo_read; fifo_empty; start } : _ Solution.I.t) =
  let spec = Reg_spec.create ~clock ~clear () in
  let sm = Always.State_machine.create (module State) ~enable:vdd spec in
  let done_ = Always.Variable.wire ~default:gnd () in
  let part1 = Always.Variable.reg ~width spec in
  let part2 = Always.Variable.reg ~width spec in
  let fifo_read_enable = Always.Variable.wire ~default:gnd () in
  Always.(
    compile
      [ sm.switch
          [ ( Wait_for_start
            , [ when_
                  start
                  [ part1 <-- zero width; part2 <-- zero width; sm.set_next Running ]
              ] )
          ; ( Running
            , [ if_
                  fifo_empty
                  [ fifo_read_enable <--. 0; sm.set_next Done ]
                  [ fifo_read_enable <--. 1
                  ; part1 <-- part1.value +:. 1
                  ; part2 <-- part2.value +: fifo_read
                  ]
              ] )
          ; Done, [ done_ <-- vdd; sm.set_next Wait_for_start ]
          ]
      ]);
  { Solution.O.done_ = done_.value
  ; part1 = part1.value
  ; part2 = part2.value
  ; fifo_read_enable = fifo_read_enable.value
  ; extra = part2.value
  }
;;
