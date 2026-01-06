open Hardcaml
open Signal

let width = Solution.int_size
let fifo_depth = 5000

(* Circuit for division with remainder. Algorithm is essentially long division.
Can be sped up by adjusting next_digit_multiplier based on widths of divident and divisor, but I had problems with Signal.leading_zeros so decided to go digit by digit. *)
module Divider = struct
  module I = struct
    type 'a t =
      { clock : 'a
      ; clear : 'a
      ; divident : 'a [@bits width]
      ; divisor : 'a [@bits width]
      ; start : 'a
      }
    [@@deriving hardcaml]
  end

  module O = struct
    type 'a t =
      { done_ : 'a
      ; quotient : 'a [@bits width]
      ; remainder : 'a [@bits width]
      }
    [@@deriving hardcaml]
  end

  module States = struct
    type t =
      | Wait_for_start
      | Running
      | Done
    [@@deriving sexp_of, compare ~localize, enumerate]
  end

  (* Straitfoward algorithm. The only difficult part is border cases. Not sure of a way to sped up aside from imrovements to division algorithm. *)
  let create
    ({ clock; clear; divident = divident_in; divisor = divisor_in; start } : _ I.t)
    =
    let spec = Reg_spec.create ~clock ~clear () in
    let open Always in
    let sm = State_machine.create (module States) spec in
    let done_ = Variable.wire ~default:gnd () in
    let quotient = Variable.reg ~width spec in
    let remainder = Variable.reg ~width spec in
    let divident = Variable.reg ~width spec in
    let divisor = Variable.reg ~width spec in
    let next_digit_multiplier = Variable.reg ~width spec in
    let negative_flag = Variable.reg ~width:1 spec in
    compile
      [ sm.switch
          [ ( Wait_for_start
            , [ when_
                  start
                  [ quotient <--. 0
                  ; divisor <-- divisor_in
                  ; next_digit_multiplier <--. Base.Int.pow 2 16
                  ; if_
                      (divident_in <+. 0)
                      [ divident <-- negate divident_in
                      ; negative_flag <--. 1
                      ; sm.set_next Running
                      ]
                      [ divident <-- divident_in
                      ; negative_flag <--. 0
                      ; sm.set_next Running
                      ]
                  ]
              ] )
          ; ( Running
            , [ if_
                  (divident.value >=+ divisor.value)
                  [ if_
                      (divident.value
                       >=+ sresize ~width (divisor.value *: next_digit_multiplier.value))
                      [ divident
                        <-- divident.value
                            -: sresize
                                 ~width
                                 (divisor.value *: next_digit_multiplier.value)
                      ; quotient <-- quotient.value +: next_digit_multiplier.value
                      ]
                      [ next_digit_multiplier <-- srl ~by:1 next_digit_multiplier.value ]
                  ]
                  [ if_
                      negative_flag.value
                      [ if_
                          (divident.value ==:. 0)
                          [ remainder <--. 0; quotient <-- negate quotient.value ]
                          [ remainder <-- divisor.value -: divident.value
                          ; quotient <-- negate (quotient.value +:. 1)
                          ]
                      ]
                      [ remainder <-- divident.value ]
                  ; sm.set_next Done
                  ]
              ] )
          ; Done, [ done_ <--. 1; sm.set_next Wait_for_start ]
          ]
      ];
    { O.done_ = done_.value; quotient = quotient.value; remainder = remainder.value }
  ;;
end

module States = struct
  type t =
    | Wait_for_start
    | Running
    | Dividing
    | Done
  [@@deriving sexp_of, compare ~localize, enumerate]
end

let create ({ clock; clear; fifo_read; fifo_empty; start } : _ Solution.I.t) =
  let spec = Reg_spec.create ~clock ~clear () in
  let open Always in
  let sm = State_machine.create (module States) spec in
  let fifo_read_enable = Variable.wire ~default:gnd () in
  let divident = Variable.wire ~default:(zero width) () in
  let divider_start = Variable.wire ~default:gnd () in
  let dial = Variable.reg ~width spec in
  let part1 = Variable.reg ~width spec in
  let part2 = Variable.reg ~width spec in
  let extra = Variable.reg ~width spec in
  let done_ = Variable.wire ~default:gnd () in
  let ({ done_ = divider_done; quotient; remainder } : _ Divider.O.t) =
    Divider.create
      { Divider.I.start = divider_start.value
      ; clock
      ; clear
      ; divident = divident.value
      ; divisor = of_int_trunc ~width 100
      }
  in
  compile
    [ sm.switch
        [ ( Wait_for_start
          , [ when_
                start
                [ part1 <--. 0; part2 <--. 0; dial <--. 50; sm.set_next Running ]
            ] )
        ; ( Running
          , [ if_
                fifo_empty
                [ sm.set_next Done ]
                [ fifo_read_enable <--. 1
                ; divident <-- dial.value +: fifo_read
                ; extra <-- dial.value +: fifo_read
                ; divider_start <--. 1
                ; sm.set_next Dividing
                ]
            ] )
        ; ( Dividing
          , [ when_
                divider_done
                [ when_ (remainder ==:. 0) [ part1 <-- part1.value +:. 1 ]
                ; if_
                    (quotient <+. 0)
                    [ part2
                      <-- part2.value
                          -: quotient
                          +: mux
                               ((remainder ==:. 0) @: (dial.value ==:. 0))
                               (List.map (fun v -> of_int_trunc ~width v) [ 0; -1; 1; 0 ])
                    ]
                    [ if_
                        (dial.value <>:. 0 &: (quotient ==:. 0) &: (remainder ==:. 0))
                        [ part2 <-- part2.value +:. 1 ]
                        [ part2 <-- part2.value +: quotient ]
                    ]
                ; dial <-- remainder
                ; sm.set_next Running
                ]
            ] )
        ; Done, [ done_ <--. 1; sm.set_next Wait_for_start ]
        ]
    ];
  { Solution.O.done_ = done_.value
  ; part1 = part1.value
  ; part2 = part2.value
  ; fifo_read_enable = fifo_read_enable.value
  ; extra = extra.value
  }
;;
