open Base
open Hardcaml

let fifo_depth = 10000
let int_size = Solution.int_size
let digits_count = 10
let dial_width = 155

let rec if_else cases =
  match cases with
  | [] -> Signal.zero int_size
  | (cond, hd) :: tl -> Signal.mux2 cond hd (if_else tl)
;;

let highest_digit num ~num_width =
  let open Signal in
  let cases =
    List.range 0 10
    |> List.map ~f:(fun d ->
      ( num <: of_int_trunc ~width:int_size ((d + 1) * Int.pow 10 num_width)
      , of_int_trunc ~width:int_size d ))
  in
  if_else cases
;;

let rec nth_digit num ~num_width ~n =
  if n = num_width
  then highest_digit num ~num_width
  else
    nth_digit
      ~num_width:(num_width - 1)
      ~n
      Signal.(
        num
        -: sel_bottom
             ~width:int_size
             (highest_digit num ~num_width
              *: of_int_trunc ~width:int_size (Int.pow 10 num_width)))
;;

(* separate signal into 10-based digits at combinatorial speed *)
let digits ~count input =
  Array.init count ~f:(fun n -> nth_digit ~num_width:digits_count ~n input) |> Array.rev
;;

module State = struct
  type t =
    | Waiting_for_start
    | Reading_initial_dial
    | Reading_splitters
    | Done
  [@@deriving sexp_of, compare ~localize, enumerate]
end

let array_values (l : Always.Variable.t array) = Array.map l ~f:(fun v -> v.value)

let array_assign l v =
  Always.proc
    (List.of_array (Array.map2_exn l v ~f:(fun left value -> Always.(left <-- value))))
;;

let array_shift a offset =
  Always.proc
    (List.of_array
       (Array.init
          (Array.length a - offset - 1)
          ~f:(fun i -> Always.(a.(i) <-- a.(i + offset).value))))
;;

let dial_update dial splitters i =
  let nothing = Signal.zero int_size in
  if i = 0 || i = dial_width - 1
  then nothing
  else
    Signal.(
      mux2 splitters.(i - 1) dial.(i - 1) nothing
      +: mux2 splitters.(i) nothing dial.(i)
      +: mux2 splitters.(i + 1) dial.(i + 1) nothing)
;;

let create ({ clock; clear; fifo_read; fifo_empty; start } : _ Solution.I.t) =
  let spec = Signal.Reg_spec.create ~clock ~clear () in
  let open Always in
  let sm = State_machine.create (module State) spec in
  let part1 = Variable.reg ~width:int_size spec in
  let part2 = Variable.reg ~width:int_size spec in
  let done_ = Variable.wire ~default:Signal.gnd () in
  let fifo_read_enable = Variable.wire ~default:Signal.gnd () in
  let dial = Array.init dial_width ~f:(fun _ -> Variable.reg ~width:int_size spec) in
  let splitters = Array.init dial_width ~f:(fun _ -> Variable.reg ~width:1 spec) in
  let tl_for_new l = Array.sub l ~pos:(dial_width - digits_count) ~len:digits_count in
  compile
    [ sm.switch
        [ ( Waiting_for_start
          , [ when_ start [ part1 <--. 0; sm.set_next Reading_initial_dial ] ] )
        ; ( Reading_initial_dial
          , [ fifo_read_enable <--. 1
            ; if_
                Signal.(fifo_read ==:. 0)
                [ sm.set_next Reading_splitters ]
                [ array_shift dial digits_count
                ; array_assign
                    (tl_for_new dial)
                    (Array.map (digits ~count:digits_count fifo_read) ~f:(fun v ->
                       Signal.(mux2 (v ==:. 2) (one int_size) (zero int_size))))
                ]
            ] )
        ; ( Reading_splitters
          , [ fifo_read_enable <--. 1
            ; if_
                fifo_empty
                [ part2
                  <-- Array.fold dial ~init:(Signal.zero int_size) ~f:(fun acc d ->
                    Signal.(acc +: d.value))
                ; sm.set_next Done
                ]
                [ if_
                    Signal.(fifo_read ==:. 0)
                    [ array_assign
                        dial
                        (Array.init
                           dial_width
                           ~f:(dial_update (array_values dial) (array_values splitters)))
                    ]
                    [ array_shift splitters digits_count
                    ; array_assign
                        (tl_for_new splitters)
                        (Array.map (digits ~count:digits_count fifo_read) ~f:(fun v ->
                           Signal.(v ==:. 2)))
                    ; part1
                      <-- List.fold
                            (List.range 0 dial_width)
                            ~init:part1.value
                            ~f:(fun acc i ->
                              Signal.(
                                acc
                                +: mux2
                                     (splitters.(i).value &: negate (dial.(i).value ==:. 0)
                                     )
                                     (one int_size)
                                     (zero int_size)))
                    ]
                ]
            ] )
        ; Done, [ done_ <--. 1 ]
        ]
    ];
  { Solution.O.done_ = done_.value
  ; part1 = part1.value
  ; part2 = part2.value
  ; extra = Signal.zero int_size
  ; fifo_read_enable = fifo_read_enable.value
  }
;;
