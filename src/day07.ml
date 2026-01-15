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
  List.init count ~f:(fun n -> nth_digit ~num_width:digits_count ~n input) |> List.rev
;;

module State = struct
  type t =
    | Waiting_for_start
    | Reading_initial_dial
    | Reading_splitters
    | Done
  [@@deriving sexp_of, compare ~localize, enumerate]
end

let list_values (l : Always.Variable.t list) = List.map l ~f:(fun v -> v.value)

let list_assign ?(start = 0) ?len l v =
  let len =
    match len with
    | None -> List.length l - start
    | Some end_ -> end_
  in
  let _, l = List.split_n l start in
  let l, _ = List.split_n l len in
  Always.proc (List.map2_exn l v ~f:(fun lv vv -> Always.(lv <-- vv)))
;;

let list_shift_left l offset =
  let _, l_offset = List.split_n l offset in
  let l, _ = List.split_n l (List.length l - offset) in
  Always.proc (List.map2_exn l (list_values l_offset) ~f:Always.( <-- ))
;;

let dial_update dial splitters =
  let shift_left l = List.tl_exn l @ [ Signal.zero int_size ] in
  let shift_right l = List.rev (shift_left (List.rev l)) in
  let dial_left =
    shift_left
      (List.map2_exn dial splitters ~f:(fun d s -> Signal.mux2 s d (Signal.zero int_size)))
  in
  let dial_right =
    shift_right
      (List.map2_exn dial splitters ~f:(fun d s -> Signal.mux2 s d (Signal.zero int_size)))
  in
  let dial =
    List.map2_exn dial splitters ~f:(fun d s -> Signal.mux2 s (Signal.zero int_size) d)
  in
  List.map3_exn dial_left dial dial_right ~f:(fun l m r -> Signal.(l +: m +: r))
;;

let create ({ clock; clear; fifo_read; fifo_empty; start } : _ Solution.I.t) =
  let spec = Signal.Reg_spec.create ~clock ~clear () in
  let open Always in
  let sm = State_machine.create (module State) spec in
  let part1 = Variable.reg ~width:int_size spec in
  let part2 = Variable.reg ~width:int_size spec in
  let done_ = Variable.wire ~default:Signal.gnd () in
  let fifo_read_enable = Variable.wire ~default:Signal.gnd () in
  let dial = List.init dial_width ~f:(fun _ -> Variable.reg ~width:int_size spec) in
  let splitters = List.init dial_width ~f:(fun _ -> Variable.reg ~width:1 spec) in
  compile
    [ sm.switch
        [ ( Waiting_for_start
          , [ when_ start [ part1 <--. 0; part2 <--. 0; sm.set_next Reading_initial_dial ]
            ] )
        ; ( Reading_initial_dial
          , [ fifo_read_enable <--. 1
            ; if_
                Signal.(fifo_read ==:. 0)
                [ sm.set_next Reading_splitters ]
                [ list_shift_left dial digits_count
                ; list_assign
                    ~start:(dial_width - digits_count)
                    dial
                    (List.map (digits ~count:digits_count fifo_read) ~f:(fun v ->
                       Signal.(mux2 (v ==:. 2) (one int_size) (zero int_size))))
                ]
            ] )
        ; ( Reading_splitters
          , [ fifo_read_enable <--. 1
            ; if_
                fifo_empty
                [ part2 <-- List.reduce_exn (list_values dial) ~f:Signal.( +: )
                ; sm.set_next Done
                ]
                [ if_
                    Signal.(fifo_read ==:. 0)
                    [ list_assign
                        dial
                        (dial_update (list_values dial) (list_values splitters))
                    ; part1
                      <-- List.fold
                            (List.map2_exn
                               (list_values dial)
                               (list_values splitters)
                               ~f:(fun d s ->
                                 Signal.(uresize ~width:int_size (d >+. 0 &: s))))
                            ~init:part1.value
                            ~f:Signal.( +: )
                    ]
                    [ list_shift_left splitters digits_count
                    ; list_assign
                        splitters
                        ~start:(dial_width - digits_count)
                        (List.map (digits ~count:digits_count fifo_read) ~f:(fun v ->
                           Signal.(v ==:. 2)))
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
