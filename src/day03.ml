open Base
open Hardcaml

let int_size = Solution.int_size
let fifo_depth = 3000
let digits_count = 10

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
  List.range 0 count
  |> List.map ~f:(fun n -> nth_digit ~num_width:digits_count ~n input)
  |> List.rev
;;

let rec digits_to_number ds =
  match ds with
  | [] -> Signal.zero int_size
  | [ d ] -> d
  | hd1 :: hd2 :: tl ->
    digits_to_number
      (Signal.(sresize ~width:int_size (hd1 *: of_string "1010") +: hd2) :: tl)
;;

(* combinatorially remove 1 digit so that resulting number is maximal *)
let remove_digit ds =
  let rec remove ls n =
    match ls, n with
    | [], _ -> []
    | _ :: tl, 0 -> tl
    | hd :: tl, n -> hd :: remove tl (n - 1)
  in
  let rec transpose m =
    if List.is_empty (List.hd_exn m)
    then []
    else List.map ~f:List.hd_exn m :: transpose (List.map ~f:List.tl_exn m)
  in
  let dsa = Array.of_list ds in
  List.init (Array.length dsa) ~f:(fun n ->
    Signal.(dsa.(n) ==: zero int_size), remove ds n)
  @ List.init (Array.length dsa) ~f:(fun n ->
    ( (if n + 1 < Array.length dsa then Signal.(dsa.(n) <+ dsa.(n + 1)) else Signal.vdd)
    , remove ds n ))
  |> List.map ~f:(fun (cond, thens) -> List.map thens ~f:(fun t -> cond, t))
  |> transpose
  |> List.map ~f:if_else
;;

let rec remove_n_digits ds n =
  if n = 0 then ds else remove_n_digits (remove_digit ds) (n - 1)
;;

module States = struct
  type t =
    | Waiting_for_start
    | Running
    | Row_end
    | Done
  [@@deriving sexp_of, compare ~localize, enumerate]
end

let create ({ clock; clear; fifo_read; fifo_empty; start } : _ Solution.I.t) =
  let spec = Signal.Reg_spec.create ~clock ~clear () in
  let open Always in
  let sm = State_machine.create (module States) spec in
  let part1 = Variable.reg ~width:int_size spec in
  let part2 = Variable.reg ~width:int_size spec in
  let extra = Signal.wire int_size in
  let done_ = Variable.wire ~default:Signal.gnd () in
  let fifo_read_enable = Variable.wire ~default:Signal.gnd () in
  let collected_digits_12 =
    List.init 12 ~f:(fun _ -> Variable.reg ~width:int_size spec)
  in
  let empty_collected_digits_12 = List.init 12 ~f:(fun _ -> Signal.zero int_size) in
  let collected_digits_2 = List.init 2 ~f:(fun _ -> Variable.reg ~width:int_size spec) in
  let empty_collected_digits_2 = List.init 2 ~f:(fun _ -> Signal.zero int_size) in
  let new_digits =
    List.init digits_count ~f:(fun _ -> Variable.wire ~default:(Signal.zero int_size) ())
  in
  let list_assign left right =
    Always.proc (List.map2_exn left right ~f:(fun l r -> l <-- r))
  in
  let var_values (ls : Variable.t list) : Signal.t list =
    List.map ls ~f:(fun v -> v.value)
  in
  Signal.( <-- ) extra (digits_to_number (var_values collected_digits_12));
  compile
    [ sm.switch
        [ ( Waiting_for_start
          , [ when_
                start
                [ part1 <--. 0
                ; part2 <--. 0
                ; list_assign collected_digits_2 empty_collected_digits_2
                ; list_assign collected_digits_12 empty_collected_digits_12
                ; sm.set_next Running
                ]
            ] )
        ; ( Running
          , [ if_
                fifo_empty
                [ sm.set_next Done ]
                [ fifo_read_enable <--. 1
                ; if_
                    Signal.(fifo_read ==:. 0)
                    [ sm.set_next Row_end ]
                    [ list_assign new_digits (digits ~count:digits_count fifo_read)
                    ; list_assign
                        collected_digits_12
                        (remove_n_digits
                           (var_values (collected_digits_12 @ new_digits))
                           digits_count)
                    ; list_assign
                        collected_digits_2
                        (remove_n_digits
                           (var_values (collected_digits_2 @ new_digits))
                           digits_count)
                    ]
                ]
            ] )
        ; ( Row_end
          , [ (part1
               <-- Signal.(
                     part1.value +: digits_to_number (var_values collected_digits_2)))
            ; (part2
               <-- Signal.(
                     part2.value +: digits_to_number (var_values collected_digits_12)))
            ; list_assign collected_digits_12 empty_collected_digits_12
            ; list_assign collected_digits_2 empty_collected_digits_2
            ; sm.set_next Running
            ] )
        ; Done, [ done_ <--. 1; sm.set_next Waiting_for_start ]
        ]
    ];
  { Solution.O.done_ = done_.value
  ; part1 = part1.value
  ; part2 = part2.value
  ; fifo_read_enable = fifo_read_enable.value
  ; extra
  }
;;
