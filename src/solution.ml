open Hardcaml
open Base

let int_size = Sys.int_size_in_bits

module I = struct
  type 'a t =
    { clock : 'a
    ; clear : 'a
    ; fifo_read : 'a [@bits int_size]
    ; fifo_empty : 'a
    ; start : 'a
    }
  [@@deriving hardcaml]
end

module O = struct
  type 'a t =
    { done_ : 'a
    ; fifo_read_enable : 'a
    ; part1 : 'a [@bits int_size]
    ; part2 : 'a [@bits int_size]
    ; extra : 'a [@bits int_size]
    }
  [@@deriving hardcaml]
end

module type Design = sig
  val fifo_depth : int
  val create : Signal.t I.t -> Signal.t O.t
end
