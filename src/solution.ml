open! Hardcaml

let int_size = 64

module I = struct
  type 'a t = {
    clock : 'a;
    clear : 'a;
    write_data : 'a; [@bits int_size]
    write_enable : 'a;
    start : 'a;
  }
  [@@deriving hardcaml]
end

module O = struct
  type 'a t = {
    _done : 'a;
    part1 : 'a; [@bits int_size]
    part2 : 'a; [@bits int_size]
  }
  [@@deriving hardcaml]
end

module type Design = sig
  val inputs : (string * int list) list
  val create : Signal.t I.t -> Signal.t O.t
end
