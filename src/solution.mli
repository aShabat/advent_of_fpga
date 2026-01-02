open! Hardcaml

val int_size : int

module I : sig
  type 'a t = {
    clock : 'a;
    clear : 'a;
    write_data : 'a; [@bits int_size]
    write_enable : 'a;
    start : 'a;
  }
  [@@deriving hardcaml]
end

module O : sig
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
