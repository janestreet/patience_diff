open! Core

(** Each move identified in the code is given a unique move ID which can be used to
    distinguish it from other moves. *)
type t [@@deriving sexp, compare ~localize]

include Comparable.S_plain with type t := t

val to_string : t -> string

(** Return the 0th move index *)
val zero : t

(** Get the next move index *)
val succ : t -> t

module Stable : sig
  module V1 : sig
    type nonrec t = t [@@deriving sexp, bin_io, compare ~localize]
  end
end
