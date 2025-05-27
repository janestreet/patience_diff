(** If a given range is part of a move it will have a [Move_kind.t]. If the move is simple
    with no ranges there will just be two ranges: One [Prev] and one [Next] that share the
    same [Move MOVE_INDEX] where the index is used to identify a given move as the same.

    If the move has modifications like additions and deletions then the [Next] part of the
    move will have replaces with [Within_move MOVE_INDEX] to denote they are just
    modifications to the moved code. *)
type t =
  | Move of Move_id.t
  | Within_move of Move_id.t
[@@deriving sexp, compare ~localize]

module Stable : sig
  module V1 : sig
    type nonrec t = t [@@deriving sexp, bin_io, compare ~localize]
  end
end
