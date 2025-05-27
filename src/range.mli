(** For handling diffs abstractly. A range is a subarray of the two original arrays with a
    constructor defining its relationship to the two original arrays. A [Same] range
    contains a series of elements which can be found in both arrays. A [Next] range
    contains elements found only in the second array, while an [Prev] range contains
    elements found only in the first array.

    If a range is part of a move it will have a non-None [Move_kind.t] or [Move_id.t] in
    the case of [Replace] and [Unified]. A [Prev] with a [Move _] [Move_kind] means that
    [Prev] has a corresponding [Next] that it was moved to. A [Prev] with a
    [Within_move _] [Move_kind] means that this was some code that was deleted within a
    block that moved to a [Next] position of the file. If a [Replace] or [Unified] range
    is associated with a move it can only be change within a move so they only hove a
    [Move_id.t option] instead of a [Move_kind.t option] like [Prev] or [Next].

    A [Replace] contains two arrays: elements in the first output array are elements found
    only in the first input array, which have been replaced by elements in the second
    output array, which are elements found only in the second input array. *)

type 'a t =
  | Same of ('a * 'a) array
  | Prev of 'a array * Move_kind.t option
  | Next of 'a array * Move_kind.t option
  | Replace of 'a array * 'a array * Move_id.t option
  | Unified of 'a array * Move_id.t option
[@@deriving sexp, compare ~localize]

(** [all_same ranges] returns true if all [ranges] are Same *)
val all_same : 'a t list -> bool

(** [prev_only ranges] drops all Next ranges and converts all Replace ranges to Prev
    ranges. *)
val prev_only : 'a t list -> 'a t list

(** [next_only ranges] drops all Prev ranges and converts all Replace ranges to Next
    ranges. *)
val next_only : 'a t list -> 'a t list

(** Counts number of elements. *)
val prev_size : 'a t -> int

val next_size : 'a t -> int

module Stable : sig
  module V2 : sig
    type nonrec 'a t = 'a t [@@deriving sexp, bin_io, compare ~localize]
  end

  module V1 : sig
    type nonrec 'a t [@@deriving sexp, bin_io]

    val to_v2 : 'a t -> 'a V2.t
    val of_v2_no_moves_exn : 'a V2.t -> 'a t
  end
end
