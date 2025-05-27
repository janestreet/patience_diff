(** In diff terms, a hunk is a unit of consecutive ranges with some [Same] context before
    and after [Next], [Prev], and [Replace] ranges. Each hunk contains information about
    the original arrays, specifically the starting indexes and the number of elements in
    both arrays to which the hunk refers.

    Furthermore, a diff is essentially a list of hunks. The simplest case is a diff with
    infinite context, consisting of exactly one hunk. *)

open! Core

type 'a t =
  { prev_start : int
  ; prev_size : int
  ; next_start : int
  ; next_size : int
  ; ranges : 'a Range.t list
  }
[@@deriving fields ~getters, sexp_of, compare ~localize]

(** [all_same t] returns true if [t] contains only Same ranges. *)
val all_same : 'a t -> bool

(** [concat_map t ~f] applies [List.concat_map] on [t.ranges]. *)
val concat_map : 'a t -> f:('a Range.t -> 'b Range.t list) -> 'b t

(** Take a hunk that was generated with infinite context and limit it to the provided
    [context]. *)
val limit_infinite_context_hunk_to_context : context:int -> 'a t -> 'a t list

module Stable : sig
  module V2 : sig
    type nonrec 'a t = 'a t [@@deriving sexp, bin_io]
  end

  module V1 : sig
    type nonrec 'a t [@@deriving sexp, bin_io]

    val to_v2 : 'a t -> 'a V2.t
    val of_v2_no_moves_exn : 'a V2.t -> 'a t
  end
end
