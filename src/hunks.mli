open! Core

type 'a t = 'a Hunk.t list

(** [unified t] converts all Replace ranges in [t] to an Prev range followed by a Next
    range. *)
val unified : 'a t -> 'a t

(** [ranges t] concatenates all the ranges of all hunks together *)
val ranges : 'a t -> 'a Range.t list

val concat_map_ranges : 'a t -> f:('a Range.t -> 'b Range.t list) -> 'b t

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
