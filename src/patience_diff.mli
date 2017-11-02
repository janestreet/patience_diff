(** This is a port of Bram Cohen's patience diff algorithm, as found in the Bazaar 1.14.1
    source code, available at http://bazaar-vcs.org.

    This copyright notice was included:

    # Copyright (C) 2005 Bram Cohen, Copyright (C) 2005, 2006 Canonical Ltd
    #
    # This program is free software; you can redistribute it and/or modify
    # it under the terms of the GNU General Public License as published by
    # the Free Software Foundation; either version 2 of the License, or
    # (at your option) any later version.
    #
    # This program is distributed in the hope that it will be useful,
    # but WITHOUT ANY WARRANTY; without even the implied warranty of
    # MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    # GNU General Public License for more details.
    #
    # You should have received a copy of the GNU General Public License
    # along with this program; if not, write to the Free Software
    # Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA
*)


(** Bram Cohen's comment from the original Python code (with syntax changed to OCaml):

    [get_matching_blocks a b] returns a list of triples describing matching
    subsequences.

    Each triple is of the form (i, j, n), and means that
    a <|> (i,i+n) = b <|> (j,j+n).  The triples are monotonically increasing in
    i and in j.

    The last triple is a dummy, (Array.length a, Array.length b, 0), and is the only
    triple with n=0.

    Example:
    get_matching_blocks [|"a";"b";"x";"c";"d"|] [|"a";"b";"c";"d"|]
    returns
    [(0, 0, 2), (3, 2, 2), (5, 4, 0)]
*)

open Core_kernel

module Matching_block : sig
  type t =
    { mine_start  : int
    ; other_start : int
    ; length      : int
    }
end

(** For handling diffs abstractly.  A range is a subarray of the two original arrays with
    a constructor defining its relationship to the two original arrays.  A [Same] range
    contains a series of elements which can be found in both arrays.  A [New] range
    contains elements found only in the new array, while an [Old] range contains elements
    found only in the old array.

    A [Replace] contains two arrays: elements in the first array are elements found only
    in the original, old array which have been replaced by elements in the second array,
    which are elements found only in the new array. *)

module Range : sig
  type 'a t =
    | Same of ('a * 'a) array
    | Old of 'a array
    | New of 'a array
    | Replace of 'a array * 'a array
    | Unified of 'a array
  [@@deriving sexp]

  (** [all_same ranges] returns true if all [ranges] are Same *)
  val all_same : 'a t list -> bool

  (** [old_only ranges] drops all New ranges and converts all Replace ranges to Old
      ranges. *)
  val old_only : 'a t list -> 'a t list

  (** [new_only ranges] drops all Old ranges and converts all Replace ranges to New
      ranges. *)
  val new_only : 'a t list -> 'a t list

  (** Counts number of elements. *)
  val mine_size : 'a t -> int
  val other_size : 'a t -> int
end

(** In diff terms, a hunk is a unit of consecutive ranges with some [Same] context before
    and after [New], [Old], and [Replace] ranges.  Each hunk contains information about
    the original arrays, specifically the starting indexes and the number of elements in
    both arrays to which the hunk refers.

    Furthermore, a diff is essentially a list of hunks.  The simplest case is a diff with
    infinite context, consisting of exactly one hunk. *)
module Hunk : sig
  type 'a t =
    { mine_start  : int
    ; mine_size   : int
    ; other_start : int
    ; other_size  : int
    ; ranges      : 'a Range.t list
    }
  [@@deriving fields, sexp_of]

  (** [all_same t] returns true if [t] contains only Same ranges. *)
  val all_same : 'a t -> bool
end

module Hunks : sig
  type 'a t = 'a Hunk.t list

  (** [unified t] converts all Replace ranges in [t] to an Old range followed by a New
      range. *)
  val unified : 'a t -> 'a t

  (** [ranges t] concatenates all the ranges of all hunks together **)
  val ranges : 'a t -> 'a Range.t list

  val concat_map_ranges : 'a t -> f : ('a Range.t -> 'b Range.t list) -> 'b t
end

module type S = sig

  type elt

  (** Get_matching_blocks not only aggregates the data from [matches a b] but also
      attempts to remove random, semantically meaningless matches ("semantic cleanup").
      The value of [big_enough] governs how aggressively we do so.  See [get_hunks]
      below for more details. *)
  val get_matching_blocks
    :  transform: ('a -> elt)
    -> ?big_enough: int
    -> mine:'a array
    -> other:'a array
    -> Matching_block.t list

  (** [matches a b] returns a list of pairs (i,j) such that a.(i) = b.(j) and such that
      the list is strictly increasing in both its first and second coordinates.  This is
      essentially a "unfolded" version of what [get_matching_blocks] returns. Instead of
      grouping the consecutive matching block using [length] this function would return
      all the pairs (mine_start * other_start). *)
  val matches : elt array -> elt array -> (int * int) list

  (** [match_ratio ~compare a b] computes the ratio defined as:

      {[
        2 * len (matches a b) / (len a + len b)
      ]}

      It is an indication of how much alike a and b are.  A ratio closer to 1.0 will
      indicate a number of matches close to the number of elements that can potentially
      match, thus is a sign that a and b are very much alike.  On the other hand, a low
      ratio means very little match. *)
  val match_ratio : elt array -> elt array -> float

  (** [get_hunks ~transform ~context ~mine ~other] will compare the arrays [mine] and
      [other] and produce a list of hunks. (The hunks will contain Same ranges of at most
      [context] elements.)  [context] defaults to infinity (producing a singleton hunk
      list).  The value of [big_enough] governs how aggressively we try to clean up
      spurious matches, by restricting our attention to only matches of length
      less than [big_enough].  Thus, setting [big_enough] to a higher value results in
      more aggressive cleanup, and the default value of 1 results in no cleanup at all.
      When this function is called by [Patdiff_core], the value of [big_enough] is 3 at
      the line level, and 7 at the word level. *)
  val get_hunks
    :  transform: ('a -> elt)
    -> context: int
    -> ?big_enough: int
    -> mine: 'a array
    -> other: 'a array
    -> 'a Hunk.t list

  type 'a segment =
    | Same of 'a array
    | Different of 'a array array

  type 'a merged_array = 'a segment list

  val merge : elt array array -> elt merged_array
end

module Make(Elt : Hashtbl.Key) : S with type elt = Elt.t

(* [String] uses String.compare *)
module String : S with type elt = string
