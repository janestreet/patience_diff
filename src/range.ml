module Stable = struct
  open! Core.Core_stable

  module V2 = struct
    type 'a t =
      | Same of ('a * 'a) array
      | Prev of 'a array * Move_kind.Stable.V1.t option
      | Next of 'a array * Move_kind.Stable.V1.t option
      | Replace of 'a array * 'a array * Move_id.Stable.V1.t option
      | Unified of 'a array * Move_id.Stable.V1.t option
    [@@deriving sexp, bin_io, compare ~localize]
  end

  module V1 = struct
    type 'a t =
      | Same of ('a * 'a) array
      | Prev of 'a array
      | Next of 'a array
      | Replace of 'a array * 'a array
      | Unified of 'a array
    [@@deriving sexp, bin_io]

    let to_v2 : 'a t -> 'a V2.t = function
      | Same lines -> Same lines
      | Prev lines -> Prev (lines, None)
      | Next lines -> Next (lines, None)
      | Replace (lines_prev, lines_next) -> Replace (lines_prev, lines_next, None)
      | Unified lines -> Unified (lines, None)
    ;;

    let of_v2_no_moves_exn : 'a V2.t -> 'a t = function
      | Prev (_, Some _) | Next (_, Some _) | Replace (_, _, Some _) | Unified (_, Some _)
        -> Core.raise_s [%sexp "cannot convert to old patdiff version with a move"]
      | Same lines -> Same lines
      | Prev (lines, None) -> Prev lines
      | Next (lines, None) -> Next lines
      | Replace (lines_prev, lines_next, None) -> Replace (lines_prev, lines_next)
      | Unified (lines, None) -> Unified lines
    ;;
  end
end

open! Core
include Stable.V2

let all_same ranges =
  List.for_all ranges ~f:(fun range ->
    match range with
    | Same _ -> true
    | _ -> false)
;;

let prev_and_next range =
  match range with
  | Same _ -> [ range ], [ range ]
  | Prev (_, (None | Some (Move _))) -> [ range ], []
  | Prev (_, Some (Within_move _)) ->
    (* Don't include [Prev]s that are within moves because they are showing that some code
       was deleted from a different [Prev] to create a corresponding [Next]. *)
    [], []
  | Next (_, _) -> [], [ range ]
  | Replace (l_range, r_range, None) -> [ Prev (l_range, None) ], [ Next (r_range, None) ]
  | Replace (_, r_range, Some move_id) ->
    (* Don't include [Replace]s for the same reason as a [Prev] within a move. *)
    [], [ Next (r_range, Some (Within_move move_id)) ]
  | Unified (_, Some _) -> [], [ range ]
  | Unified (_, None) -> [ range ], [ range ]
;;

let prev_only ranges = List.concat_map ranges ~f:(fun range -> fst (prev_and_next range))
let next_only ranges = List.concat_map ranges ~f:(fun range -> snd (prev_and_next range))

let prev_size = function
  | Unified (lines, None)
  | Replace (lines, _, None)
  | Prev (lines, None)
  | Prev (lines, Some (Move _)) -> Array.length lines
  | Same lines -> Array.length lines
  (* Don't include [Prev]s that are within moves as they are conceptually [Next]s *)
  | Replace (_, _, Some _) | Prev (_, Some (Within_move _)) | Next _ | Unified (_, Some _)
    -> 0
;;

let next_size = function
  | Unified (lines, _) | Replace (_, lines, _) | Next (lines, _) -> Array.length lines
  | Same lines -> Array.length lines
  | Prev _ -> 0
;;
