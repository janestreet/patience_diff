module Stable = struct
  open! Core.Core_stable
  module Hunk = Hunk.Stable

  module V2 = struct
    type 'a t = 'a Hunk.V2.t list [@@deriving sexp, bin_io]
  end

  module V1 = struct
    type 'a t = 'a Hunk.V1.t list [@@deriving sexp, bin_io]

    let to_v2 = Core.List.map ~f:Hunk.V1.to_v2
    let of_v2_no_moves_exn = Core.List.map ~f:Hunk.V1.of_v2_no_moves_exn
  end
end

open! Core
include Stable.V2

let concat_map_ranges hunks ~f = List.map hunks ~f:(Hunk.concat_map ~f)

let unified hunks =
  let f : 'a Range.t -> 'a Range.t list = function
    | Replace (l_range, r_range, move_id) ->
      let move_kind =
        Option.map move_id ~f:(fun move_id -> Move_kind.Within_move move_id)
      in
      [ Prev (l_range, move_kind); Next (r_range, move_kind) ]
    | range -> [ range ]
  in
  concat_map_ranges hunks ~f
;;

let ranges hunks = List.concat_map hunks ~f:(fun hunk -> hunk.Hunk.ranges)
