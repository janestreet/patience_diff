module Stable = struct
  open! Core.Core_stable
  module Range = Range.Stable

  module V2 = struct
    type 'a t =
      { prev_start : int
      ; prev_size : int
      ; next_start : int
      ; next_size : int
      ; ranges : 'a Range.V2.t list
      }
    [@@deriving fields ~getters, sexp, bin_io]
  end

  module V1 = struct
    type 'a t =
      { prev_start : int
      ; prev_size : int
      ; next_start : int
      ; next_size : int
      ; ranges : 'a Range.V1.t list
      }
    [@@deriving fields ~getters, sexp, bin_io]

    let to_v2 t =
      { V2.prev_start = t.prev_start
      ; prev_size = t.prev_size
      ; next_start = t.next_start
      ; next_size = t.next_size
      ; ranges = Core.List.map t.ranges ~f:Range.V1.to_v2
      }
    ;;

    let of_v2_no_moves_exn (t : _ V2.t) =
      { prev_start = t.prev_start
      ; prev_size = t.prev_size
      ; next_start = t.next_start
      ; next_size = t.next_size
      ; ranges = Core.List.map t.ranges ~f:Range.V1.of_v2_no_moves_exn
      }
    ;;
  end
end

open! Core
include Stable.V2

let _invariant t =
  Invariant.invariant [%here] t [%sexp_of: _ t] (fun () ->
    [%test_result: int]
      (List.sum (module Int) t.ranges ~f:Range.prev_size)
      ~expect:t.prev_size
      ~message:"prev_size";
    [%test_result: int]
      (List.sum (module Int) t.ranges ~f:Range.next_size)
      ~expect:t.next_size
      ~message:"next_size")
;;

let all_same hunk = Range.all_same hunk.ranges
let concat_map t ~f = { t with ranges = List.concat_map t.ranges ~f }
