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
    [@@deriving fields ~getters, sexp, bin_io, compare ~localize]
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
  Invariant.invariant t [%sexp_of: _ t] (fun () ->
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

let limit_infinite_context_hunk_to_context ~context hunk =
  let trim_range range =
    match range with
    | Range.Same arr ->
      let len = Array.length arr in
      if len <= context
      then `Did_not_trim range
      else (
        let start = Range.Same (Array.sub arr ~pos:0 ~len:context) in
        let end_ = Range.Same (Array.sub arr ~pos:(len - context) ~len:context) in
        let lines_trimmed = len - (2 * context) in
        `Trimmed (lines_trimmed, start, end_))
    | _ -> `Did_not_trim range
  in
  let working_ranges = Queue.create () in
  let working_ranges_offset_from_previous = ref 0 in
  let all_hunks = Queue.create () in
  let finish_hunk ~offset_from_previous_end =
    Queue.enqueue
      all_hunks
      (!working_ranges_offset_from_previous, Queue.to_list working_ranges);
    Queue.clear working_ranges;
    working_ranges_offset_from_previous := offset_from_previous_end
  in
  let last_range_index = List.length hunk.ranges - 1 in
  List.iteri hunk.ranges ~f:(fun i range ->
    match trim_range range with
    | `Did_not_trim range ->
      Queue.enqueue working_ranges range;
      if i = last_range_index then finish_hunk ~offset_from_previous_end:0
    | `Trimmed (lines_trimmed, start_range, end_range) ->
      if i = 0
      then (
        working_ranges_offset_from_previous := lines_trimmed + Range.prev_size start_range;
        Queue.enqueue working_ranges end_range)
      else if i = last_range_index
      then (
        Queue.enqueue working_ranges start_range;
        finish_hunk ~offset_from_previous_end:0)
      else if lines_trimmed > 0
      then (
        Queue.enqueue working_ranges start_range;
        finish_hunk ~offset_from_previous_end:lines_trimmed;
        Queue.enqueue working_ranges end_range)
      else Queue.enqueue working_ranges range);
  let prev_end = ref 1 in
  let next_end = ref 1 in
  Queue.to_list all_hunks
  |> List.filter_map ~f:(fun (offset_from_previous, ranges) ->
    if List.is_empty ranges
    then None
    else (
      let hunk =
        { ranges
        ; prev_start = !prev_end + offset_from_previous
        ; prev_size = List.sum (module Int) ranges ~f:Range.prev_size
        ; next_start = !next_end + offset_from_previous
        ; next_size = List.sum (module Int) ranges ~f:Range.next_size
        }
      in
      prev_end := hunk.prev_size + hunk.prev_start;
      next_end := hunk.next_size + hunk.next_start;
      Some hunk))
;;
