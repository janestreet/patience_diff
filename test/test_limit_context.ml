open! Core
open Import

module Test_case = struct
  type t =
    { prev : string array
    ; next : string array
    ; context : int
    }
  [@@deriving sexp_of]

  let gen =
    let open Quickcheck.Let_syntax in
    let%bind context = Int.gen_incl 1 10 in
    let%bind prev_length = Int.gen_incl 30 60 in
    let%bind next_length = Int.gen_incl 30 60 in
    let line_gen =
      Quickcheck.Generator.of_list [ "apple"; "banana"; "cherry"; "dog"; "egg"; "fish" ]
    in
    let%bind prev = Quickcheck.Generator.list_with_length prev_length line_gen in
    let%bind next = Quickcheck.Generator.list_with_length next_length line_gen in
    return { prev = List.to_array prev; next = List.to_array next; context }
  ;;
end

let%test_unit "Ensure that limiting infinite context hunk to context is correct" =
  Quickcheck.test ~sexp_of:Test_case.sexp_of_t Test_case.gen ~f:(fun test_case ->
    let hunks =
      Patience_diff.String.get_hunks
        ~transform:Fn.id
        ~context:test_case.context
        ~prev:test_case.prev
        ~next:test_case.next
        ()
    in
    let infinite_context_hunk =
      Patience_diff.String.get_hunks
        ~transform:Fn.id
        ~context:(-1)
        ~prev:test_case.prev
        ~next:test_case.next
        ()
      |> List.hd_exn
    in
    let limited_hunks =
      Patience_diff.Hunk.limit_infinite_context_hunk_to_context
        ~context:test_case.context
        infinite_context_hunk
    in
    [%test_result: string Patience_diff.Hunk.t list] limited_hunks ~expect:hunks)
;;
