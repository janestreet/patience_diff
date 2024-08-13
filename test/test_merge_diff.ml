open! Base
open Import

let merge_and_print arrs =
  Patience_diff.String.merge arrs
  |> List.iter ~f:(function
    | Same arr ->
      print_endline "---- Same -----";
      Array.iter arr ~f:(fun line -> print_endline ("   " ^ line))
    | Different arrs ->
      print_endline "-- Different --";
      Array.iteri arrs ~f:(fun idx ->
        Array.iter ~f:(fun line -> print_endline ("(" ^ Int.to_string idx ^ ")" ^ line))))
;;

let%expect_test "Identical documents" =
  let doc = [| "a"; "b"; "c"; "d" |] in
  merge_and_print [| doc |];
  [%expect
    {|
    ---- Same -----
       a
       b
       c
       d
    |}];
  merge_and_print [| doc; doc |];
  [%expect
    {|
    ---- Same -----
       a
       b
       c
       d
    |}];
  merge_and_print [| doc; doc; doc |];
  [%expect
    {|
    ---- Same -----
       a
       b
       c
       d
    |}]
;;

let%expect_test "Empty documents" =
  let doc = [| "a"; "b"; "c" |] in
  merge_and_print [| doc; [||] |];
  [%expect
    {|
    -- Different --
    (0)a
    (0)b
    (0)c
    |}];
  merge_and_print [| [||]; doc |];
  [%expect
    {|
    -- Different --
    (1)a
    (1)b
    (1)c
    |}];
  let doc = [| "a"; "b"; "c" |] in
  merge_and_print [| doc; [| "" |] |];
  [%expect
    {|
    -- Different --
    (0)a
    (0)b
    (0)c
    (1)
    |}];
  merge_and_print [| [| "" |]; doc |];
  [%expect
    {|
    -- Different --
    (0)
    (1)a
    (1)b
    (1)c
    |}]
;;

let%expect_test "Documents with trailing added lines" =
  let short = [| "a"; "b" |] in
  let long = [| "a"; "b"; "c"; "d" |] in
  merge_and_print [| short; long |];
  [%expect
    {|
    ---- Same -----
       a
       b
    -- Different --
    (1)c
    (1)d
    |}];
  merge_and_print [| long; short |];
  [%expect
    {|
    ---- Same -----
       a
       b
    -- Different --
    (0)c
    (0)d
    |}]
;;

let%expect_test "Documents with leading added lines" =
  let short = [| "c"; "d" |] in
  let long = [| "a"; "b"; "c"; "d" |] in
  merge_and_print [| short; long |];
  [%expect
    {|
    -- Different --
    (1)a
    (1)b
    ---- Same -----
       c
       d
    |}];
  merge_and_print [| long; short |];
  [%expect
    {|
    -- Different --
    (0)a
    (0)b
    ---- Same -----
       c
       d
    |}]
;;

let%expect_test "Mixed documents with changes" =
  let short = [| "a"; "b"; "foo"; "c"; "d" |] in
  let long = [| "a"; "b"; "c"; "bar"; "d" |] in
  merge_and_print [| short; long |];
  [%expect
    {|
    ---- Same -----
       a
       b
    -- Different --
    (0)foo
    ---- Same -----
       c
    -- Different --
    (1)bar
    ---- Same -----
       d
    |}]
;;
