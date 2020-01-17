open! Base
open Combinat
open Shared

let%expect_test "combinations" =
  Combination.iter { k = 3; n = 5 } ~f:(fun c ->
      Stdio.print_endline (Sexp.to_string_hum ([%sexp_of: int array] (to_array c))));
  [%expect
    {|
    (0 1 2)
    (0 1 3)
    (0 2 3)
    (1 2 3)
    (0 1 4)
    (0 2 4)
    (1 2 4)
    (0 3 4)
    (1 3 4)
    (2 3 4) |}]
