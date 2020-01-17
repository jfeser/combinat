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

let%expect_test "combinations_list" =
  Combination.Of_list.iter
    (List.init 10 ~f:(fun i -> i), 6)
    ~f:(fun l -> [%sexp_of: int list] l |> Stdio.print_s);
  [%expect {|
    (0 1 2 3 4 5)
    (0 1 2 3 4 6)
    (0 1 2 3 5 6)
    (0 1 2 4 5 6)
    (0 1 3 4 5 6)
    (0 2 3 4 5 6)
    (1 2 3 4 5 6)
    (0 1 2 3 4 7)
    (0 1 2 3 5 7)
    (0 1 2 4 5 7)
    (0 1 3 4 5 7)
    (0 2 3 4 5 7)
    (1 2 3 4 5 7)
    (0 1 2 3 6 7)
    (0 1 2 4 6 7)
    (0 1 3 4 6 7)
    (0 2 3 4 6 7)
    (1 2 3 4 6 7)
    (0 1 2 5 6 7)
    (0 1 3 5 6 7)
    (0 2 3 5 6 7)
    (1 2 3 5 6 7)
    (0 1 4 5 6 7)
    (0 2 4 5 6 7)
    (1 2 4 5 6 7)
    (0 3 4 5 6 7)
    (1 3 4 5 6 7)
    (2 3 4 5 6 7)
    (0 1 2 3 4 8)
    (0 1 2 3 5 8)
    (0 1 2 4 5 8)
    (0 1 3 4 5 8)
    (0 2 3 4 5 8)
    (1 2 3 4 5 8)
    (0 1 2 3 6 8)
    (0 1 2 4 6 8)
    (0 1 3 4 6 8)
    (0 2 3 4 6 8)
    (1 2 3 4 6 8)
    (0 1 2 5 6 8)
    (0 1 3 5 6 8)
    (0 2 3 5 6 8)
    (1 2 3 5 6 8)
    (0 1 4 5 6 8)
    (0 2 4 5 6 8)
    (1 2 4 5 6 8)
    (0 3 4 5 6 8)
    (1 3 4 5 6 8)
    (2 3 4 5 6 8)
    (0 1 2 3 7 8)
    (0 1 2 4 7 8)
    (0 1 3 4 7 8)
    (0 2 3 4 7 8)
    (1 2 3 4 7 8)
    (0 1 2 5 7 8)
    (0 1 3 5 7 8)
    (0 2 3 5 7 8)
    (1 2 3 5 7 8)
    (0 1 4 5 7 8)
    (0 2 4 5 7 8)
    (1 2 4 5 7 8)
    (0 3 4 5 7 8)
    (1 3 4 5 7 8)
    (2 3 4 5 7 8)
    (0 1 2 6 7 8)
    (0 1 3 6 7 8)
    (0 2 3 6 7 8)
    (1 2 3 6 7 8)
    (0 1 4 6 7 8)
    (0 2 4 6 7 8)
    (1 2 4 6 7 8)
    (0 3 4 6 7 8)
    (1 3 4 6 7 8)
    (2 3 4 6 7 8)
    (0 1 5 6 7 8)
    (0 2 5 6 7 8)
    (1 2 5 6 7 8)
    (0 3 5 6 7 8)
    (1 3 5 6 7 8)
    (2 3 5 6 7 8)
    (0 4 5 6 7 8)
    (1 4 5 6 7 8)
    (3 4 5 6 7 8)
    (0 1 2 3 4 9)
    (0 1 2 3 5 9)
    (0 1 2 4 5 9)
    (0 1 3 4 5 9)
    (0 2 3 4 5 9)
    (1 2 3 4 5 9)
    (0 1 2 3 6 9)
    (0 1 2 4 6 9)
    (0 1 3 4 6 9)
    (0 2 3 4 6 9)
    (1 2 3 4 6 9)
    (0 1 2 5 6 9)
    (0 1 3 5 6 9)
    (0 2 3 5 6 9)
    (1 2 3 5 6 9)
    (0 1 4 5 6 9)
    (0 2 4 5 6 9)
    (1 2 4 5 6 9)
    (0 3 4 5 6 9)
    (1 3 4 5 6 9)
    (2 3 4 5 6 9)
    (0 1 2 3 7 9)
    (0 1 2 4 7 9)
    (0 1 3 4 7 9)
    (0 2 3 4 7 9)
    (1 2 3 4 7 9)
    (0 1 2 5 7 9)
    (0 1 3 5 7 9)
    (0 2 3 5 7 9)
    (1 2 3 5 7 9)
    (0 1 4 5 7 9)
    (0 2 4 5 7 9)
    (1 2 4 5 7 9)
    (0 3 4 5 7 9)
    (1 3 4 5 7 9)
    (2 3 4 5 7 9)
    (0 1 2 6 7 9)
    (0 1 3 6 7 9)
    (0 2 3 6 7 9)
    (1 2 3 6 7 9)
    (0 1 4 6 7 9)
    (0 2 4 6 7 9)
    (1 2 4 6 7 9)
    (0 3 4 6 7 9)
    (1 3 4 6 7 9)
    (2 3 4 6 7 9)
    (0 1 5 6 7 9)
    (0 2 5 6 7 9)
    (1 2 5 6 7 9)
    (0 3 5 6 7 9)
    (1 3 5 6 7 9)
    (2 3 5 6 7 9)
    (0 4 5 6 7 9)
    (1 4 5 6 7 9)
    (3 4 5 6 7 9)
    (0 1 2 3 8 9)
    (0 1 2 4 8 9)
    (0 1 3 4 8 9)
    (0 2 3 4 8 9)
    (1 2 3 4 8 9)
    (0 1 2 5 8 9)
    (0 1 3 5 8 9)
    (0 2 3 5 8 9)
    (1 2 3 5 8 9)
    (0 1 4 5 8 9)
    (0 2 4 5 8 9)
    (1 2 4 5 8 9)
    (0 3 4 5 8 9)
    (1 3 4 5 8 9)
    (2 3 4 5 8 9)
    (0 1 2 6 8 9)
    (0 1 3 6 8 9)
    (0 2 3 6 8 9)
    (1 2 3 6 8 9)
    (0 1 4 6 8 9)
    (0 2 4 6 8 9)
    (1 2 4 6 8 9)
    (0 3 4 6 8 9)
    (1 3 4 6 8 9)
    (2 3 4 6 8 9)
    (0 1 5 6 8 9)
    (0 2 5 6 8 9)
    (1 2 5 6 8 9)
    (0 3 5 6 8 9)
    (1 3 5 6 8 9)
    (2 3 5 6 8 9)
    (0 4 5 6 8 9)
    (1 4 5 6 8 9)
    (3 4 5 6 8 9)
    (0 1 2 7 8 9)
    (0 1 3 7 8 9)
    (0 2 3 7 8 9)
    (1 2 3 7 8 9)
    (0 1 4 7 8 9)
    (0 2 4 7 8 9)
    (1 2 4 7 8 9)
    (0 3 4 7 8 9)
    (1 3 4 7 8 9)
    (2 3 4 7 8 9)
    (0 1 5 7 8 9)
    (0 2 5 7 8 9)
    (1 2 5 7 8 9)
    (0 3 5 7 8 9)
    (1 3 5 7 8 9)
    (2 3 5 7 8 9)
    (0 4 5 7 8 9)
    (1 4 5 7 8 9)
    (3 4 5 7 8 9)
    (0 1 6 7 8 9)
    (0 2 6 7 8 9)
    (1 2 6 7 8 9)
    (0 3 6 7 8 9)
    (1 3 6 7 8 9)
    (2 3 6 7 8 9)
    (0 4 6 7 8 9)
    (1 4 6 7 8 9)
    (3 4 6 7 8 9)
    (0 5 6 7 8 9)
    (1 5 6 7 8 9)
    (4 5 6 7 8 9) |}]
