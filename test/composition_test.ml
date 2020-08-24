open! Base
open Combinat
open Shared

let%expect_test "compositions" =
  Composition.create ~k:4 ~n:7
  |> Composition.iter ~f:(fun c ->
         Stdio.print_endline
           (Sexp.to_string_hum ([%sexp_of: int array] (to_array c))));
  [%expect
    {|
       (1 1 1 4)
       (1 1 2 3)
       (1 2 1 3)
       (2 1 1 3)
       (1 1 3 2)
       (1 2 2 2)
       (2 1 2 2)
       (1 3 1 2)
       (2 2 1 2)
       (3 1 1 2)
       (1 1 4 1)
       (1 2 3 1)
       (2 1 3 1)
       (1 3 2 1)
       (2 2 2 1)
       (3 1 2 1)
       (1 4 1 1)
       (2 3 1 1)
       (4 1 1 1) |}]

let%expect_test "compositions-2" =
  for n = 2 to 8 do
    Composition.create ~k:2 ~n
    |> Composition.iter ~f:(fun c ->
           Stdio.print_endline
             (Sexp.to_string_hum ([%sexp_of: int array] (to_array c))));
    [%expect
      {|
         (1 1 1 4)
         (1 1 2 3)
         (1 2 1 3)
         (2 1 1 3)
         (1 1 3 2)
         (1 2 2 2)
         (2 1 2 2)
         (1 3 1 2)
         (2 2 1 2)
         (3 1 1 2)
         (1 1 4 1)
         (1 2 3 1)
         (2 1 3 1)
         (1 3 2 1)
         (2 2 2 1)
         (3 1 2 1)
         (1 4 1 1)
         (2 3 1 1)
         (4 1 1 1) |}]
  done
