open! Base
open Combinat
open Shared

let%expect_test "m_partition_iter" =
  Partition.create ~n:6 ~parts:2
  |> Partition.iter ~f:(fun c ->
         Stdio.print_endline
           (Sexp.to_string_hum ([%sexp_of: int array] (to_array c))));
  [%expect {|
    (5 1)
    (4 2)
    (3 3) |}]

let%expect_test "m_partition_iter" =
  Partition.create ~n:1 ~parts:2
  |> Partition.iter ~f:(fun c ->
         Stdio.print_endline
           (Sexp.to_string_hum ([%sexp_of: int array] (to_array c))));
  [%expect {| |}]

let%expect_test "m_partition_iter" =
  Partition.create ~n:6 ~parts:3
  |> Partition.iter ~f:(fun c ->
         Stdio.print_endline
           (Sexp.to_string_hum ([%sexp_of: int array] (to_array c))));
  [%expect {|
    (4 1 1)
    (3 2 1)
    (2 2 2) |}]

let%expect_test "m_partition_iter" =
  Partition.create ~n:0 ~parts:2
  |> Partition.iter ~f:(fun c ->
         Stdio.print_endline
           (Sexp.to_string_hum ([%sexp_of: int array] (to_array c))));
  [%expect {| |}]

let%expect_test "partition_with_zeros" =
  Partition.With_zeros.create ~n:6 ~parts:3
  |> Partition.With_zeros.iter ~f:(fun c ->
         Stdio.print_endline
           (Sexp.to_string_hum ([%sexp_of: int array] (to_array c))));
  [%expect
    {|
    (6 0 0)
    (5 1 0)
    (4 2 0)
    (3 3 0)
    (4 1 1)
    (3 2 1)
    (2 2 2) |}]
