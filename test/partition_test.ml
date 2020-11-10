open! Base
open! Stdio
open! Combinat
open Partition

let print_partition ~n ~parts =
  create ~n ~parts |> iter ~f:(fun c -> print_s @@ [%sexp_of: Int_array.t] c)

let print_partition_z ~n ~parts =
  With_zeros.(
    create ~n ~parts |> iter ~f:(fun c -> print_s @@ [%sexp_of: Int_array.t] c))

let%expect_test "m_partition_iter" =
  print_partition ~n:6 ~parts:2;
  [%expect {|
    (5 1)
    (4 2)
    (3 3) |}]

let%expect_test "m_partition_iter" =
  print_partition ~n:1 ~parts:2;
  [%expect {| |}]

let%expect_test "m_partition_iter" =
  print_partition ~n:6 ~parts:3;
  [%expect {|
    (4 1 1)
    (3 2 1)
    (2 2 2) |}]

let%expect_test "m_partition_iter" =
  print_partition ~n:0 ~parts:2;
  [%expect {| |}]

let%expect_test "m_partition_iter" =
  print_partition ~n:0 ~parts:0;
  [%expect {| () |}]

let%expect_test "m_partition_iter" =
  print_partition ~n:0 ~parts:1;
  [%expect {| |}]

let%expect_test "partition_with_zeros" =
  print_partition_z ~n:6 ~parts:3;
  [%expect
    {|
    (6 0 0)
    (5 1 0)
    (4 2 0)
    (3 3 0)
    (4 1 1)
    (3 2 1)
    (2 2 2) |}]

let%expect_test "partition_with_zeros" =
  print_partition_z ~n:0 ~parts:3;
  [%expect {|
    (0 0 0) |}]
