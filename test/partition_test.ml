open! Base
open Combinat
open Shared

let%expect_test "m_partition_iter" =
  Partition.iter
    ~f:(fun c ->
      Stdio.print_endline (Sexp.to_string_hum ([%sexp_of: int array] (to_array c))))
    (6, 2);
  [%expect {|
    (5 1)
    (4 2)
    (3 3) |}]

let%expect_test "m_partition_iter" =
  Partition.iter
    ~f:(fun c ->
      Stdio.print_endline (Sexp.to_string_hum ([%sexp_of: int array] (to_array c))))
    (1, 2);
  [%expect {| |}]

let%expect_test "m_partition_iter" =
  Partition.iter
    ~f:(fun c ->
      Stdio.print_endline (Sexp.to_string_hum ([%sexp_of: int array] (to_array c))))
    (6, 3);
  [%expect {|
    (4 1 1)
    (3 2 1)
    (2 2 2) |}]

let%expect_test "m_partition_iter" =
  Partition.iter
    ~f:(fun c ->
      Stdio.print_endline (Sexp.to_string_hum ([%sexp_of: int array] (to_array c))))
    (0, 2);
  [%expect {| |}]

let%expect_test "partition_with_zeros" =
  Partition.With_zeros.iter (6, 3) ~f:(fun c ->
      Stdio.print_endline (Sexp.to_string_hum ([%sexp_of: int array] (to_array c))));
  [%expect
    {|
    (6 0 0)
    (5 1 0)
    (4 2 0)
    (3 3 0)
    (4 1 1)
    (3 2 1)
    (2 2 2) |}]
