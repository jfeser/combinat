open! Combinat

let print c = print_s @@ [%sexp_of: int array] c

let%expect_test "m_partition_iter" =
  partitions ~n:6 ~k:2 print;
  [%expect {|
    (5 1)
    (4 2)
    (3 3) |}]

let%expect_test "m_partition_iter" =
  partitions ~n:1 ~k:2 print;
  [%expect {| |}]

let%expect_test "m_partition_iter" =
  partitions ~n:6 ~k:3 print;
  [%expect {|
    (4 1 1)
    (3 2 1)
    (2 2 2) |}]

let%expect_test "m_partition_iter" =
  partitions ~n:0 ~k:2 print;
  [%expect {| |}]

let%expect_test "m_partition_iter" =
  partitions ~n:0 ~k:0 print;
  [%expect {| () |}]

let%expect_test "m_partition_iter" =
  partitions ~n:0 ~k:1 print;
  [%expect {| |}]

let%expect_test "partition_with_zeros" =
  partitions_with_zeros ~n:6 ~k:3 print;
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
  partitions_with_zeros ~n:0 ~k:3 print;
  [%expect {|
    (0 0 0) |}]
