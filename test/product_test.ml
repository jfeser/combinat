open Combinat
open Shared

let%expect_test "product-0" =
  product [] print;
  [%expect {| () |}]

let%expect_test "product-1" =
  product [ [] ] print;
  [%expect {|  |}]

let%expect_test "" =
  product [ []; [ 1 ] ] print;
  [%expect {|  |}]

let%expect_test "" =
  product [ [ 2 ]; [ 1 ] ] print;
  [%expect {| (2 1) |}]

let%expect_test "" =
  product [ [ 1; 2; 3 ]; [ 4; 5 ]; [ 6 ]; [ 7; 8 ] ] print;
  [%expect {|
    (1 4 6 7)
    (2 4 6 7)
    (3 4 6 7)
    (1 5 6 7)
    (2 5 6 7)
    (3 5 6 7)
    (1 4 6 8)
    (2 4 6 8)
    (3 4 6 8)
    (1 5 6 8)
    (2 5 6 8)
    (3 5 6 8) |}]
