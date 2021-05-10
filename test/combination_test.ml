open Combinat

let print c = print_s @@ [%sexp_of: int array] c

let%expect_test "combinations-5-5" =
  combinations [ 0; 1; 2; 3; 4 ] ~k:5 print;
  [%expect {|
    (0 1 2 3 4) |}]

let%expect_test "combinations-5-4" =
  combinations [ 0; 1; 2; 3; 4 ] ~k:4 print;
  [%expect
    {|
    (0 1 2 3)
    (0 1 2 4)
    (0 1 3 4)
    (0 2 3 4)
    (1 2 3 4) |}]

let%expect_test "combinations-5-3" =
  combinations [ 0; 1; 2; 3; 4 ] ~k:3 print;
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

let%expect_test "combinations-5-2" =
  combinations [ 0; 1; 2; 3; 4 ] ~k:2 print;
  [%expect
    {|
       (0 1)
       (0 2)
       (1 2)
       (0 3)
       (1 3)
       (2 3)
       (0 4)
       (1 4)
       (2 4)
       (3 4) |}]

let%expect_test "combinations-5-1" =
  combinations [ 0; 1; 2; 3; 4 ] ~k:1 print;
  [%expect {|
       (0)
       (1)
       (2)
       (3)
       (4) |}]

let%expect_test "combinations-5-0" =
  combinations [ 0; 1; 2; 3; 4 ] ~k:0 print;
  [%expect {|
       () |}]
