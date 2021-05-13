open Combinat
open Shared

let%expect_test "sequences-0-1" =
  require_does_raise [%here] (fun () -> sequences [] ~k:1 ignore);
  [%expect {|
    ("sequences: expected n > 0" (n 0)) |}]

let%expect_test "sequences-k<0" =
  require_does_raise [%here] (fun () -> sequences [ 0; 1; 2 ] ~k:(-1) ignore);
  [%expect {|
    ("sequences: expected k >= 0" (k -1)) |}]

let%expect_test "sequences-0-0" =
  sequences [] ~k:0 print;
  [%expect {|
    () |}]

let%expect_test "sequences-5-0" =
  sequences [ 0; 1; 2; 3; 4 ] ~k:0 print;
  [%expect {|
    () |}]

let%expect_test "sequences-5-1" =
  sequences [ 0; 1; 2; 3; 4 ] ~k:1 print;
  [%expect {|
    (0)
    (1)
    (2)
    (3)
    (4) |}]

let%expect_test "sequences-5-2" =
  sequences [ 0; 1; 2; 3; 4 ] ~k:2 print;
  [%expect
    {|
    (0 0)
    (0 1)
    (0 2)
    (0 3)
    (0 4)
    (1 0)
    (1 1)
    (1 2)
    (1 3)
    (1 4)
    (2 0)
    (2 1)
    (2 2)
    (2 3)
    (2 4)
    (3 0)
    (3 1)
    (3 2)
    (3 3)
    (3 4)
    (4 0)
    (4 1)
    (4 2)
    (4 3)
    (4 4) |}]
