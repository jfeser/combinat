open Combinat

let print c = print_s @@ [%sexp_of: int array] c

let%expect_test "compositions-n<0" =
  require_does_raise [%here] (fun () -> compositions ~k:(-2) ~n:(-1) ignore);
  [%expect {| (Failure "composition: expected n >= 0, got n=-1") |}]

let%expect_test "compositions-k<0" =
  require_does_raise [%here] (fun () -> compositions ~k:(-1) ~n:2 ignore);
  [%expect {| (Failure "composition: expected k >= 0, got k=-1") |}]

let%expect_test "compositions" =
  compositions ~k:0 ~n:0 print;
  [%expect {| () |}]

let%expect_test "compositions" =
  compositions ~k:0 ~n:7 print;
  [%expect {| |}]

let%expect_test "compositions" =
  compositions ~k:1 ~n:7 print;
  [%expect {| (7) |}]

let%expect_test "compositions" =
  compositions ~k:2 ~n:7 print;
  [%expect {|
    (1 6)
    (2 5)
    (3 4)
    (4 3)
    (5 2)
    (6 1) |}]

let%expect_test "compositions" =
  compositions ~k:3 ~n:7 print;
  [%expect
    {|
    (1 1 5)
    (1 2 4)
    (2 1 4)
    (1 3 3)
    (2 2 3)
    (3 1 3)
    (1 4 2)
    (2 3 2)
    (3 2 2)
    (4 1 2)
    (1 5 1)
    (2 4 1)
    (3 3 1)
    (4 2 1)
    (5 1 1) |}]

let%expect_test "compositions" =
  compositions ~k:4 ~n:7 print;
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
       (3 2 1 1)
       (4 1 1 1) |}]

let%expect_test "compositions" =
  compositions ~k:5 ~n:7 print;
  [%expect
    {|
       (1 1 1 1 3)
       (1 1 1 2 2)
       (1 1 2 1 2)
       (1 2 1 1 2)
       (2 1 1 1 2)
       (1 1 1 3 1)
       (1 1 2 2 1)
       (1 2 1 2 1)
       (2 1 1 2 1)
       (1 1 3 1 1)
       (1 2 2 1 1)
       (2 1 2 1 1)
       (1 3 1 1 1)
       (2 2 1 1 1)
       (3 1 1 1 1) |}]

let%expect_test "compositions" =
  compositions ~k:6 ~n:7 print;
  [%expect
    {|
     (1 1 1 1 1 2)
     (1 1 1 1 2 1)
     (1 1 1 2 1 1)
     (1 1 2 1 1 1)
     (1 2 1 1 1 1)
     (2 1 1 1 1 1) |}]

let%expect_test "compositions" =
  compositions ~k:7 ~n:7 print;
  [%expect {|
       (1 1 1 1 1 1 1) |}]

let%expect_test "compositions-2" =
  for n = 2 to 8 do
    compositions ~k:2 ~n print;
    [%expect
      {|
         (* CR expect_test: Collector ran multiple times with different outputs *)
         =========================================================================
         (1 1)

         =========================================================================
         (1 2)
         (2 1)

         =========================================================================
         (1 3)
         (2 2)
         (3 1)

         =========================================================================
         (1 4)
         (2 3)
         (3 2)
         (4 1)

         =========================================================================
         (1 5)
         (2 4)
         (3 3)
         (4 2)
         (5 1)

         =========================================================================
         (1 6)
         (2 5)
         (3 4)
         (4 3)
         (5 2)
         (6 1)

         =========================================================================
         (1 7)
         (2 6)
         (3 5)
         (4 4)
         (5 3)
         (6 2)
         (7 1) |}]
  done
