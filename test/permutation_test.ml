open Combinat

let print c = print_s @@ [%sexp_of: Int_array.t] c

(* let%expect_test "permutations_iter" =
 *   Of_list.(
 *     iter (create [ 1; 2; 3; 4 ]) ~f:(fun c -> print_s @@ [%sexp_of: int list] c));
 *   [%expect
 *     {|
 *     (1 2 3 4)
 *     (1 2 4 3)
 *     (1 3 2 4)
 *     (1 3 4 2)
 *     (1 4 2 3)
 *     (1 4 3 2)
 *     (2 1 3 4)
 *     (2 1 4 3)
 *     (2 3 1 4)
 *     (2 3 4 1)
 *     (2 4 1 3)
 *     (2 4 3 1)
 *     (3 1 2 4)
 *     (3 1 4 2)
 *     (3 2 1 4)
 *     (3 2 4 1)
 *     (3 4 1 2)
 *     (3 4 2 1)
 *     (4 1 2 3)
 *     (4 1 3 2)
 *     (4 2 1 3)
 *     (4 2 3 1)
 *     (4 3 1 2)
 *     (4 3 2 1) |}] *)

(* let%expect_test "sorted_permutations_iter" =
 *   let lt x y = match (x, y) with 1, 3 | 2, 3 | 2, 4 -> true | _ -> false in
 *   permutations_ordered ~n:4 ~lt print;
 *   [%expect
 *     {|
 *     (1 2 3 4)
 *     (1 2 4 3)
 *     (2 1 3 4)
 *     (2 1 4 3)
 *     (2 4 1 3) |}]
 * 
 * let%expect_test "sorted_permutations_young" =
 *   let young_tableaux x y =
 *     match (x, y) with
 *     | 1, (2 | 3 | 4 | 5 | 6 | 7 | 8 | 9)
 *     | 2, (3 | 5 | 6 | 8 | 9)
 *     | 3, (6 | 9)
 *     | 4, (5 | 6 | 7 | 8 | 9)
 *     | 5, (6 | 8 | 9)
 *     | 6, 9
 *     | 7, (8 | 9)
 *     | 8, 9 ->
 *         true
 *     | _ -> false
 *   in
 *   permutations_ordered ~n:9 ~lt:young_tableaux print;
 *   [%expect
 *     {|
 *     (1 2 3 4 5 6 7 8 9)
 *     (1 2 3 4 5 7 6 8 9)
 *     (1 2 3 4 5 8 6 7 9)
 *     (1 2 3 4 6 7 5 8 9)
 *     (1 2 3 4 6 8 5 7 9)
 *     (1 2 4 3 5 6 7 8 9)
 *     (1 2 4 3 5 7 6 8 9)
 *     (1 2 4 3 5 8 6 7 9)
 *     (1 2 4 3 6 7 5 8 9)
 *     (1 2 4 3 6 8 5 7 9)
 *     (1 2 5 3 6 7 4 8 9)
 *     (1 2 5 3 6 8 4 7 9)
 *     (1 2 5 3 4 6 7 8 9)
 *     (1 2 5 3 4 7 6 8 9)
 *     (1 2 5 3 4 8 6 7 9)
 *     (1 2 6 3 4 7 5 8 9)
 *     (1 2 6 3 4 8 5 7 9)
 *     (1 2 7 3 4 8 5 6 9)
 *     (1 2 6 3 5 7 4 8 9)
 *     (1 2 6 3 5 8 4 7 9)
 *     (1 2 7 3 5 8 4 6 9)
 *     (1 3 4 2 5 6 7 8 9)
 *     (1 3 4 2 5 7 6 8 9)
 *     (1 3 4 2 5 8 6 7 9)
 *     (1 3 4 2 6 7 5 8 9)
 *     (1 3 4 2 6 8 5 7 9)
 *     (1 3 5 2 6 7 4 8 9)
 *     (1 3 5 2 6 8 4 7 9)
 *     (1 4 5 2 6 7 3 8 9)
 *     (1 4 5 2 6 8 3 7 9)
 *     (1 3 5 2 4 6 7 8 9)
 *     (1 3 5 2 4 7 6 8 9)
 *     (1 3 5 2 4 8 6 7 9)
 *     (1 3 6 2 4 7 5 8 9)
 *     (1 3 6 2 4 8 5 7 9)
 *     (1 3 7 2 4 8 5 6 9)
 *     (1 3 6 2 5 7 4 8 9)
 *     (1 3 6 2 5 8 4 7 9)
 *     (1 3 7 2 5 8 4 6 9)
 *     (1 4 6 2 5 7 3 8 9)
 *     (1 4 6 2 5 8 3 7 9)
 *     (1 4 7 2 5 8 3 6 9) |}] *)

let%expect_test "restricted_permutations_iter" =
  let f a =
    let open Int_array in
    let module Array = struct
      let get = get
    end in
    match length a with
    | 1 -> not (a.(0) = 2)
    | 2 -> not (a.(0) = 1 && a.(1) = 4)
    | 3 ->
        not
          ( (a.(0) = 1 && a.(1) = 3 && a.(2) = 2)
          || (a.(0) = 3 && a.(1) = 1 && a.(2) = 4) )
    | 4 -> not (a.(0) = 4 && a.(1) = 3 && a.(2) = 1 && a.(3) = 2)
    | _ -> true
  in
  permutations_filtered ~n:4 ~f print;
  [%expect
    {|
    (1 2 3 4)
    (1 2 4 3)
    (1 3 4 2)
    (3 1 2 4)
    (3 2 1 4)
    (3 2 4 1)
    (3 4 1 2)
    (3 4 2 1)
    (4 1 2 3)
    (4 1 3 2)
    (4 2 1 3)
    (4 2 3 1)
    (4 3 2 1) |}]
