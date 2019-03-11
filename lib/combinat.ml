open Base
open Util
module Seq = Sequence
open Bigarray

type int_array = (int, int_elt, c_layout) Array1.t

module Partition = Container.Make0 (struct
  type t = int * int

  module Elt = struct
    type t = int_array

    let equal = Util.equal
  end

  let rec loop1 a s j =
    if a.{j} >= a.{1} - 1 then loop1 a (s + a.{j}) (j + 1) else (s, j)

  let rec loop2 a x s j =
    if j > 1 then (
      a.{j} <- x ;
      let s = s - x in
      let j = j - 1 in
      loop2 a x s j )
    else s

  let fold (n, m) ~init ~f =
    if m = 0 then init
    else if m = 1 then f init (Bigarray.(Array1.of_array int c_layout) [|n|])
    else
      let a = Bigarray.(Array1.create int c_layout (m + 2)) in
      let a' = Array1.sub a 1 m in
      a.{1} <- n - m + 1 ;
      for i = 2 to m do
        a.{i} <- 1
      done ;
      a.{m + 1} <- -1 ;
      let rec h2 acc =
        let acc = f acc a' in
        if a.{2} >= a.{1} - 1 then
          let j = 3 in
          let s = a.{1} + a.{2} - 1 in
          let s, j = loop1 a s j in
          if j <= m then (
            let x = a.{j} + 1 in
            a.{j} <- x ;
            let j = j - 1 in
            let s = loop2 a x s j in
            a.{1} <- s ; h2 acc )
          else acc
        else (
          a.{1} <- a.{1} - 1 ;
          a.{2} <- a.{2} + 1 ;
          h2 acc )
      in
      h2 init

  let iter = `Define_using_fold

  let length = `Define_using_fold
end)

let%expect_test "m_partition_iter" =
  Partition.iter
    ~f:(fun c ->
      Stdio.print_endline
        (Sexp.to_string_hum ([%sexp_of: int array] (to_array c))) )
    (6, 3) ;
  [%expect {|
    (4 1 1)
    (3 2 1)
    (2 2 2) |}]

module Partition_with_zeros = Container.Make0 (struct
  type t = int * int

  module Elt = struct
    type t = int_array

    let equal = Util.equal
  end

  let fold (n, m) ~init ~f =
    let arr = Bigarray.(Array1.create int c_layout m) in
    let f acc c =
      Array1.fill arr 0 ;
      Array1.blit c (Array1.sub arr 0 (Array1.dim c)) ;
      f acc arr
    in
    let rec fold m' acc =
      if m < m' then acc
      else fold (m' + 1) (Partition.fold (n, m') ~init:acc ~f)
    in
    fold 0 init

  let iter = `Define_using_fold

  let length = `Define_using_fold
end)

let%expect_test "partition_with_zeros" =
  Partition_with_zeros.iter (6, 3) ~f:(fun c ->
      Stdio.print_endline
        (Sexp.to_string_hum ([%sexp_of: int array] (to_array c))) ) ;
  [%expect
    {|
    (6 0 0)
    (5 1 0)
    (4 2 0)
    (3 3 0)
    (4 1 1)
    (3 2 1)
    (2 2 2) |}]

module Permutation = Container.Make0 (struct
  type t = int array

  module Elt = struct
    type t = int_array

    let equal = Util.equal
  end

  let fold items ~init ~f =
    let n = Array.length items in
    let a = Bigarray.(Array1.create int c_layout (n + 1)) in
    for i = 1 to n do
      a.{i} <- items.(i - 1)
    done ;
    a.{0} <- items.(n - 1) - 1 ;
    let a' = Array1.sub a 1 n in
    let rec l1 acc =
      let acc = f acc a' in
      let j = n - 1 in
      let rec loop1 j = if a.{j} >= a.{j + 1} then loop1 (j - 1) else j in
      let j = loop1 j in
      if j > 0 then (
        let l = n in
        let rec loop2 l = if a.{j} >= a.{l} then loop2 (l - 1) else l in
        let l = loop2 l in
        let tmp = a.{j} in
        a.{j} <- a.{l} ;
        a.{l} <- tmp ;
        let k = j + 1 in
        let l = n in
        let rec loop3 k l =
          if k < l then (
            let tmp = a.{k} in
            a.{k} <- a.{l} ;
            a.{l} <- tmp ;
            loop3 (k + 1) (l - 1) )
        in
        loop3 k l ; l1 acc )
      else acc
    in
    l1 init

  let iter = `Define_using_fold

  let length = `Define_using_fold
end)

let%expect_test "permutations_iter" =
  Permutation.iter [|1; 2; 3; 4|] ~f:(fun c ->
      Stdio.print_endline
        (Sexp.to_string_hum ([%sexp_of: int array] (to_array c))) ) ;
  [%expect
    {|
    (1 2 3 4)
    (1 2 4 3)
    (1 3 2 4)
    (1 3 4 2)
    (1 4 2 3)
    (1 4 3 2)
    (2 1 3 4)
    (2 1 4 3)
    (2 3 1 4)
    (2 3 4 1)
    (2 4 1 3)
    (2 4 3 1)
    (3 1 2 4)
    (3 1 4 2)
    (3 2 1 4)
    (3 2 4 1)
    (3 4 1 2)
    (3 4 2 1)
    (4 1 2 3)
    (4 1 3 2)
    (4 2 1 3)
    (4 2 3 1)
    (4 3 1 2)
    (4 3 2 1) |}]

module SortedPermutation = Container.Make0 (struct
  type t = int * (int -> int -> bool)

  module Elt = struct
    type t = int_array * int_array

    let equal (a1, a2) (b1, b2) = Util.equal a1 b1 && Util.equal a2 b2
  end

  let fold p ~init ~f =
    let n, ( << ) = p in
    let ( << ) x y =
      match (x, y) with 0, 0 -> false | 0, _ -> true | x, y -> x << y
    in
    let a = Bigarray.(Array1.create int c_layout (n + 1)) in
    let a' = Bigarray.(Array1.create int c_layout (n + 1)) in
    let elem = (Bigarray.Array1.sub a 1 n, Bigarray.Array1.sub a' 1 n) in
    for j = 0 to n do
      a.{j} <- j ; a'.{j} <- j
    done ;
    let rec v3 acc k =
      let j = a'.{k} in
      let l = a.{j - 1} in
      if l << k then (
        for j = j to k - 1 do
          let l = a.{j + 1} in
          a.{j} <- l ; a'.{l} <- j
        done ;
        a.{k} <- k ;
        a'.{k} <- k ;
        let k = k - 1 in
        if k > 0 then v3 acc k else acc )
      else (
        a.{j - 1} <- k ;
        a.{j} <- l ;
        a'.{k} <- j - 1 ;
        a'.{l} <- j ;
        v3 (f acc elem) n )
    in
    v3 (f init elem) n

  let iter = `Define_using_fold

  let length = `Define_using_fold
end)

let%expect_test "sorted_permutations_iter" =
  SortedPermutation.iter
    (4, fun x y -> match (x, y) with 1, 3 | 2, 3 | 2, 4 -> true | _ -> false)
    ~f:(fun (c, _) ->
      Stdio.print_endline
        (Sexp.to_string_hum ([%sexp_of: int array] (to_array c))) ) ;
  [%expect
    {|
    (1 2 3 4)
    (1 2 4 3)
    (2 1 3 4)
    (2 1 4 3)
    (2 4 1 3) |}]

let%expect_test "sorted_permutations_young" =
  let young_tableaux x y =
    match (x, y) with
    | 1, (2 | 3 | 4 | 5 | 6 | 7 | 8 | 9)
     |2, (3 | 5 | 6 | 8 | 9)
     |3, (6 | 9)
     |4, (5 | 6 | 7 | 8 | 9)
     |5, (6 | 8 | 9)
     |6, 9
     |7, (8 | 9)
     |8, 9 ->
        true
    | _ -> false
  in
  SortedPermutation.iter (9, young_tableaux) ~f:(fun (_, c) ->
      Stdio.print_endline
        (Sexp.to_string_hum ([%sexp_of: int array] (to_array c))) ) ;
  [%expect
    {|
    (1 2 3 4 5 6 7 8 9)
    (1 2 3 4 5 7 6 8 9)
    (1 2 3 4 5 8 6 7 9)
    (1 2 3 4 6 7 5 8 9)
    (1 2 3 4 6 8 5 7 9)
    (1 2 4 3 5 6 7 8 9)
    (1 2 4 3 5 7 6 8 9)
    (1 2 4 3 5 8 6 7 9)
    (1 2 4 3 6 7 5 8 9)
    (1 2 4 3 6 8 5 7 9)
    (1 2 5 3 6 7 4 8 9)
    (1 2 5 3 6 8 4 7 9)
    (1 2 5 3 4 6 7 8 9)
    (1 2 5 3 4 7 6 8 9)
    (1 2 5 3 4 8 6 7 9)
    (1 2 6 3 4 7 5 8 9)
    (1 2 6 3 4 8 5 7 9)
    (1 2 7 3 4 8 5 6 9)
    (1 2 6 3 5 7 4 8 9)
    (1 2 6 3 5 8 4 7 9)
    (1 2 7 3 5 8 4 6 9)
    (1 3 4 2 5 6 7 8 9)
    (1 3 4 2 5 7 6 8 9)
    (1 3 4 2 5 8 6 7 9)
    (1 3 4 2 6 7 5 8 9)
    (1 3 4 2 6 8 5 7 9)
    (1 3 5 2 6 7 4 8 9)
    (1 3 5 2 6 8 4 7 9)
    (1 4 5 2 6 7 3 8 9)
    (1 4 5 2 6 8 3 7 9)
    (1 3 5 2 4 6 7 8 9)
    (1 3 5 2 4 7 6 8 9)
    (1 3 5 2 4 8 6 7 9)
    (1 3 6 2 4 7 5 8 9)
    (1 3 6 2 4 8 5 7 9)
    (1 3 7 2 4 8 5 6 9)
    (1 3 6 2 5 7 4 8 9)
    (1 3 6 2 5 8 4 7 9)
    (1 3 7 2 5 8 4 6 9)
    (1 4 6 2 5 7 3 8 9)
    (1 4 6 2 5 8 3 7 9)
    (1 4 7 2 5 8 3 6 9) |}]

module RestrictedPermutation = Container.Make0 (struct
  type t = int * (int_array -> bool)

  module Elt = struct
    type t = int_array

    let equal = Util.equal
  end

  let fold p ~init ~f =
    let n, t = p in
    let a = Bigarray.(Array1.create int c_layout (n + 1)) in
    let l = Bigarray.(Array1.create int c_layout (n + 1)) in
    let u = Bigarray.(Array1.create int c_layout (n + 1)) in
    let t_args = Array.create ~len:(n + 1) (Bigarray.Array1.sub a 1 1) in
    for k = 1 to n do
      t_args.(k) <- Bigarray.Array1.sub a 1 k
    done ;
    let elem = Bigarray.Array1.sub a 1 n in
    for k = 0 to n - 1 do
      l.{k} <- k + 1
    done ;
    l.{n} <- 0 ;
    let rec x3 acc k p q =
      a.{k} <- q ;
      if t t_args.(k) then
        if k = n then x6 (f acc elem) k
        else (
          u.{k} <- p ;
          l.{p} <- l.{q} ;
          x3 acc (k + 1) 0 l.{0} )
      else
        let p = q in
        let q = l.{p} in
        if q = 0 then x6 acc k else x3 acc k p q
    and x6 acc k =
      let k = k - 1 in
      if k = 0 then acc
      else
        let p = u.{k} in
        let q = a.{k} in
        l.{p} <- q ;
        let p = q in
        let q = l.{p} in
        if q = 0 then x6 acc k else x3 acc k p q
    in
    x3 init 1 0 l.{0}

  let iter = `Define_using_fold

  let length = `Define_using_fold
end)

let%expect_test "restricted_permutations_iter" =
  RestrictedPermutation.iter
    ( 4
    , fun a ->
        match Bigarray.Array1.dim a with
        | 1 -> not (a.{0} = 2)
        | 2 -> not (a.{0} = 1 && a.{1} = 4)
        | 3 ->
            not
              ( (a.{0} = 1 && a.{1} = 3 && a.{2} = 2)
              || (a.{0} = 3 && a.{1} = 1 && a.{2} = 4) )
        | 4 -> not (a.{0} = 4 && a.{1} = 3 && a.{2} = 1 && a.{3} = 2)
        | _ -> true )
    ~f:(fun c ->
      Stdio.print_endline
        (Sexp.to_string_hum ([%sexp_of: int array] (to_array c))) ) ;
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

module Combination = struct
  include Container.Make0 (struct
    type t = int * int

    module Elt = struct
      type t = int_array

      let equal = Util.equal
    end

    let fold (t, n) ~init ~f =
      assert (t >= 0 && t <= n) ;
      if t = 0 then init
      else if t = n then
        f init
          ( Array.init n ~f:(fun i -> i)
          |> Bigarray.(Array1.of_array int c_layout) )
      else
        let c = Bigarray.(Array1.create int c_layout (t + 3)) in
        for i = 1 to t do
          c.{i} <- i - 1
        done ;
        c.{t + 1} <- n ;
        c.{t + 2} <- 0 ;
        let j = t in
        let x = 0 in
        let c' = Array1.sub c 1 t in
        let rec t2 acc x j =
          let acc = f acc c' in
          if j > 0 then (
            let x = j in
            c.{j} <- x ;
            let j = j - 1 in
            t2 acc x j )
          else if c.{1} + 1 < c.{2} then (
            c.{1} <- c.{1} + 1 ;
            t2 acc x j )
          else
            let j = 2 in
            let rec loop j =
              c.{j - 1} <- j - 2 ;
              if c.{j} + 1 = c.{j + 1} then
                let j = j + 1 in
                loop j
              else j
            in
            let j = loop j in
            let x = c.{j} + 1 in
            if j <= t then (
              c.{j} <- x ;
              let j = j - 1 in
              t2 acc x j )
            else acc
        in
        t2 init x j

    let iter = `Define_using_fold

    let length = `Define_using_fold
  end)
end

let%expect_test "combinations" =
  Combination.iter (3, 5) ~f:(fun c ->
      Stdio.print_endline
        (Sexp.to_string_hum ([%sexp_of: int array] (to_array c))) ) ;
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
