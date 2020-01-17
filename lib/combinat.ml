open Base
open Util
module Seq = Sequence

let rec binom n k =
  if k > n then failwith "Undefined"
  else if k = 0 then 1
  else if k > n / 2 then binom n (n - k)
  else n * binom (n - 1) (k - 1) / k

let rec fact n = if n = 1 then 1 else n * fact (n - 1)

let%expect_test "" =
  binom 4 2 |> Stdio.printf "%d\n";
  [%expect {| 6 |}]

open Bigarray

type int_array = (int, int_elt, c_layout) Bigarray.Array1.t

module Unsafe = struct
  module Bigarray = struct
    include Bigarray

    module Array1 = struct
      include Bigarray.Array1

      let get : int_array -> int -> int = unsafe_get

      let set : int_array -> int -> int -> unit = unsafe_set
    end
  end
end

module type PARTITION = sig
  type t = int * int

  val create : n:int -> parts:int -> t

  include Container.S0 with type t := t and type elt := int_array
end

module Partition_t = struct
  type t = int * int

  let create ~n ~parts = (n, parts)
end

module Partition_core = struct
  include Partition_t

  module Elt = struct
    type t = int_array

    let equal = Util.equal
  end

  open Unsafe

  let rec loop1 a s j =
    if a.{j} >= a.{1} - 1 then loop1 a (s + a.{j}) (j + 1) else (s, j)

  let rec loop2 a x s j =
    if j > 1 then (
      a.{j} <- x;
      let s = s - x in
      let j = j - 1 in
      loop2 a x s j )
    else s

  let rec h2 a a' f m acc =
    let acc = f acc a' in
    if a.{2} >= a.{1} - 1 then
      let j = 3 in
      let s = a.{1} + a.{2} - 1 in
      let s, j = loop1 a s j in
      if j <= m then (
        let x = a.{j} + 1 in
        a.{j} <- x;
        let j = j - 1 in
        let s = loop2 a x s j in
        a.{1} <- s;
        h2 a a' f m acc )
      else acc
    else (
      a.{1} <- a.{1} - 1;
      a.{2} <- a.{2} + 1;
      h2 a a' f m acc )

  let fold (n, m) ~init ~f =
    if n < m || m = 0 then init
    else if m = 1 then f init (Bigarray.(Array1.of_array int c_layout) [| n |])
    else
      let a = Bigarray.(Array1.create int c_layout (m + 2)) in
      let a' = Array1.sub a 1 m in
      a.{1} <- n - m + 1;
      for i = 2 to m do
        a.{i} <- 1
      done;
      a.{m + 1} <- -1;
      h2 a a' f m init

  let iter = `Define_using_fold

  let length = `Define_using_fold
end

module Partition = struct
  include Partition_t
  include Container.Make0 (Partition_core)
end

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

module Partition_with_zeros = struct
  include Partition_t

  include Container.Make0 (struct
    include Partition_core

    let orig_fold = fold

    let fold (n, m) ~init ~f =
      let arr = Bigarray.(Array1.create int c_layout m) in
      let f acc c =
        Array1.fill arr 0;
        Array1.blit c (Array1.sub arr 0 (Array1.dim c));
        f acc arr
      in
      let rec fold m' acc =
        if m < m' then acc else fold (m' + 1) (orig_fold (n, m') ~init:acc ~f)
      in
      fold 0 init
  end)
end

let%expect_test "partition_with_zeros" =
  Partition_with_zeros.iter (6, 3) ~f:(fun c ->
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

module Permutation_t = struct
  type t = int

  let create x = x
end

module Permutation = struct
  include Permutation_t

  include Container.Make0 (struct
    include Permutation_t

    module Elt = struct
      type t = int_array

      let equal = Util.equal
    end

    open Unsafe

    let rec loop1 a j = if a.{j} >= a.{j + 1} then loop1 a (j - 1) else j

    let rec loop2 a j l = if a.{j} >= a.{l} then loop2 a j (l - 1) else l

    let rec loop3 a k l =
      if k < l then (
        let tmp = a.{k} in
        a.{k} <- a.{l};
        a.{l} <- tmp;
        loop3 a (k + 1) (l - 1) )

    let rec l1 a a' f n acc =
      let acc = f acc a' in
      let j = n - 1 in
      let j = loop1 a j in
      if j > 0 then (
        let l = n in
        let l = loop2 a j l in
        let tmp = a.{j} in
        a.{j} <- a.{l};
        a.{l} <- tmp;
        let k = j + 1 in
        let l = n in
        loop3 a k l;
        l1 a a' f n acc )
      else acc

    let fold n ~init ~f =
      let a = Bigarray.(Array1.create int c_layout (n + 1)) in
      for i = 1 to n do
        a.{i} <- i - 1
      done;
      a.{0} <- n - 2;
      let a' = Bigarray.Array1.sub a 1 n in
      l1 a a' f n init

    let iter = `Define_using_fold

    let length = `Custom fact
  end)

  module Of_list = struct
    type 'a t = 'a list

    let create = Fn.id

    include Build.Make (struct
      type nonrec 'a t = 'a t

      type 'a elt = 'a list

      let fold l ~init ~f =
        let n = List.length l in
        let elems = List.to_array l in
        fold n ~init ~f:(fun x a -> f x (List.init n ~f:(fun i -> elems.(a.{i}))))

      let iter = `Define_using_fold

      let length =
        let length l = length (List.length l) in
        `Custom length
    end)
  end
end

let%expect_test "permutations_iter" =
  Permutation.Of_list.iter [ 1; 2; 3; 4 ] ~f:(fun c ->
      Stdio.print_endline (Sexp.to_string_hum ([%sexp_of: int list] c)));
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

module SortedPermutation = struct
  type t = int * (int -> int -> bool)

  let create n compare = (n, compare)

  include Container.Make0 (struct
    type nonrec t = t

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
        a.{j} <- j;
        a'.{j} <- j
      done;
      let rec v3 acc k =
        let j = a'.{k} in
        let l = a.{j - 1} in
        if l << k then (
          for j = j to k - 1 do
            let l = a.{j + 1} in
            a.{j} <- l;
            a'.{l} <- j
          done;
          a.{k} <- k;
          a'.{k} <- k;
          let k = k - 1 in
          if k > 0 then v3 acc k else acc )
        else (
          a.{j - 1} <- k;
          a.{j} <- l;
          a'.{k} <- j - 1;
          a'.{l} <- j;
          v3 (f acc elem) n )
      in
      v3 (f init elem) n

    let iter = `Define_using_fold

    let length = `Define_using_fold
  end)

  module Of_list = struct
    type 'a t = 'a list * ('a -> 'a -> bool)

    include Build.Make (struct
      type nonrec 'a t = 'a t

      type 'a elt = 'a list

      let fold (l, cmp) ~init ~f =
        let n = List.length l in
        let elems = List.to_array l in
        let cmp i i' = cmp elems.(i) elems.(i') in
        fold (create n cmp) ~init ~f:(fun x (a, _) ->
            f x (List.init n ~f:(fun i -> elems.(a.{i}))))

      let iter = `Define_using_fold

      let length = `Define_using_fold
    end)

    let create l cmp = (l, cmp)
  end
end

let%expect_test "sorted_permutations_iter" =
  SortedPermutation.iter
    (4, fun x y -> match (x, y) with 1, 3 | 2, 3 | 2, 4 -> true | _ -> false)
    ~f:(fun (c, _) ->
      Stdio.print_endline (Sexp.to_string_hum ([%sexp_of: int array] (to_array c))));
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
    | 2, (3 | 5 | 6 | 8 | 9)
    | 3, (6 | 9)
    | 4, (5 | 6 | 7 | 8 | 9)
    | 5, (6 | 8 | 9)
    | 6, 9
    | 7, (8 | 9)
    | 8, 9 ->
        true
    | _ -> false
  in
  SortedPermutation.iter (9, young_tableaux) ~f:(fun (_, c) ->
      Stdio.print_endline (Sexp.to_string_hum ([%sexp_of: int array] (to_array c))));
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

module RestrictedPermutation = struct
  type t = int * (int_array -> bool)

  include Container.Make0 (struct
    type nonrec t = t

    module Elt = struct
      type t = int_array

      let equal = Util.equal
    end

    let fold (n, t) ~init ~f =
      let a = Bigarray.(Array1.create int c_layout (n + 1)) in
      let l = Bigarray.(Array1.create int c_layout (n + 1)) in
      let u = Bigarray.(Array1.create int c_layout (n + 1)) in
      let t_args = Array.create ~len:(n + 1) (Bigarray.Array1.sub a 1 1) in
      for k = 1 to n do
        t_args.(k) <- Bigarray.Array1.sub a 1 k
      done;
      let elem = Bigarray.Array1.sub a 1 n in
      for k = 0 to n - 1 do
        l.{k} <- k + 1
      done;
      l.{n} <- 0;
      let rec x3 acc k p q =
        a.{k} <- q;
        if t t_args.(k) then
          if k = n then x6 (f acc elem) k
          else (
            u.{k} <- p;
            l.{p} <- l.{q};
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
          l.{p} <- q;
          let p = q in
          let q = l.{p} in
          if q = 0 then x6 acc k else x3 acc k p q
      in
      x3 init 1 0 l.{0}

    let iter = `Define_using_fold

    let length = `Define_using_fold
  end)

  let create n f = (n, f)

  module Of_list = struct
    type 'a t = 'a list * ('a list -> bool)

    include Build.Make (struct
      type nonrec 'a t = 'a t

      type 'a elt = 'a list

      let fold (l, filter) ~init ~f =
        let n = List.length l in
        let elems = List.to_array l in
        let filter a = filter (List.init n ~f:(fun i -> elems.(a.{i}))) in
        fold (create n filter) ~init ~f:(fun x a ->
            f x (List.init n ~f:(fun i -> elems.(a.{i}))))

      let iter = `Define_using_fold

      let length = `Define_using_fold
    end)

    let create l f = (l, f)
  end
end

let%expect_test "restricted_permutations_iter" =
  RestrictedPermutation.iter
    ( 4,
      fun a ->
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
      Stdio.print_endline (Sexp.to_string_hum ([%sexp_of: int array] (to_array c))));
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
  type t = { n : int; k : int }

  include Container.Make0 (struct
    type nonrec t = t

    module Elt = struct
      type t = int_array

      let equal = Util.equal
    end

    open Unsafe

    let rec loop c j =
      c.{j - 1} <- j - 2;
      if c.{j} + 1 = c.{j + 1} then
        let j = j + 1 in
        loop c j
      else j

    let rec t2 c c' f t acc x j =
      let acc = f acc c' in
      if j > 0 then (
        let x = j in
        c.{j} <- x;
        let j = j - 1 in
        t2 c c' f t acc x j )
      else if c.{1} + 1 < c.{2} then (
        c.{1} <- c.{1} + 1;
        t2 c c' f t acc x j )
      else
        let j = 2 in
        let j = loop c j in
        let x = c.{j} + 1 in
        if j <= t then (
          c.{j} <- x;
          let j = j - 1 in
          t2 c c' f t acc x j )
        else acc

    let fold { n; k = t } ~init ~f =
      assert (t >= 0 && t <= n);
      if t = 0 then init
      else if t = n then
        f init
          (Array.init n ~f:(fun i -> i) |> Bigarray.(Array1.of_array int c_layout))
      else
        let c = Bigarray.(Array1.create int c_layout (t + 3)) in
        for i = 1 to t do
          c.{i} <- i - 1
        done;
        c.{t + 1} <- n;
        c.{t + 2} <- 0;
        let j = t in
        let x = 0 in
        let c' = Array1.sub c 1 t in
        t2 c c' f t init x j

    let iter = `Define_using_fold

    let length =
      let length { n; k } = binom (n + k - 1) k in
      `Custom length
  end)

  module Of_list = Build.Make (struct
    type 'a t = 'a list * int

    type 'a elt = 'a list

    let fold (l, k) ~init ~f =
      let n = List.length l in
      let elems = List.to_array l in
      fold { n; k } ~init ~f:(fun x a ->
          f x (List.init n ~f:(fun i -> elems.(a.{i}))))

    let iter = `Define_using_fold

    let length =
      let length (l, k) = length { n = List.length l; k } in
      `Custom length
  end)
end

let%expect_test "combinations" =
  Combination.iter { k = 3; n = 5 } ~f:(fun c ->
      Stdio.print_endline (Sexp.to_string_hum ([%sexp_of: int array] (to_array c))));
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
