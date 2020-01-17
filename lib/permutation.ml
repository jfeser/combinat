open! Base
open! Shared

module T = struct
  type t = int

  let create x = x
end

include T

include Container.Make0 (struct
  include T

  module Elt = struct
    type t = int_array

    let equal = equal
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

module Sorted = struct
  type t = int * (int -> int -> bool)

  let create n compare = (n, compare)

  include Container.Make0 (struct
    type nonrec t = t

    module Elt = struct
      type t = int_array * int_array

      let equal (a1, a2) (b1, b2) = equal a1 b1 && equal a2 b2
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

module Restricted = struct
  type t = int * (int_array -> bool)

  include Container.Make0 (struct
    type nonrec t = t

    module Elt = struct
      type t = int_array

      let equal = equal
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
