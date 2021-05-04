open! Base
open! Shared
open Printf

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

type t = { n : int; k : int }

let create ~n ~k =
  if k < 0 then failwith @@ sprintf "Combination: expected k >= 0, got k = %d" k
  else if k > n then
    failwith @@ sprintf "Combination: expected k < n, got k = %d and n = %d" k n
  else { n; k }

include Container.Make0 (struct
  type nonrec t = t

  module Elt = struct
    type t = int_array

    let equal = equal
  end

  open Unsafe

  type 'a args = {
    c : int_array;
    c' : int_array;
    f : 'a -> int_array -> 'a;
    t : int;
  }

  let rec loop ({ c; t; _ } as args) acc j =
    c.{j - 1} <- j - 2;
    if c.{j} + 1 = c.{j + 1} then loop args acc (j + 1)
    else if j <= t then (
      c.{j} <- c.{j} + 1;
      t2 args acc (j - 1))
    else acc

  and t2 ({ c; c'; f; _ } as args) acc j =
    let acc = f acc c' in
    if j > 0 then (
      c.{j} <- j;
      t2 args acc (j - 1))
    else t3 args acc

  and t3 ({ c; c'; f; _ } as args) acc =
    let acc =
      if c.{1} + 1 < c.{2} then (
        c.{1} <- c.{2} - 1;
        f acc c')
      else acc
    in
    loop args acc 2

  let fold { n; k = t } ~init ~f =
    let int = Bigarray.int and c_layout = Bigarray.c_layout in
    let module A = Bigarray.Array1 in
    if t = 0 then init
    else if t = n then
      f init (Array.init n ~f:(fun i -> i) |> A.of_array int c_layout)
    else
      let c = A.create int c_layout (t + 3) in
      for i = 1 to t do
        c.{i} <- i - 1
      done;
      c.{t + 1} <- n;
      c.{t + 2} <- 0;
      let j = t in
      let c' = A.sub c 1 t in
      t2 { c; c'; f; t } init j

  let iter = `Define_using_fold

  let length =
    let length { n; k } = binom (n + k - 1) k in
    `Custom length
end)

module Of_list = struct
  type 'a t = 'a list * int

  include Build.Make (struct
    type nonrec 'a t = 'a t

    type 'a elt = 'a list

    let fold (l, k) ~init ~f =
      let n = List.length l in
      let elems = List.to_array l in
      create ~n ~k
      |> fold ~init ~f:(fun x a -> f x (List.init k ~f:(fun i -> elems.(a.{i}))))

    let iter = `Define_using_fold

    let length =
      let length (l, k) = length (create ~n:(List.length l) ~k) in
      `Custom length
  end)

  let create l k = (l, k)
end
