open! Base
open! Shared

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
      t2 args acc (j - 1) )
    else acc

  and t2 ({ c; c'; f; _ } as args) acc j =
    let acc = f acc c' in
    if j > 0 then (
      c.{j} <- j;
      t2 args acc (j - 1) )
    else t3 args acc

  and t3 ({ c; c'; f; _ } as args) acc =
    let acc =
      if c.{1} + 1 < c.{2} then (
        c.{1} <- c.{2} - 1;
        f acc c' )
      else acc
    in
    loop args acc 2

  let fold { n; k = t } ~init ~f =
    let open Bigarray in
    let open Array1 in
    assert (t >= 0 && t <= n);
    if t = 0 then init
    else if t = n then f init (Array.init n ~f:(fun i -> i) |> of_array int c_layout)
    else
      let c = create int c_layout (t + 3) in
      for i = 1 to t do
        c.{i} <- i - 1
      done;
      c.{t + 1} <- n;
      c.{t + 2} <- 0;
      let j = t in
      let c' = sub c 1 t in
      t2 { c; c'; f; t } init j

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
    fold { n; k } ~init ~f:(fun x a -> f x (List.init k ~f:(fun i -> elems.(a.{i}))))

  let iter = `Define_using_fold

  let length =
    let length (l, k) = length { n = List.length l; k } in
    `Custom length
end)
