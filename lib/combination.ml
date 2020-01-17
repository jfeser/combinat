open! Base
open! Shared

type t = { n : int; k : int }

include Container.Make0 (struct
  type nonrec t = t

  module Elt = struct
    type t = int_array

    let equal = equal
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
      let x = 0 in
      let c' = sub c 1 t in
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
    fold { n; k } ~init ~f:(fun x a -> f x (List.init n ~f:(fun i -> elems.(a.{i}))))

  let iter = `Define_using_fold

  let length =
    let length (l, k) = length { n = List.length l; k } in
    `Custom length
end)
