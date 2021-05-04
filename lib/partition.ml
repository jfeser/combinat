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

module type S = sig
  type t

  val create : n:int -> parts:int -> t
  (** Create a partition of an integer {i n} into {i parts} parts. *)

  include Container.S0 with type t := t and type elt := int_array

  val to_list : t -> int list list
end

module T = struct
  type t = int * int

  let create ~n ~parts = (n, parts)
end

module Default = struct
  include T

  module Elt = struct
    type t = int_array

    let equal = equal
  end

  open Unsafe

  type 'a args = {
    a : int_array;
    a' : int_array;
    f : 'a -> int_array -> 'a;
    m : int;
  }

  let rec loop2 a x s j =
    if j > 1 then (
      a.{j} <- x;
      loop2 a x (s - x) (j - 1))
    else s

  let rec loop1 ({ a; _ } as args) acc a1m s j =
    let aj = a.{j} in
    if aj >= a1m then loop1 args acc a1m (s + aj) (j + 1) else h3 args acc s j

  and h2 ({ a; a'; f; _ } as args) acc =
    let acc = f acc a' and a1m = a.{1} - 1 and a2 = a.{2} in
    if a2 >= a1m then loop1 args acc a1m (a1m + a2) 3
    else (
      a.{1} <- a1m;
      a.{2} <- a2 + 1;
      h2 args acc)

  and h3 ({ a; m; _ } as args) acc s j =
    if j <= m then (
      let x = a.{j} + 1 in
      a.{j} <- x;
      a.{1} <- loop2 a x s (j - 1);
      h2 args acc)
    else acc

  let fold (n, m) ~init ~f =
    let module A = Bigarray.Array1 in
    let of_array = A.of_array Bigarray.int Bigarray.c_layout in
    if n < m || (n > 0 && m = 0) then init
    else if n = 0 then f init (of_array [||])
    else if m = 1 then f init (of_array [| n |])
    else
      let a = A.create Bigarray.int Bigarray.c_layout (m + 2) in
      let a' = A.sub a 1 m in
      a.{1} <- n - m + 1;
      for i = 2 to m do
        a.{i} <- 1
      done;
      a.{m + 1} <- -1;
      h2 { a; a'; f; m } init

  let iter = `Define_using_fold

  let length = `Define_using_fold
end

include T
include Container.Make0 (Default)

let to_list p =
  fold p ~init:[] ~f:(fun l x ->
      List.init (Bigarray.Array1.dim x) ~f:(fun i -> x.{i}) :: l)

module With_zeros = struct
  include T

  include Container.Make0 (struct
    include Default

    let orig_fold = fold

    let fold (n, m) ~init ~f =
      let module A = Bigarray.Array1 in
      let arr = A.create Bigarray.int Bigarray.c_layout m in
      let f acc c =
        A.fill arr 0;
        A.blit c (A.sub arr 0 (A.dim c));
        f acc arr
      in
      let rec fold m' acc =
        if m < m' then acc else fold (m' + 1) (orig_fold (n, m') ~init:acc ~f)
      in
      fold 0 init
  end)

  let to_list p =
    fold p ~init:[] ~f:(fun l x ->
        List.init (Bigarray.Array1.dim x) ~f:(fun i -> x.{i}) :: l)
end
