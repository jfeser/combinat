open! Base
open! Shared

module type S = sig
  type t = int * int

  val create : n:int -> parts:int -> t

  include Container.S0 with type t := t and type elt := int_array
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
    let open Bigarray in
    let open Array1 in
    if n < m || m = 0 then init
    else if m = 1 then f init ((of_array int c_layout) [| n |])
    else
      let a = create int c_layout (m + 2) in
      let a' = sub a 1 m in
      a.{1} <- n - m + 1;
      for i = 2 to m do
        a.{i} <- 1
      done;
      a.{m + 1} <- -1;
      h2 a a' f m init

  let iter = `Define_using_fold

  let length = `Define_using_fold
end

include T
include Container.Make0 (Default)

module With_zeros = struct
  include T

  include Container.Make0 (struct
    include Default

    let orig_fold = fold

    let fold (n, m) ~init ~f =
      let open Bigarray in
      let open Array1 in
      let arr = create int c_layout m in
      let f acc c =
        fill arr 0;
        blit c (sub arr 0 (dim c));
        f acc arr
      in
      let rec fold m' acc =
        if m < m' then acc else fold (m' + 1) (orig_fold (n, m') ~init:acc ~f)
      in
      fold 0 init
  end)
end
