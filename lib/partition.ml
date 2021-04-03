open! Base

type args = { a : Int_array.t; a' : Int_array.t; f : Int_array.t -> unit; m : int }

let rec loop2 a x s j =
  if j > 1 then (
    a.{j} <- x;
    loop2 a x (s - x) (j - 1) )
  else s

let rec loop1 ({ a; _ } as args) a1m s j : unit =
  let aj = a.{j} in
  if aj >= a1m then loop1 args a1m (s + aj) (j + 1) else h3 args s j

and h2 ({ a; a'; f; _ } as args) : unit =
  f a';
  let a1m = a.{1} - 1 and a2 = a.{2} in
  if a2 >= a1m then loop1 args a1m (a1m + a2) 3
  else (
    a.{1} <- a1m;
    a.{2} <- a2 + 1;
    h2 args )

and h3 ({ a; m; _ } as args) s j : unit =
  if j <= m then (
    let x = a.{j} + 1 in
    a.{j} <- x;
    a.{1} <- loop2 a x s (j - 1);
    h2 args )

let iter ~n ~k:m f =
  let open Bigarray in
  let open Array1 in
  if n < m || (n > 0 && m = 0) then ()
  else if n = 0 then f ((of_array int c_layout) [||])
  else if m = 1 then f ((of_array int c_layout) [| n |])
  else
    let a = create int c_layout (m + 2) in
    let a' = sub a 1 m in
    a.{1} <- n - m + 1;
    for i = 2 to m do
      a.{i} <- 1
    done;
    a.{m + 1} <- -1;
    h2 { a; a'; f; m }

let iter_with_zeros ~n ~k:m f =
  let open Bigarray in
  let open Array1 in
  let arr = create int c_layout m in
  let f c =
    fill arr 0;
    blit c (sub arr 0 (dim c));
    f arr
  in
  for k' = 0 to m do
    iter ~n ~k:k' f
  done
