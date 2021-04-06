open! Base

type args = { a : int array; a' : int array; f : int array -> unit; m : int }

let rec loop2 a x s j =
  if j > 1 then (
    a.(j) <- x;
    loop2 a x (s - x) (j - 1) )
  else s

let rec loop1 ({ a; _ } as args) a1m s j : unit =
  let aj = a.(j) in
  if aj >= a1m then loop1 args a1m (s + aj) (j + 1) else h3 args s j

and h2 ({ a; a'; f; m } as args) : unit =
  Array.blit ~src:a ~src_pos:1 ~dst:a' ~dst_pos:0 ~len:m;
  f a';
  let a1m = a.(1) - 1 and a2 = a.(2) in
  if a2 >= a1m then loop1 args a1m (a1m + a2) 3
  else (
    a.(1) <- a1m;
    a.(2) <- a2 + 1;
    h2 args )

and h3 ({ a; m; _ } as args) s j : unit =
  if j <= m then (
    let x = a.(j) + 1 in
    a.(j) <- x;
    a.(1) <- loop2 a x s (j - 1);
    h2 args )

let iter ~n ~k:m f =
  if n < m || (n > 0 && m = 0) then ()
  else if n = 0 then f [||]
  else if m = 1 then f [| n |]
  else
    let a = Array.create ~len:(m + 2) 0 in
    let a' = Array.create ~len:m 0 in
    a.(1) <- n - m + 1;
    for i = 2 to m do
      a.(i) <- 1
    done;
    a.(m + 1) <- -1;
    h2 { a; a'; f; m }

let iter_with_zeros ~n ~k:m f =
  let output = Array.create ~len:m 0 in
  let f c =
    Array.blit ~src:c ~src_pos:0 ~dst:output ~dst_pos:0 ~len:(Array.length c);
    f output
  in
  for k' = 0 to m do
    iter ~n ~k:k' f
  done
