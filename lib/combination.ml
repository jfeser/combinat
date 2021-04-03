open! Base

type args = { c : Int_array.t; c' : Int_array.t; f : Int_array.t -> unit; t : int }

let rec loop ({ c; t; _ } as args) j =
  c.{j - 1} <- j - 2;
  if c.{j} + 1 = c.{j + 1} then loop args (j + 1)
  else if j <= t then (
    c.{j} <- c.{j} + 1;
    t2 args (j - 1) )

and t2 ({ c; c'; f; _ } as args) j =
  f c';
  if j > 0 then (
    c.{j} <- j;
    t2 args (j - 1) )
  else t3 args

and t3 ({ c; c'; f; _ } as args) =
  if c.{1} + 1 < c.{2} then (
    c.{1} <- c.{2} - 1;
    f c' );
  loop args 2

let iter ~n ~k f =
  if k < 0 then raise_s [%message "combination: expected k >= 0" (k : int)];
  if k > n then raise_s [%message "combination: expected k < n" (k : int) (n : int)];

  let open Bigarray in
  let open Array1 in
  if k = 0 then ()
  else if k = n then f (Array.init n ~f:(fun i -> i) |> of_array int c_layout)
  else
    let c = create int c_layout (k + 3) in
    for i = 1 to k do
      c.{i} <- i - 1
    done;
    c.{k + 1} <- n;
    c.{k + 2} <- 0;
    let c' = sub c 1 k in
    t2 { c; c'; f; t = k } k
