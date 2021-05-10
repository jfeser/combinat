type args = { c : int array; f : int array -> unit; t : int }

let rec loop ({ c; t; _ } as args) j =
  c.(j - 1) <- j - 2;
  if c.(j) + 1 = c.(j + 1) then loop args (j + 1)
  else if j <= t then (
    c.(j) <- c.(j) + 1;
    t2 args (j - 1))

and t2 ({ c; f; _ } as args) j =
  f c;
  if j > 0 then (
    c.(j) <- j;
    t2 args (j - 1))
  else t3 args

and t3 ({ c; f; _ } as args) =
  if c.(1) + 1 < c.(2) then (
    c.(1) <- c.(2) - 1;
    f c);
  loop args 2

let iter elems ~k f =
  let n = List.length elems in
  if k < 0 then raise_s [%message "combination: expected k >= 0" (k : int)];
  if k > n then raise_s [%message "combination: expected k < n" (k : int) (n : int)];

  let elems = Array.of_list elems in
  let output = Array.sub elems ~pos:0 ~len:k in
  let f a =
    for i = 1 to k - 1 do
      output.(i) <- elems.(a.(i))
    done;
    f output
  in

  if k = 0 then ()
  else if k = n then f (Array.init n ~f:(fun i -> i))
  else
    let c = Array.create ~len:(k + 3) 0 in
    for i = 1 to k do
      c.(i) <- i - 1
    done;
    c.(k + 1) <- n;
    c.(k + 2) <- 0;
    t2 { c; f; t = k } k
