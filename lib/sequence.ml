let rec inc a n f =
  let k = Array.length a in
  f a;
  a.(k - 1) <- a.(k - 1) + 1;
  if a.(k - 1) >= n then carry a n f (k - 1) else inc a n f

and carry a n f j =
  if j > 0 then (
    a.(j) <- 0;
    a.(j - 1) <- a.(j - 1) + 1;
    if a.(j - 1) >= n then carry a n f (j - 1) else inc a n f)

let iter elems ~k f =
  let n = List.length elems in
  if k < 0 then raise_s [%message "sequences: expected k >= 0" (k : int)];
  if k = 0 then f [||]
  else (
    if n = 0 then raise_s [%message "sequences: expected n > 0" (n : int)];
    let elems = Array.of_list elems in
    let a = Array.create ~len:k 0 in
    let output = Array.create ~len:k elems.(0) in
    let f a =
      for i = 0 to k - 1 do
        output.(i) <- elems.(a.(i))
      done;
      f output
    in
    inc a n f)
