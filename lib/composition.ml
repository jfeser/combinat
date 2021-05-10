let rec iter2 f i n p =
  if i >= n then ()
  else (
    p.(0) <- i;
    p.(1) <- n - i;
    f p;
    iter2 f (i + 1) n p)

let iter ~n ~k f =
  if n < k then ()
  else
    let p = Array.create ~len:k 0 in
    if k = 0 && n = 0 then f p
    else if k = 1 then (
      p.(0) <- n;
      f p)
    else if k = 2 then iter2 f 1 n p
    else
      let elems = List.init (n - 1) ~f:Fn.id in
      Combination.iter elems ~k:(k - 1) @@ fun c ->
      p.(0) <- c.(0) + 1;
      for i = 1 to k - 2 do
        p.(i) <- c.(i) - c.(i - 1)
      done;
      p.(k - 1) <- n - 1 - c.(k - 2);
      f p
