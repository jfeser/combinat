type args = { c : int array; mutable x : int; f : int array -> unit; t : int }

module Algorithm_t = struct
  let rec init ~n ~t f =
    let c =
      Array.init (t + 3) (fun j ->
          if 1 <= j && j <= t then j - 1 else if j = t + 1 then n else 0)
    in
    let args = { c; x = 0; f; t } in
    visit args t

  and visit args j =
    args.f @@ Array.copy args.c;
    if j > 0 then (
      args.x <- j;
      increase args j)
    else easy args j

  and easy args j =
    let c = args.c in
    if c.(1) + 1 < c.(2) then (
      c.(1) <- c.(1) + 1;
      visit args j);
    find args 2

  and find args j =
    let c = args.c in
    c.(j - 1) <- j - 2;
    args.x <- c.(j) + 1;

    let j = ref j in
    while args.x = c.(!j + 1) do
      j := !j + 1
    done;
    let j = !j in

    done_ args j

  and done_ args j = if j > args.t then () else increase args j

  and increase args j =
    args.c.(j) <- args.x;
    visit args (j - 1)
end

module Algorithm_l = struct
  let rec init ~n ~t f =
    let c =
      Array.init (t + 3) (fun j ->
          if 1 <= j && j <= t then j - 1 else if j = t + 1 then n else 0)
    in
    let args = { c; x = 0; f; t } in
    visit args

  and visit args =
    args.f @@ Array.copy args.c;
    find args

  and find args =
    let c = args.c in
    let j = ref 1 in
    while c.(!j) + 1 = c.(!j + 1) do
      c.(!j) <- !j - 1;
      j := !j + 1
    done;
    done_ args !j

  and done_ args j = if j > args.t then () else increase args j

  and increase args j =
    args.c.(j) <- args.c.(j) + 1;
    visit args
end

let iter elems ~k:t f =
  let n = List.length elems in
  if t < 0 then failwith (Printf.sprintf "combination: expected k >= 0, got %d" t);
  if t > n then
    failwith (Printf.sprintf "combination: expected k < n, got k=%d n=%d" t n);

  let elems = Array.of_list elems in
  let output = Array.sub elems 0 t in
  let f a =
    for i = 0 to t - 1 do
      output.(i) <- elems.(a.(i + 1))
    done;
    f @@ Array.copy output
  in

  if t = 0 then f [||]
  else if t = n then f (Array.init (n + 1) (fun i -> i - 1))
  else Algorithm_l.init ~n ~t f
