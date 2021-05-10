open! Base

type 'a args = { a : int array; f : int array -> unit; n : int }

let[@inline] swap a x y =
  let tmp = a.(x) in
  a.(x) <- a.(y);
  a.(y) <- tmp

let rec loop1 a j = if a.(j) >= a.(j + 1) then loop1 a (j - 1) else j

let rec loop2 a j l = if a.(j) >= a.(l) then loop2 a j (l - 1) else l

let rec loop3 a k l =
  if k < l then (
    swap a k l;
    loop3 a (k + 1) (l - 1))

let rec l1 ({ a; f; n } as args) =
  f a;
  let j = loop1 a (n - 1) in
  if j > 0 then (
    let l = loop2 a j n in
    swap a j l;
    loop3 a (j + 1) n;
    l1 args)

let iter elems f =
  let n = List.length elems in
  let elems = Array.of_list elems in
  let a = Array.init ~f:(fun i -> if i = 0 then n - 2 else i - 1) (n + 1) in
  let output = Array.copy elems in
  let f a =
    for i = 0 to n - 1 do
      output.(i) <- elems.(a.(i + 1))
    done;
    f output
  in
  if Array.is_empty elems then f [||] else l1 { a; f; n }

(* let iter_ordered ~n ~lt f =
 *   let ( << ) x y =
 *     match (x, y) with 0, 0 -> false | 0, _ -> true | x, y -> lt x y
 *   in
 *   let a = Array.init (n + 1) ~f:Fn.id in
 *   let a' = Array.copy a in
 *   for j = 0 to n do
 *     a.(j) <- j;
 *     a'.(j) <- j
 *   done;
 *   let rec v3 k =
 *     let j = a'.(k) in
 *     let l = a.(j - 1) in
 *     if l << k then (
 *       for j = j to k - 1 do
 *         let l = a.(j + 1) in
 *         a.(j) <- l;
 *         a'.(l) <- j
 *       done;
 *       a.(k) <- k;
 *       a'.(k) <- k;
 *       let k = k - 1 in
 *       if k > 0 then v3 k )
 *     else (
 *       a.(j - 1) <- k;
 *       a.(j) <- l;
 *       a'.(k) <- j - 1;
 *       a'.(l) <- j;
 *       f [||];
 *       v3 n )
 *   in
 *   f [||];
 *   v3 n
 * 
 * let iter_filtered ~n ~f:t f =
 *   let a = Bigarray.(Array1.create int c_layout (n + 1)) in
 *   let l = Bigarray.(Array1.create int c_layout (n + 1)) in
 *   let u = Bigarray.(Array1.create int c_layout (n + 1)) in
 *   let t_args = Array.create ~len:(n + 1) (Bigarray.Array1.sub a 1 1) in
 *   for k = 1 to n do
 *     t_args.(k) <- Bigarray.Array1.sub a 1 k
 *   done;
 *   let elem = Bigarray.Array1.sub a 1 n in
 *   for k = 0 to n - 1 do
 *     l.(k) <- k + 1
 *   done;
 *   l.(n) <- 0;
 *   let rec x3 k p q =
 *     a.(k) <- q;
 *     if t t_args.(k) then
 *       if k = n then (
 *         f elem;
 *         x6 k )
 *       else (
 *         u.(k) <- p;
 *         l.(p) <- l.(q);
 *         x3 (k + 1) 0 l.(0) )
 *     else
 *       let p = q in
 *       let q = l.(p) in
 *       if q = 0 then x6 k else x3 k p q
 *   and x6 k =
 *     let k = k - 1 in
 *     if k = 0 then ()
 *     else
 *       let p = u.(k) in
 *       let q = a.(k) in
 *       l.(p) <- q;
 *       let p = q in
 *       let q = l.(p) in
 *       if q = 0 then x6 k else x3 k p q
 *   in
 *   x3 1 0 l.(0) *)
