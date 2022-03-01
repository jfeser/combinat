open! Base

let create_permutation a p =
  let n = Array.length a in
  assert (n = Array.length p);
  Array.init n ~f:(fun i -> a.(p.(i)))
