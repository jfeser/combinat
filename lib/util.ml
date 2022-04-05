let create_permutation a p =
  let n = Array.length a in
  assert (n = Array.length p);
  Array.init n (fun i -> a.(p.(i)))
