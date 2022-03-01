type +'a iter = ('a -> unit) -> unit

let combinations = Combination.iter

let permutations = Permutation.iter

let sequences = Sequence.iter

let sequences_restricted = Sequence.iter_restricted

let permutations_ordered _ = assert false

let permutations_filtered _ = assert false

let partitions = Partition.iter

let partitions_with_zeros = Partition.iter_with_zeros

let compositions = Composition.iter

let to_list s =
  let q = Queue.create () in
  s (fun a -> Queue.enqueue q @@ Array.copy a);
  Queue.to_list q

let random ?(state = Random.State.default) ?(k = 1) seq =
  let res = Option_array.create ~len:k and i = ref 0 in
  seq (fun x ->
      (if !i < k then Option_array.set_some res !i x
      else
        let j = Random.State.int_incl state 0 (!i - 1) in
        if j < k then Option_array.set_some res j x);
      Int.incr i);
  List.init k ~f:(Option_array.get_some_exn res)
