type +'a iter = ('a -> unit) -> unit

let combinations = Combination.iter

let permutations = Permutation.iter

let sequences = Sequence.iter

let sequences_restricted = Sequence.iter_restricted

let permutations_ordered _ = assert false

(* Permutation.iter_ordered *)

let permutations_filtered _ = assert false

(* Permutation.iter_filtered *)

let partitions = Partition.iter

let partitions_with_zeros = Partition.iter_with_zeros

let compositions = Composition.iter

let subsets _ ~k:_ = failwith ""

let to_list s =
  let q = Queue.create () in
  s (fun a -> Queue.enqueue q @@ Array.copy a);
  Queue.to_list q
