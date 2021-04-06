type +'a iter = ('a -> unit) -> unit

let combinations = Combination.iter

let permutations = Permutation.iter

let permutations_ordered _ = failwith ""

(* Permutation.iter_ordered *)

let permutations_filtered _ = failwith ""

(* Permutation.iter_filtered *)

let partitions = Partition.iter

let partitions_with_zeros = Partition.iter_with_zeros

let compositions = Composition.iter

let subsets ~n:_ ~k:_ = failwith ""
