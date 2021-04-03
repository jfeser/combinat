module Int_array = Int_array

type +'a iter = ('a -> unit) -> unit

let combinations = Combination.iter

let permutations = Permutation.iter

let permutations_ordered = Permutation.iter_ordered

let permutations_filtered = Permutation.iter_filtered

let partitions = Partition.iter

let partitions_with_zeros = Partition.iter_with_zeros

let compositions = Composition.iter

let using_set _ = failwith ""

let using_list _ = failwith ""

let subsets ~n:_ ~k:_ = failwith ""
