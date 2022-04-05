type +'a iter = ('a -> unit) -> unit

let combinations = Combination.iter
let permutations = Permutation.iter
let sequences = Sequence.iter
let sequences_restricted = Sequence.iter_restricted
let partitions = Partition.iter
let partitions_with_zeros = Partition.iter_with_zeros
let compositions = Composition.iter
