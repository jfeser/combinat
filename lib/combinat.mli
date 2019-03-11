open Base

type int_array = (int, Bigarray.int_elt, Bigarray.c_layout) Bigarray.Array1.t

(** Compute the partitions of an integer {i n} into {i m} parts. See
    (Knuth 3b, pg. 2). *)
module Partition :
  Container.S0 with type t := int * int and type elt := int_array

(** Compute the partitions of an integer {i n} into {i m} parts,
    including partitions where some elements are zero. *)
module Partition_with_zeros :
  Container.S0 with type t := int * int and type elt := int_array

(** Compute the unique permutations of an array. See (Knuth 2b, pg. 1). *)
module Permutation :
  Container.S0 with type t := int array and type elt := int_array

module SortedPermutation :
  Container.S0
  with type t := int * (int -> int -> bool)
   and type elt := int_array * int_array

module RestrictedPermutation :
  Container.S0
  with type t := int * (int_array -> bool)
   and type elt := int_array

(** Compute all of the {i t} combinations of the numbers in [0, {i n}]. *)
module Combination :
  Container.S0 with type t := int * int and type elt := int_array
