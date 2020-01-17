open Base

type int_array = (int, Bigarray.int_elt, Bigarray.c_layout) Bigarray.Array1.t

module type PARTITION = sig
  type t = int * int

  val create : n:int -> parts:int -> t

  include Container.S0 with type t := t and type elt := int_array
end

module Partition : PARTITION
(** Compute the partitions of an integer {i n} into {i m} parts. See
    (Knuth 3b, pg. 2). *)

module Partition_with_zeros : PARTITION
(** Compute the partitions of an integer {i n} into {i m} parts,
    including partitions where some elements are zero. *)

(** Compute the unique permutations of an array. See (Knuth 2b, pg. 1). *)
module Permutation : sig
  type t = int

  val create : int -> t

  include Container.S0 with type t := t and type elt := int_array

  module Of_list : sig
    type 'a t

    val create : 'a list -> 'a t

    include Container.Generic with type 'a t := 'a t and type 'a elt := 'a list
  end
end

module SortedPermutation : sig
  type t

  val create : int -> (int -> int -> bool) -> t

  include Container.S0 with type t := t and type elt := int_array * int_array

  module Of_list : sig
    type 'a t

    val create : 'a list -> ('a -> 'a -> bool) -> 'a t

    include Container.Generic with type 'a t := 'a t and type 'a elt := 'a list
  end
end

module RestrictedPermutation : sig
  type t

  val create : int -> (int_array -> bool) -> t

  include Container.S0 with type t := t and type elt := int_array

  module Of_list : sig
    type 'a t

    val create : 'a list -> ('a list -> bool) -> 'a t

    include Container.Generic with type 'a t := 'a t and type 'a elt := 'a list
  end
end

(** Compute all of the {i t} combinations of the numbers in [0, {i n}]. *)
module Combination : sig
  type t = { n : int; k : int }

  include Container.S0 with type t := t and type elt := int_array

  module Of_list :
    Container.Generic with type 'a t := 'a list * int and type 'a elt := 'a list
end
