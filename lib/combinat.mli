open Base

(** Internal iterators for combinatorial objects. *)

type int_array = (int, Bigarray.int_elt, Bigarray.c_layout) Bigarray.Array1.t

(** Iterators for integer partitions. *)
module Partition : sig
  include Partition.S

  module With_zeros : Partition.S
  (** Compute the partitions of an integer {i n} into {i m} parts,
      including partitions where some elements are zero. *)
end

(** Iterators for permutations. *)
module Permutation : sig
  type t

  val create : int -> t
  (** Create a new permutation of {i n} elements.*)

  include Container.S0 with type t := t and type elt := int_array

  module Of_list : sig
    type 'a t

    val create : 'a list -> 'a t
    (** Create a new permutation of a list.*)

    include Container.Generic with type 'a t := 'a t and type 'a elt := 'a list
  end

  module Sorted : sig
    type t

    val create : int -> (int -> int -> bool) -> t
    (** Create a new permutation of {i n} elements ordered by a less-than relation. *)

    include Container.S0 with type t := t and type elt := int_array * int_array

    module Of_list : sig
      type 'a t

      val create : 'a list -> ('a -> 'a -> bool) -> 'a t
      (** Create a new permutation of a list ordered by a less-than relation. *)

      include Container.Generic with type 'a t := 'a t and type 'a elt := 'a list
    end
  end

  module Restricted : sig
    type t

    val create : int -> (int_array -> bool) -> t
    (** Create a new permutation of {i n} elements that is filtered by a
    function. Only orderings with a prefix that the function allows will be
    enumerated. *)

    include Container.S0 with type t := t and type elt := int_array

    module Of_list : sig
      type 'a t

      val create : 'a list -> ('a list -> bool) -> 'a t
      (** Create a new permutation of a list that is filtered by a function. *)

      include Container.Generic with type 'a t := 'a t and type 'a elt := 'a list
    end
  end
end

(** Iterators for combinations. *)
module Combination : sig
  type t

  val create : n:int -> k:int -> t
  (** Create a {i k}-combination of {i n} elements. *)

  include Container.S0 with type t := t and type elt := int_array

  module Of_list : sig
    type 'a t

    val create : 'a list -> int -> 'a t
    (** Create a {i k}-combination of a list. *)

    include Container.Generic with type 'a t := 'a t and type 'a elt := 'a list
  end
end
