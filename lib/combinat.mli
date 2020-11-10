open Base

(** Internal iterators for combinatorial objects. *)

module Int_array : sig
  type t [@@deriving sexp_of]

  val length : t -> int

  val get : t -> int -> int
end

(** Iterators for integer partitions. *)
module Partition : sig
  type t

  val create : n:int -> parts:int -> t
  (** Create a partition of an integer {i n} into {i parts} parts. *)

  include Container.S0 with type t := t and type elt := Int_array.t

  val to_list : t -> int list list

  (** Compute the partitions of an integer {i n} into {i m} parts,
      including partitions where some elements are zero. *)
  module With_zeros : sig
    type t

    val create : n:int -> parts:int -> t
    (** Create a partition of an integer {i n} into {i parts} parts. *)

    include Container.S0 with type t := t and type elt := Int_array.t

    val to_list : t -> int list list
  end
end

(** Iterators for permutations. *)
module Permutation : sig
  type t

  val create : int -> t
  (** Create a new permutation of {i n} elements.*)

  include Container.S0 with type t := t and type elt := Int_array.t

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

    include Container.S0 with type t := t and type elt := Int_array.t * Int_array.t

    module Of_list : sig
      type 'a t

      val create : 'a list -> ('a -> 'a -> bool) -> 'a t
      (** Create a new permutation of a list ordered by a less-than relation. *)

      include Container.Generic with type 'a t := 'a t and type 'a elt := 'a list
    end
  end

  module Restricted : sig
    type t

    val create : int -> (Int_array.t -> bool) -> t
    (** Create a new permutation of {i n} elements that is filtered by a
    function. Only orderings with a prefix that the function allows will be
    enumerated. *)

    include Container.S0 with type t := t and type elt := Int_array.t

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

  include Container.S0 with type t := t and type elt := Int_array.t

  module Of_list : sig
    type 'a t

    val create : 'a list -> int -> 'a t
    (** Create a {i k}-combination of a list. *)

    include Container.Generic with type 'a t := 'a t and type 'a elt := 'a list
  end
end

(** Iterators for compositions. *)
module Composition : sig
  type t

  val create : n:int -> k:int -> t
  (** Create a composition of an integer {i n} into {i k} parts. *)

  include Container.S0 with type t := t and type elt := Int_array.t
end
