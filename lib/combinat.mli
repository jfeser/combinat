open Base

(* Implementations of combinatorics routines. *)

val m_partition : int -> int -> int Array.t Sequence.t
(** Compute the partitions of an integer {i n} into {i m} parts. See
    (Knuth 3b, pg. 2). *)

val m_partition_with_zeros : int -> int -> int Array.t Sequence.t
(** Compute the partitions of an integer {i n} into {i m} parts,
    including partitions where some elements are zero. *)

val permutations : int Array.t -> int Array.t Sequence.t
(** Compute the unique permutations of an array. See (Knuth 2b, pg. 1). *)

val combinations : int -> int -> int array Sequence.t

val combinations_iter : f:(int array -> unit) -> int -> int -> unit

val combinations_iter_ba :
     f:((int, Bigarray.int_elt, Bigarray.c_layout) Bigarray.Array1.t -> unit)
  -> int
  -> int
  -> unit

val all_combinations : int -> int array Sequence.t

module Poly : sig
  val combinations : int -> 'a array -> 'a array Sequence.t

  val all_combinations : 'a array -> 'a array Sequence.t

  val permutations : 'a Array.t -> 'a Array.t Sequence.t
end
