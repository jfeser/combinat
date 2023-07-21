(** Internal iterators for combinatorial objects. *)

type +'a iter = ('a -> unit) -> unit

val partitions : n:int -> k:int -> int array iter
(** Iterator over the partitions of an integer {i n} into {i k} parts. *)

val partitions_with_zeros : n:int -> k:int -> int array iter
(** Iterator over the partitions of an integer {i n} into {i k} parts, including
   partitions where some elements are zero. *)

val permutations : 'a list -> 'a array iter
(** Iterator over the permutations of a set of {i n} elements.

permutations ~n:3 = [[0;1;2]; [0;2;1]; [1;0;2]; [1;2;0]; [2;0;1]; [2;1;0]]
*)

val sequences : 'a list -> k:int -> 'a array iter
(** Iterator over the sequences of length {i k} of a list of elements. Sequences
    are a superset of combinations that also consider every ordering. *)

val sequences_restricted : 'a list list -> 'a array iter
(** Iterator over all sequences where the ith element of the sequence comes from the ith list. *)

val combinations : 'a list -> k:int -> 'a array iter
(** Iterator over the combinations of size {i k} a set of {i n} elements.

combinations ~n:4 ~k:2 = [[0;1]; [0;2]; [0;3]; [1;2]; [1;3]; [2;3]]
*)

val compositions : n:int -> k:int -> int array iter
(** Iterator over the {i k} compositions of an integer {i n}. *)

val product : 'a list list -> 'a array iter
(** Iterator over the cartesian product of a list of lists. *)
