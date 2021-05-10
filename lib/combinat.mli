(** Internal iterators for combinatorial objects.

Note: The arrays exposed by this api are mutated by the iterators. They must be
   copied explicitly if they are to be stored elsewhere. *)

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

val permutations_ordered : 'a list -> lt:('a -> 'a -> bool) -> 'a array iter
(** Iterator over the permutations of a set of {i n} elements, including only
   the permutations that are ordered according to a less-than relation {i
   lt}. 

let lt a b = a = 0 && b = 2
permutations_ordered ~n:3 ~lt = [[0;1;2]; [0;2;1]; [1;0;2]]
*)

val permutations_filtered : 'a list -> f:('a array -> bool) -> 'a array iter
(** Iterator over the permutations of a set of {i n} elements, including only
   the permutations where {i f} is true for all prefixes. 

let f x = Int_array.get x 0 = 0
permutations_filtered ~n:3 ~f = [[0;1;2]; [0;2;1]]
*)

val combinations : 'a list -> k:int -> 'a array iter
(** Iterator over the combinations of size {i k} a set of {i n} elements. 

combinations ~n:4 ~k:2 = [[0;1]; [0;2]; [0;3]; [1;2]; [1;3]; [2;3]]
*)

val compositions : n:int -> k:int -> int array iter
(** Iterator over the {i k} compositions of an integer {i n}. *)

val subsets : 'a list -> k:int -> 'a array iter

val to_list : 'a array iter -> 'a array list
(** Convert an iterator to a list. The arrays will be copied. *)
