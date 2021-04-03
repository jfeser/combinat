(** Internal iterators for combinatorial objects. *)

module Int_array : sig
  type t [@@deriving sexp_of]

  val length : t -> int

  val get : t -> int -> int
end

type +'a iter = ('a -> unit) -> unit

val partitions : n:int -> k:int -> Int_array.t iter
(** Iterator over the partitions of an integer {i n} into {i k} parts. *)

val partitions_with_zeros : n:int -> k:int -> Int_array.t iter
(** Iterator over the partitions of an integer {i n} into {i k} parts, including
   partitions where some elements are zero. *)

val permutations : n:int -> Int_array.t iter
(** Iterator over the permutations of a set of {i n} elements. 

permutations ~n:3 = [[0;1;2]; [0;2;1]; [1;0;2]; [1;2;0]; [2;0;1]; [2;1;0]]
*)

val permutations_ordered :
  n:int -> lt:(int -> int -> bool) -> (Int_array.t * Int_array.t) iter
(** Iterator over the permutations of a set of {i n} elements, including only
   the permutations that are ordered according to a less-than relation {i
   lt}. 

let lt a b = a = 0 && b = 2
permutations ~n:3 ~lt = [[0;1;2]; [0;2;1]; [1;0;2]]
*)

val permutations_filtered : n:int -> f:(Int_array.t -> bool) -> Int_array.t iter
(** Iterator over the permutations of a set of {i n} elements, including only
   the permutations where {i f} is true for all prefixes. 

let f x = Int_array.get x 0 = 0
permutations ~n:3 ~lt = [[0;1;2]; [0;2;1]]
*)

val combinations : n:int -> k:int -> Int_array.t iter
(** Iterator over the combinations of size {i k} a set of {i n} elements. 

combinations ~n:4 ~k:2 = [[0;1]; [0;2]; [0;3]; [1;2]; [1;3]; [2;3]]
*)

val compositions : n:int -> k:int -> Int_array.t iter

val subsets : n:int -> k:int -> Int_array.t iter

val using_list : Int_array.t iter -> 'a list -> 'a list iter

val using_set : Int_array.t iter -> ('a, 'b) Set.t -> ('a, 'b) Set.t iter
