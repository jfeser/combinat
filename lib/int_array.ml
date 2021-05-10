open Bigarray

type t = (int, int_elt, c_layout) Array1.t

let length = Array1.dim

let get a i = a.{i}

let to_list a = List.init (Array1.dim a) ~f:(get a)

let sexp_of_t a = [%sexp_of: int list] @@ to_list a
