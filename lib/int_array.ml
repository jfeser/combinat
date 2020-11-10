open! Base
open Bigarray

type t = (int, int_elt, c_layout) Array1.t

let length = Array1.dim

let get a i = a.{i}

let sexp_of_t a =
  Sexp.List (List.init (Array1.dim a) ~f:(fun i -> [%sexp_of: int] @@ get a i))
