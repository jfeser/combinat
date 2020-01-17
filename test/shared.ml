open! Base

let to_array arr = Array.init (Bigarray.Array1.dim arr) ~f:(fun i -> arr.{i})
