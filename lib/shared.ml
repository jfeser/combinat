open! Base

type int_array = (int, Bigarray.int_elt, Bigarray.c_layout) Bigarray.Array1.t

module Unsafe = struct
  module Bigarray = struct
    include Bigarray

    module Array1 = struct
      include Bigarray.Array1

      let get : int_array -> int -> int = unsafe_get

      let set : int_array -> int -> int -> unit = unsafe_set
    end
  end
end

let rec fact n = if n = 1 then 1 else n * fact (n - 1)

let to_array arr = Array.init (Bigarray.Array1.dim arr) ~f:(fun i -> arr.{i})

let equal a1 a2 =
  assert (Bigarray.Array1.dim a1 = Bigarray.Array1.dim a2);
  let n = Bigarray.Array1.dim a1 in
  let rec loop i =
    if i >= n then true else if a1.{i} = a2.{i} then loop (i + 1) else false
  in
  loop 0

let rec binom n k =
  if k > n then failwith "Undefined"
  else if k = 0 then 1
  else if k > n / 2 then binom n (n - k)
  else n * binom (n - 1) (k - 1) / k
