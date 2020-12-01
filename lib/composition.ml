open! Base
open! Shared

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

type t = { n : int; k : int }

let create ~n ~k = { n; k }

include Container.Make0 (struct
  type nonrec t = t

  module Elt = struct
    type t = int_array

    let equal = equal
  end

  open Unsafe

  let rec fold2 f acc i n p =
    if i >= n then acc
    else (
      p.{0} <- i;
      p.{1} <- n - i;
      let acc = f acc p in
      fold2 f acc (i + 1) n p )

  let fold { n; k } ~init ~f =
    let open Bigarray in
    let open Array1 in
    if n < k then init
    else
      let p = create int c_layout k in
      if k = 0 && n = 0 then f init p
      else if k = 1 then (
        p.{0} <- n;
        f init p )
      else if k = 2 then fold2 f init 1 n p
      else
        Combination.create ~n:(n - 1) ~k:(k - 1)
        |> Combination.fold ~init ~f:(fun x c ->
               p.{0} <- c.{0} + 1;
               for i = 1 to k - 2 do
                 p.{i} <- c.{i} - c.{i - 1}
               done;
               p.{k - 1} <- n - 1 - c.{k - 2};

               f x p)

  let iter = `Define_using_fold

  let length = `Define_using_fold
end)
