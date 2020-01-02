open! Base

let with_return = With_return.with_return

type ('t, 'a, 'accum) fold =
  't -> init:'accum -> f:('accum -> 'a -> 'accum) -> 'accum

type ('t, 'a) iter = 't -> f:('a -> unit) -> unit

type 't length = 't -> int

let iter ~fold t ~f = fold t ~init:() ~f:(fun () a -> f a)

let count ~fold t ~f = fold t ~init:0 ~f:(fun n a -> if f a then n + 1 else n)

let sum (type a) ~fold (module M : Container.Summable with type t = a) t ~f =
  fold t ~init:M.zero ~f:(fun n a -> M.( + ) n (f a))

let fold_result ~fold ~init ~f t =
  with_return (fun { return } ->
      Result.Ok
        (fold t ~init ~f:(fun acc item ->
             match f acc item with Result.Ok x -> x | Error _ as e -> return e)))

let fold_until ~fold ~init ~f ~finish t =
  with_return (fun { return } ->
      finish
        (fold t ~init ~f:(fun acc item ->
             match f acc item with
             | Continue_or_stop.Continue x -> x
             | Stop x -> return x)))

let min_elt ~fold t ~compare =
  fold t ~init:None ~f:(fun acc elt ->
      match acc with
      | None -> Some elt
      | Some min -> if compare min elt > 0 then Some elt else acc)

let max_elt ~fold t ~compare =
  fold t ~init:None ~f:(fun acc elt ->
      match acc with
      | None -> Some elt
      | Some max -> if compare max elt < 0 then Some elt else acc)

let length ~fold c = fold c ~init:0 ~f:(fun acc _ -> acc + 1)

let is_empty ~iter c =
  with_return (fun r ->
      iter c ~f:(fun _ -> r.return false);
      true)

let exists ~iter c ~f =
  with_return (fun r ->
      iter c ~f:(fun x -> if f x then r.return true);
      false)

let for_all ~iter c ~f =
  with_return (fun r ->
      iter c ~f:(fun x -> if not (f x) then r.return false);
      true)

let find_map ~iter t ~f =
  with_return (fun r ->
      iter t ~f:(fun x ->
          match f x with None -> () | Some _ as res -> r.return res);
      None)

let find ~iter c ~f =
  with_return (fun r ->
      iter c ~f:(fun x -> if f x then r.return (Some x));
      None)

let to_list ~fold c = List.rev (fold c ~init:[] ~f:(fun acc x -> x :: acc))

let to_array ~length ~iter c =
  let array = ref [||] in
  let i = ref 0 in
  iter c ~f:(fun x ->
      if !i = 0 then array := Array.create ~len:(length c) x;
      !array.(!i) <- x;
      Int.incr i);
  !array

module Make (T : Base__.Container_intf.Make_gen_arg) : sig
  include
    Base__.Container.Generic with type 'a t := 'a T.t with type 'a elt := 'a T.elt
end = struct
  let fold = T.fold

  let iter =
    match T.iter with
    | `Custom iter -> iter
    | `Define_using_fold -> fun t ~f -> iter ~fold t ~f

  let length =
    match T.length with
    | `Custom length -> length
    | `Define_using_fold -> fun t -> length ~fold t

  let is_empty t = is_empty ~iter t

  let sum m t = sum ~fold m t

  let count t ~f = count ~fold t ~f

  let exists t ~f = exists ~iter t ~f

  let for_all t ~f = for_all ~iter t ~f

  let find_map t ~f = find_map ~iter t ~f

  let find t ~f = find ~iter t ~f

  let to_list t = to_list ~fold t

  let to_array t = to_array ~length ~iter t

  let min_elt t ~compare = min_elt ~fold t ~compare

  let max_elt t ~compare = max_elt ~fold t ~compare

  let fold_result t ~init ~f = fold_result t ~fold ~init ~f

  let fold_until t ~init ~f ~finish = fold_until t ~fold ~init ~f ~finish
end