let rec inc a l f =
  let k = Array.length a in
  f a;
  a.(k - 1) <- a.(k - 1) + 1;
  if a.(k - 1) >= l.(k - 1) then carry a l f (k - 1) else inc a l f

and carry a l f j =
  if j > 0 then (
    a.(j) <- 0;
    a.(j - 1) <- a.(j - 1) + 1;
    if a.(j - 1) >= l.(j - 1) then carry a l f (j - 1) else inc a l f)

let iter_restricted elems f =
  let k = List.length elems in

  (* an empty list means that there are no valid entries for an index *)
  if List.exists elems ~f:List.is_empty then ()
  else if k = 0 then f [||] (* there is only one empty sequence *)
  else
    let elems = Array.of_list @@ List.map ~f:Array.of_list elems in
    let a = Array.create ~len:k 0 and l = Array.map elems ~f:Array.length in
    let output = Array.create ~len:k elems.(0).(0) in
    let f a =
      for i = 0 to k - 1 do
        output.(i) <- elems.(i).(a.(i))
      done;
      f output
    in
    inc a l f

let iter elems ~k f =
  if k < 0 then raise_s [%message "sequences: expected k >= 0" (k : int)];
  iter_restricted (List.init k ~f:(fun _ -> elems)) f
