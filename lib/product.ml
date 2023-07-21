let iter l k =
  let elems = Array.of_list (List.map Array.of_list l) in
  if Array.for_all (fun a -> Array.length a > 0) elems then
    let counters = Array.make (Array.length elems) 0 in

    let rec carry i =
      if i < Array.length counters then (
        counters.(i) <- counters.(i) + 1;
        if counters.(i) >= Array.length elems.(i) then (
          counters.(i) <- 0;
          carry (i + 1))
        else emit ())
    and emit () =
      k (Array.mapi (fun j xs -> xs.(counters.(j))) elems);
      carry 0
    in

    emit ()
