open Base
module Seq = Sequence

let m_partition n m =
  (* if m <= 0 then failwiths "'m' must be greater than or equal to 1." m [%sexp_of:int]; *)
  if n < m then Seq.empty
  else if m <= 0 then Seq.empty
  else if m = 1 then Seq.singleton (Array.create ~len:1 n)
  else
    let a_init = Array.create ~len:m 1 in
    a_init.(0) <- n - m + 1 ;
    let init_seq = Seq.singleton a_init in
    let rest_seq =
      Seq.unfold ~init:a_init ~f:(fun a ->
          let a = Array.copy a in
          if a.(1) >= a.(0) - 1 then (
            let j = ref 2 in
            let s = ref (a.(0) + a.(1) - 1) in
            while !j < m && a.(!j) >= a.(0) - 1 do
              s := !s + a.(!j) ;
              Caml.incr j
            done ;
            if !j >= m then None
            else
              let x = a.(!j) + 1 in
              a.(!j) <- x ;
              Caml.decr j ;
              while !j > 0 do
                a.(!j) <- x ;
                s := !s - x ;
                Caml.decr j
              done ;
              a.(0) <- !s ;
              Some (Array.copy a, a) )
          else (
            a.(0) <- a.(0) - 1 ;
            a.(1) <- a.(1) + 1 ;
            Some (Array.copy a, a) ) )
    in
    Seq.append init_seq rest_seq

let%expect_test "m_partition" =
  m_partition 6 3
  |> Seq.iter ~f:(fun c ->
         Stdio.print_endline (Sexp.to_string_hum ([%sexp_of: int array] c)) ) ;
  [%expect {|
    (4 1 1)
    (3 2 1)
    (2 2 2) |}]

let m_partition_with_zeros n m =
  if n = 0 then Array.create ~len:m 0 |> Seq.singleton
  else
    Seq.init (m + 1) ~f:(fun m' ->
        m_partition n m'
        |> Seq.map ~f:(fun p ->
               let p' = Array.create ~len:m 0 in
               Array.blito ~src:p ~dst:p' () ;
               p' ) )
    |> Seq.concat

let%expect_test "m_partition_with_zeros" =
  m_partition_with_zeros 6 3
  |> Seq.iter ~f:(fun c ->
         Stdio.print_endline (Sexp.to_string_hum ([%sexp_of: int array] c)) ) ;
  [%expect
    {|
    (6 0 0)
    (5 1 0)
    (4 2 0)
    (3 3 0)
    (4 1 1)
    (3 2 1)
    (2 2 2) |}]

let permutations a_init =
  let a_init = Array.copy a_init in
  Array.sort ~compare:Int.compare a_init ;
  let init_seq = Seq.singleton a_init in
  let rest_seq =
    Seq.unfold ~init:a_init ~f:(fun a ->
        let a = Array.copy a in
        let n = Array.length a in
        let j = ref (n - 2) in
        while !j >= 0 && a.(!j) >= a.(!j + 1) do
          Caml.decr j
        done ;
        if !j < 0 then None
        else
          let l = ref (n - 1) in
          while a.(!j) >= a.(!l) do
            Caml.decr l
          done ;
          Array.swap a !j !l ;
          let k = ref (!j + 1) in
          let l = ref (n - 1) in
          while !k < !l do
            Array.swap a !k !l ; Caml.incr k ; Caml.decr l
          done ;
          Some (a, a) )
  in
  Seq.append init_seq rest_seq

let%expect_test "permutations" =
  permutations [|1; 2; 3; 4|]
  |> Seq.iter ~f:(fun c ->
         Stdio.print_endline (Sexp.to_string_hum ([%sexp_of: int array] c)) ) ;
  [%expect
    {|
    (1 2 3 4)
    (1 2 4 3)
    (1 3 2 4)
    (1 3 4 2)
    (1 4 2 3)
    (1 4 3 2)
    (2 1 3 4)
    (2 1 4 3)
    (2 3 1 4)
    (2 3 4 1)
    (2 4 1 3)
    (2 4 3 1)
    (3 1 2 4)
    (3 1 4 2)
    (3 2 1 4)
    (3 2 4 1)
    (3 4 1 2)
    (3 4 2 1)
    (4 1 2 3)
    (4 1 3 2)
    (4 2 1 3)
    (4 2 3 1)
    (4 3 1 2)
    (4 3 2 1) |}]

let combinations t n =
  assert (t >= 0 && t <= n) ;
  if t = 0 then Seq.singleton [||]
  else if t = n then Seq.singleton (Array.init n ~f:(fun i -> i))
  else
    let c =
      Array.init (t + 3) ~f:(fun i ->
          if 1 <= i && i <= t then i - 1
          else if i = t + 1 then n
          else if i = t + 2 then 0
          else 0 )
    in
    let j = t in
    Seq.unfold_step
      ~init:(c, j, 0, `T2)
      ~f:(fun (c, j, x, state) ->
        match state with
        | `T2 ->
            let ret = Array.sub c ~pos:1 ~len:t in
            let state, x = if j > 0 then (`T6, j) else (`T3, x) in
            Seq.Step.Yield (ret, (c, j, x, state))
        | `T3 ->
            let c, j, state =
              if c.(1) + 1 < c.(2) then (
                let c = Array.copy c in
                c.(1) <- c.(1) + 1 ;
                (c, j, `T2) )
              else (c, 2, `T4_T5)
            in
            Seq.Step.Skip (c, j, x, state)
        | `T4_T5 ->
            let c = Array.copy c in
            let rec loop j =
              c.(j - 1) <- j - 2 ;
              let x = c.(j) + 1 in
              if x = c.(j + 1) then loop (j + 1) else (x, j)
            in
            let x, j = loop j in
            if j > t then Seq.Step.Done else Seq.Step.Skip (c, j, x, `T6)
        | `T6 ->
            let c = Array.copy c in
            c.(j) <- x ;
            let j = j - 1 in
            let state = `T2 in
            Seq.Step.Skip (c, j, x, state) )

let combinations_iter ~f t n =
  assert (t >= 0 && t <= n) ;
  if t = 0 then ()
  else if t = n then f (Array.init n ~f:(fun i -> i))
  else
    let c =
      Array.init (t + 3) ~f:(fun i ->
          if 1 <= i && i <= t then i - 1
          else if i = t + 1 then n
          else if i = t + 2 then 0
          else 0 )
    in
    let c' = Array.sub c ~pos:1 ~len:t in
    let ( .%() ) = Array.unsafe_get in
    let ( .%()<- ) arr i x =
      if i <= t then Array.unsafe_set c' (i - 1) x ;
      Array.unsafe_set arr i x
    in
    let j = ref t in
    let x = ref 0 in
    let rec t2 () =
      f c' ;
      if !j > 0 then (
        x := !j ;
        t6 () )
      else t3 ()
    and t3 () =
      if c.%(1) + 1 < c.%(2) then (
        c.%(1) <- (c.%(1) + 1) ;
        t2 () )
      else (
        j := 2 ;
        t4_t5 () )
    and t4_t5 () =
      let rec loop () =
        c.%((!j - 1)) <- (!j - 2) ;
        x := c.%(!j) + 1 ;
        if !x = c.%((!j + 1)) then (
          j := !j + 1 ;
          loop () )
      in
      loop () ;
      if !j <= t then t6 ()
    and t6 () =
      c.%(!j) <- !x ;
      j := !j - 1 ;
      t2 ()
    in
    t2 ()

let%expect_test "combinations" =
  combinations_iter 3 5 ~f:(fun c ->
      Stdio.print_endline (Sexp.to_string_hum ([%sexp_of: int array] c)) ) ;
  [%expect
    {|
    (0 1 2)
    (0 1 3)
    (0 2 3)
    (1 2 3)
    (0 1 4)
    (0 2 4)
    (1 2 4)
    (0 3 4)
    (1 3 4)
    (2 3 4) |}]

let all_combinations n =
  Seq.range 1 n ~stop:`inclusive
  |> Seq.concat_map ~f:(fun t -> combinations t n)

let%expect_test "combinations" =
  combinations 3 5
  |> Seq.iter ~f:(fun c ->
         Stdio.print_endline (Sexp.to_string_hum ([%sexp_of: int array] c)) ) ;
  [%expect
    {|
    (0 1 2)
    (0 1 3)
    (0 2 3)
    (1 2 3)
    (0 1 4)
    (0 2 4)
    (1 2 4)
    (0 3 4)
    (1 3 4)
    (2 3 4) |}]

let%expect_test "all_combinations" =
  all_combinations 5
  |> Seq.iter ~f:(fun c ->
         Stdio.print_endline (Sexp.to_string_hum ([%sexp_of: int array] c)) ) ;
  [%expect
    {|
    (0)
    (1)
    (2)
    (3)
    (4)
    (0 1)
    (0 2)
    (1 2)
    (0 3)
    (1 3)
    (2 3)
    (0 4)
    (1 4)
    (2 4)
    (3 4)
    (0 1 2)
    (0 1 3)
    (0 2 3)
    (1 2 3)
    (0 1 4)
    (0 2 4)
    (1 2 4)
    (0 3 4)
    (1 3 4)
    (2 3 4)
    (0 1 2 3)
    (0 1 2 4)
    (0 1 3 4)
    (0 2 3 4)
    (1 2 3 4)
    (0 1 2 3 4) |}]

module Poly = struct
  let permutations elems =
    permutations (Array.init (Array.length elems) ~f:(fun x -> x))
    |> Seq.map ~f:(fun indices -> Array.map indices ~f:(fun i -> elems.(i)))

  let combinations t elems =
    let n = Array.length elems in
    combinations t n
    |> Seq.map ~f:(fun indices -> Array.map indices ~f:(fun i -> elems.(i)))

  let all_combinations elems =
    let n = Array.length elems in
    all_combinations n
    |> Seq.map ~f:(fun indices -> Array.map indices ~f:(fun i -> elems.(i)))
end
