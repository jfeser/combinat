open Core
open Core_bench
module Seq = Sequence
open Combinat

let rec combinations_naive k list =
  if k <= 0 then [ [] ]
  else
    match list with
    | [] -> []
    | h :: tl ->
        let with_h = List.map ~f:(fun l -> h :: l) (combinations_naive (k - 1) tl) in
        let without_h = combinations_naive k tl in
        with_h @ without_h

let distribute c l =
  let rec insert acc1 acc2 = function
    | [] -> acc2
    | hd :: tl ->
        insert (hd :: acc1) (List.rev_append acc1 (hd :: c :: tl) :: acc2) tl
  in
  insert [] [ c :: l ] l

let rec permutations_naive = function
  | [] -> [ [] ]
  | hd :: tl ->
      List.fold_left
        ~f:(fun acc x -> List.rev_append (distribute hd x) acc)
        ~init:[] (permutations_naive tl)

let combinations_bench =
  let open Bench in
  let open Test in
  let naive ~n ~k = combinations_naive k (List.init n ~f:(fun i -> i))
  and std ~n ~k = combinations (List.init n ~f:Fun.id) ~k ignore in
  make_command
    [
      create_group ~name:"small"
        [
          create ~name:"naive" (fun () -> naive ~n:10 ~k:6);
          create ~name:"std" (fun () -> std ~n:10 ~k:6);
        ];
      create_group ~name:"med"
        [
          create ~name:"naive" (fun () -> naive ~n:25 ~k:10);
          create ~name:"std" (fun () -> std ~n:25 ~k:10);
        ];
    ]

let permutations_bench =
  let open Bench in
  let open Test in
  let naive n = permutations_naive (List.init n ~f:(fun i -> i))
  and std n = permutations (List.init n ~f:(fun i -> i)) ignore in
  make_command
    [
      create_group ~name:"med"
        [
          create ~name:"naive" (fun () -> naive 8);
          create ~name:"std" (fun () -> std 8);
        ];
    ]

let partitions_bench =
  let open Bench in
  let open Test in
  let std ~n ~k = partitions ~n ~k ignore in
  make_command [ create ~name:"std" (fun () -> std ~n:65 ~k:20) ]

let () =
  Command.group ~summary:"combinat benchmarks"
    [
      ("combinations", combinations_bench);
      ("permutations", permutations_bench);
      ("partitions", partitions_bench);
    ]
  |> Command.run
