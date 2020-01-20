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
  make_command
    [
      create ~name:"combinations_naive_small" (fun () ->
          combinations_naive 6 (List.init 10 ~f:(fun i -> i))
          |> List.iter ~f:(fun _ -> Sys.opaque_identity ()));
      create ~name:"combinations_list_small" (fun () ->
          Combination.Of_list.iter
            (List.init 10 ~f:(fun i -> i), 6)
            ~f:(fun _ -> Sys.opaque_identity ()));
      create ~name:"combinations_small" (fun () ->
          Combination.iter ~f:(fun _ -> Sys.opaque_identity ()) { k = 6; n = 10 });
      create ~name:"combinations_naive_med" (fun () ->
          combinations_naive 5 (List.init 25 ~f:(fun i -> i))
          |> List.iter ~f:(fun _ -> Sys.opaque_identity ()));
      create ~name:"combinations_list_small" (fun () ->
          Combination.Of_list.iter
            (List.init 25 ~f:(fun i -> i), 5)
            ~f:(fun _ -> Sys.opaque_identity ()));
      create ~name:"combinations_med" (fun () ->
          Combination.iter ~f:(fun _ -> Sys.opaque_identity ()) { k = 5; n = 25 });
      create ~name:"combinations_large" (fun () ->
          Combination.iter ~f:(fun _ -> Sys.opaque_identity ()) { k = 11; n = 33 });
    ]

let permutations_bench =
  let open Bench in
  let open Test in
  make_command
    [
      (* create_group ~name:"small"
       *   [
       *     create ~name:"naive" (fun () ->
       *         permutations_naive (List.init 3 ~f:(fun i -> i))
       *         |> List.iter ~f:(fun _ -> Sys.opaque_identity ()));
       *     create ~name:"std" (fun () ->
       *         Permutation.iter ~f:(fun _ -> Sys.opaque_identity ()) 3);
       *   ]; *)
      create_group ~name:"med"
        [
          create ~name:"naive" (fun () ->
              permutations_naive (List.init 5 ~f:(fun i -> i))
              |> List.iter ~f:(fun _ -> Sys.opaque_identity ()));
          create ~name:"std" (fun () ->
              Permutation.iter ~f:(fun _ -> Sys.opaque_identity ()) 5);
        ];
    ]

let sorted_perms_bench =
  let open Bench in
  let open Test in
  make_command
    [
      (let young_tableaux x y =
         match (x, y) with
         | 1, (2 | 3 | 4 | 5 | 6 | 7 | 8 | 9)
         | 2, (3 | 5 | 6 | 8 | 9)
         | 3, (6 | 9)
         | 4, (5 | 6 | 7 | 8 | 9)
         | 5, (6 | 8 | 9)
         | 6, 9
         | 7, (8 | 9)
         | 8, 9 ->
             true
         | _ -> false
       in
       create ~name:"sorted_permutations_med" (fun () ->
           let p = Permutation.Sorted.create 9 young_tableaux in
           Permutation.Sorted.iter p ~f:(fun _ -> Sys.opaque_identity ())));
    ]

let () =
  Command.group ~summary:"combinat benchmarks"
    [
      ("combinations", combinations_bench);
      ("permutations", permutations_bench);
      ("sorted-permutations", sorted_perms_bench);
      ( "restricted-permutations",
        Bench.make_command
          [
            (let restrict a =
               match Bigarray.Array1.dim a with
               | 1 -> not (a.{0} = 2)
               | 2 -> not (a.{0} = 1 && a.{1} = 4)
               | 3 ->
                   not
                     ( (a.{0} = 1 && a.{1} = 3 && a.{2} = 2)
                     || (a.{0} = 3 && a.{1} = 1 && a.{2} = 4) )
               | 4 -> not (a.{0} = 4 && a.{1} = 3 && a.{2} = 1 && a.{3} = 2)
               | _ -> true
             in
             Bench.Test.create ~name:"restricted_permutations_med" (fun () ->
                 let p = Permutation.Restricted.create 8 restrict in
                 Permutation.Restricted.iter p ~f:(fun _ -> Sys.opaque_identity ())));
          ] );
      ( "partitions",
        Bench.make_command
          [
            Bench.Test.create ~name:"partitions_med" (fun () ->
                Combinat.Partition.iter (65, 20) ~f:(fun _ -> ()));
          ] );
    ]
  |> Command.run
