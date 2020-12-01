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
  let module C = Combination in
  make_command
    [
      create_group ~name:"small"
        [
          create ~name:"naive" (fun () ->
              combinations_naive 6 (List.init 10 ~f:(fun i -> i))
              |> List.iter ~f:(fun _ -> Sys.opaque_identity ()));
          create ~name:"list" (fun () ->
              C.Of_list.(
                create (List.init 10 ~f:(fun i -> i)) 6
                |> iter ~f:(fun _ -> Sys.opaque_identity ())));
          create ~name:"std" (fun () ->
              C.(create ~k:6 ~n:10 |> iter ~f:(fun _ -> Sys.opaque_identity ())));
        ];
      create_group ~name:"med"
        [
          create ~name:"naive" (fun () ->
              combinations_naive 5 (List.init 25 ~f:(fun i -> i))
              |> List.iter ~f:(fun _ -> Sys.opaque_identity ()));
          create ~name:"list" (fun () ->
              C.Of_list.(
                create (List.init 25 ~f:(fun i -> i)) 5
                |> iter ~f:(fun _ -> Sys.opaque_identity ())));
          create ~name:"std" (fun () ->
              C.(create ~k:5 ~n:25 |> iter ~f:(fun _ -> Sys.opaque_identity ())));
        ];
      create ~name:"combinations_large" (fun () ->
          C.(create ~k:11 ~n:33 |> iter ~f:(fun _ -> Sys.opaque_identity ())));
    ]

let permutations_bench =
  let open Bench in
  let open Test in
  let module P = Permutation in
  make_command
    [
      create_group ~name:"med"
        [
          create ~name:"naive" (fun () ->
              permutations_naive (List.init 8 ~f:(fun i -> i))
              |> List.iter ~f:(fun _ -> Sys.opaque_identity ()));
          create ~name:"std" (fun () ->
              P.(create 8 |> iter ~f:(fun _ -> Sys.opaque_identity ())));
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
               let ( .%{} ) = Int_array.get in
               match Int_array.length a with
               | 1 -> not (a.%{0} = 2)
               | 2 -> not (a.%{0} = 1 && a.%{1} = 4)
               | 3 ->
                   not
                     ( (a.%{0} = 1 && a.%{1} = 3 && a.%{2} = 2)
                     || (a.%{0} = 3 && a.%{1} = 1 && a.%{2} = 4) )
               | 4 -> not (a.%{0} = 4 && a.%{1} = 3 && a.%{2} = 1 && a.%{3} = 2)
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
                Combinat.Partition.(create ~n:65 ~parts:20 |> iter ~f:(fun _ -> ())));
          ] );
    ]
  |> Command.run
