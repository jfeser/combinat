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

let () =
  Command.group ~summary:"combinat benchmarks"
    [
      ( "combinations",
        Bench.make_command
          [
            Bench.Test.create ~name:"combinations_naive_small" (fun () ->
                combinations_naive 6 (List.init 10 ~f:(fun i -> i)));
            Bench.Test.create ~name:"combinations_internal_small" (fun () ->
                Combination.iter ~f:(fun _ -> ()) { k = 6; n = 10 });
            Bench.Test.create ~name:"combinations_naive_med" (fun () ->
                combinations_naive 5 (List.init 25 ~f:(fun i -> i)));
            Bench.Test.create ~name:"combinations_internal_med" (fun () ->
                Combination.iter ~f:(fun _ -> ()) { k = 5; n = 25 });
            Bench.Test.create ~name:"combinations_internal_ba_large" (fun () ->
                Combination.iter ~f:(fun _ -> ()) { k = 11; n = 33 });
          ] );
      ( "permutations",
        Bench.make_command
          [
            Bench.Test.create ~name:"permutations_internal_med" (fun () ->
                Permutation.iter ~f:(fun _ -> ()) 8);
          ] );
      ( "sorted-permutations",
        Bench.make_command
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
             Bench.Test.create ~name:"sorted_permutations_internal_med" (fun () ->
                 SortedPermutation.iter (9, young_tableaux) ~f:(fun _ -> ())));
          ] );
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
             Bench.Test.create ~name:"sorted_permutations_internal_med" (fun () ->
                 RestrictedPermutation.iter (8, restrict) ~f:(fun _ -> ())));
          ] );
      ( "partitions",
        Bench.make_command
          [
            Bench.Test.create ~name:"partitions_internal_med" (fun () ->
                Combinat.Partition.iter (65, 20) ~f:(fun _ -> ()));
          ] );
    ]
  |> Command.run
