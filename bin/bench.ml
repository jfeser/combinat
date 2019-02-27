open Core
open Core_bench
module Seq = Sequence

let rec combinations_naive k list =
  if k <= 0 then [[]]
  else
    match list with
    | [] -> []
    | h :: tl ->
        let with_h =
          List.map ~f:(fun l -> h :: l) (combinations_naive (k - 1) tl)
        in
        let without_h = combinations_naive k tl in
        with_h @ without_h

let () =
  Command.run
    (Bench.make_command
       [ Bench.Test.create ~name:"combinations_naive_small" (fun () ->
             combinations_naive 6 (List.init 10 ~f:(fun i -> i)) )
       ; Bench.Test.create ~name:"combinations_external_small" (fun () ->
             Combinat.combinations 6 10 |> Seq.iter ~f:(fun _ -> ()) )
       ; Bench.Test.create ~name:"combinations_internal_small" (fun () ->
             Combinat.combinations_iter ~f:(fun _ -> ()) 6 10 )
       ; Bench.Test.create ~name:"combinations_naive_med" (fun () ->
             combinations_naive 5 (List.init 25 ~f:(fun i -> i)) )
       ; Bench.Test.create ~name:"combinations_external_med" (fun () ->
             Combinat.combinations 5 25 |> Seq.iter ~f:(fun _ -> ()) )
       ; Bench.Test.create ~name:"combinations_internal_med" (fun () ->
             Combinat.combinations_iter ~f:(fun _ -> ()) 5 25 )
       ; Bench.Test.create ~name:"combinations_internal_med_ba" (fun () ->
             Combinat.combinations_iter_ba ~f:(fun _ -> ()) 5 25 ) ])
