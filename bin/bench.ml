open Core
open Core_bench
module Seq = Sequence

let () =
  Command.run
    (Bench.make_command
       [ Bench.Test.create ~name:"combinations_external_small" (fun () ->
             Combinat.combinations 6 10 |> Seq.iter ~f:(fun _ -> ()) )
       ; Bench.Test.create ~name:"combinations_internal_small" (fun () ->
             Combinat.combinations_iter ~f:(fun _ -> ()) 6 10 ) ])
