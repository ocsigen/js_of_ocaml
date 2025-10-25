(* TEST
 ocamlopt_flags += " -O3 ";
*)

open Domain
[@@@ocaml.alert "-unsafe_multidomain-unsafe_parallelism-do_not_spawn_domains"]

let () =
  let l = lazy (print_string "Lazy Forced\n") in
  let d = spawn (fun () -> Lazy.force l) in
  join d
