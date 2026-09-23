(* TEST
 skip;
*)

(* Tests RESUMETERM with extra_args != 0 in bytecode,
   by calling a handler with a tail-continue that returns a function *)

open Effect
open Effect.Deep
open Effect.Deep.Safe

type _ t += E : int t

let handle comp =
  try_with comp ()
  { effc = fun (type a) (e : a t) ->
      match e with
      | E -> Some (fun (k : (a,_) continuation) -> continue k 10)
      | _ -> None }

let () =
  handle (fun h () ->
      Printf.printf "%d\n" (Effect.Safe.perform h E);
      Printf.printf "%d\n") 42
