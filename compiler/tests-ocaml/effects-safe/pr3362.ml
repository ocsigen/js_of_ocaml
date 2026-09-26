(* TEST
   { bytecode; }
   { native; }
*)

open Effect
open Effect.Deep
open Effect.Deep.Safe

type _ t += F : string t

let handle comp =
  Gc.compact ();
  try_with comp ()
    { effc = fun (type a) (e : a t) ->
      Gc.compact ();
      match e with
      | F -> Some (fun (k : (a,_) continuation) ->
        Gc.compact (); continue k "Hello, world!")
      | _ -> None }

let () = handle (fun h () ->
  Gc.compact ();
  print_endline (Effect.Safe.perform h F ^ (" " ^ Effect.Safe.perform h F)))
