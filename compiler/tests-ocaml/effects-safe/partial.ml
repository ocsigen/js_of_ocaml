(* TEST
   { bytecode; }
   { native; }
*)

open Effect
open Effect.Deep
open Effect.Deep.Safe

type _ t += E : unit t
exception Done

(* Set up (and tear down) an inner handler for [E]. It returns before any
   effect is performed, so the subsequent [perform E] must reach the outer
   handler rather than this (now popped) one. *)
let handle_partial () =
  try_with (fun _h () -> ()) ()
  { effc = fun (type a) (e : a t) ->
      match e with
      | E -> Some (fun k -> assert false)
      | _ -> None }

let f h () = Effect.Safe.perform h E [@nontail]

let () =
  match_with (fun h () -> handle_partial (); f h ()) ()
  { retc = (fun x -> assert false);
    exnc = (function
      | Done -> print_string "ok\n"
      | e -> raise e);
    effc = fun (type a) (e : a t) ->
      match e with
      | E -> Some (fun (k : (a, _) continuation) -> discontinue k Done)
      | _ -> None }
