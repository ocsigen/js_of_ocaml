(* TEST
   { bytecode; }
   { native; }
*)

open Effect
open Effect.Deep
open Effect.Deep.Safe

type _ t += E : unit t
exception X

let () =
  Printf.printf "%d\n%!" @@
  match_with (fun _h () ->
    Printf.printf "in handler. raising X\n%!";
    raise X) ()
    { retc = (fun v -> v);
      exnc = (function
        | X -> 10
        | e -> raise e);
      effc = (fun (type a) (e : a t) ->
        match e with
        | E -> Some (fun k -> 11)
        | e -> None) }
