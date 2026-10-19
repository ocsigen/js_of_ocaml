(* TEST
   { bytecode; }
   { native; }
*)

open Printf
open Effect
open Effect.Deep
open Effect.Deep.Safe

type _ t += E : int -> int t

let f h () =
  printf "perform effect (E 0)\n%!";
  let v = Effect.Safe.perform h (E 0) in
  printf "perform returns %d\n%!" v;
  v + 1

let v =
  match_with f ()
  { retc = (fun v -> printf "done %d\n%!" v; v + 1);
    exnc = (fun e -> raise e);
    effc = (fun (type a) (e : a t) ->
      match e with
      | E v -> Some (fun (k : (a, _) continuation) ->
          printf "caught effect (E %d). continuing..\n%!" v;
          let v = continue k (v + 1) in
          printf "continue returns %d\n%!" v;
          v + 1)
      | e -> None) }

let () = printf "result=%d\n%!" v
