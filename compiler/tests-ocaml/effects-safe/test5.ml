(* TEST
   { bytecode; }
   { native; }
*)

open Effect
open Effect.Deep
open Effect.Deep.Safe

type _ t += Foo : int -> int t

let f h () = (Effect.Safe.perform h (Foo 3)) (* 3 + 1 *)
           + (Effect.Safe.perform h (Foo 3)) (* 3 + 1 *)

let r =
  try_with f ()
  { effc = fun (type a) (e : a t) ->
      match e with
      | Foo i -> Some (fun (k : (a, _) continuation) ->
          try_with (fun _h x -> continue k x) (i + 1)
          { effc = fun (type a) (e : a t) ->
              match e with
              | Foo i -> Some (fun k -> failwith "NO")
              | _ -> None })
      | e -> None }

let () = Printf.printf "%d\n" r
