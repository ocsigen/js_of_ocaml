(* TEST
   { bytecode; }
   { native; }
*)

open Effect
open Effect.Deep
open Effect.Deep.Safe

type _ t += Foo : int -> int t

let r =
  try_with (fun h eff -> Effect.Safe.perform h eff [@nontail]) (Foo 3)
  { effc = fun (type a) (e : a t) ->
      match e with
      | Foo i -> Some (fun (k : (a,_) continuation) ->
          try_with (fun _h x -> continue k x) (i+1)
          { effc = fun (type a) (e : a t) ->
              match e with
              | Foo i -> Some (fun k -> failwith "NO")
              | e -> None })
      | e -> None }

let () = Printf.printf "%d\n" r
