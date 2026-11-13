(* TEST
   { bytecode; }
   { native; }
*)

open Effect
open Effect.Safe
open Effect.Deep
open Effect.Deep.Safe

type 'a t += Ping : int -> int t

(* [retc] (like [exnc] and [effc]) runs in the context that *encloses* the
   handler -- by the time it runs, the handler's own computation has already
   returned -- so it can only perform effects when that enclosing context is
   itself within a handler. The plain [Safe.match_with] therefore gives [retc]
   no [Handler.t]. To perform effects from [retc] we use
   [With_handler.match_with], passing the *outer* handler's token; the outer
   handler (which handles [Ping]) is what actually services the effect. *)

let () =
  match_with
    (fun outer_h () ->
       With_handler.match_with outer_h
         (fun _inner_h () -> 1)
         ()
         { retc = (fun h v -> perform h (Ping v))
         ; exnc = (fun _h e -> raise e)
         ; effc = (fun (type a) _h (_e : a t) -> None)
         })
    ()
    { retc = (fun v -> v)
    ; exnc = (fun e -> raise e)
    ; effc =
        (fun (type a) (e : a t) ->
          match e with
          | Ping i -> Some (fun (k : (a, _) continuation) -> continue k (i + 10))
          | _ -> None)
    }
  |> print_int;
  print_newline ()
