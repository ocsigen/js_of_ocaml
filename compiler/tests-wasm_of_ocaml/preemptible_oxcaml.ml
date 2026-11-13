(* Preemptible effect handlers.  Neither js_of_ocaml nor wasm_of_ocaml has a
   tick source, so [tickc] is never called and no [Preemption] effect is ever
   performed; what the runtime has to get right is that a preemptible handler
   otherwise behaves exactly like an ordinary deep handler. *)

open Effect
open Effect.Deep

type _ Effect.t += Add : int -> int Effect.t

exception Boom

let ticked = ref false

let tickc () =
  (* Asking to preempt would be honoured if ticks ever fired, so this also
     pins down that they do not. *)
  ticked := true;
  Preempt

let effc (type a) (e : a Effect.t) =
  match e with
  | Add n -> Some (fun (k : (a, _) continuation) -> continue k (n * 10))
  | _ -> None

(* The value path: effects performed under the handler are handled, and the
   result goes through [retc]. *)
let test_value () =
  let r =
    Preemptible.Safe.match_with
      (fun h () -> Effect.Safe.perform h (Add 1) + Effect.Safe.perform h (Add 2))
      ()
      { retc = (fun x -> x + 1); exnc = raise; effc; tickc }
  in
  assert (r = 31)

(* The exception path goes through [exnc]. *)
let test_exception () =
  let r =
    Preemptible.Safe.match_with
      (fun _h () -> raise Boom)
      ()
      { retc = (fun _ -> "returned")
      ; exnc =
          (function
          | Boom -> "caught"
          | e -> raise e)
      ; effc
      ; tickc
      }
  in
  assert (r = "caught")

(* An unhandled effect passes through to the enclosing handler. *)
type _ Effect.t += Other : int Effect.t

let test_forward () =
  let r =
    try_with
      (fun () ->
        Preemptible.Safe.match_with
          (fun h () -> Effect.Safe.perform h Other)
          ()
          { retc = (fun x -> x); exnc = raise; effc; tickc })
      ()
      { effc =
          (fun (type a) (e : a Effect.t) ->
            match e with
            | Other -> Some (fun (k : (a, _) continuation) -> continue k 7)
            | _ -> None)
      }
  in
  assert (r = 7)

(* [try_with] takes the tick handler as a labelled argument instead. *)
let test_try_with () =
  let r =
    Preemptible.Safe.try_with
      ~on_tick:tickc
      (fun h () -> Effect.Safe.perform h (Add 4))
      ()
      { effc }
  in
  assert (r = 40)

let () =
  test_value ();
  test_exception ();
  test_forward ();
  test_try_with ();
  assert (not !ticked);
  print_endline "OK"
