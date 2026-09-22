open Effect
open Effect.Deep

type _ Effect.t += Dummy : unit t

(* A handler installed inside [assume_no_perform] re-enables effects for
   its own body, as documented in [Effect_js.assume_no_perform]. *)
let nested_handler () =
  let handler =
    { effc =
        (fun (type a) (e : a Effect.t) ->
          match e with
          | Dummy -> Some (fun (k : (a, _) continuation) -> continue k ())
          | _ -> None)
    }
  in
  match
    Jsoo_runtime.Effect.assume_no_perform (fun () ->
        try_with
          (fun () ->
            perform Dummy;
            "handled")
          ()
          handler)
  with
  | "handled" -> ()
  | s ->
      print_endline ("failed: " ^ s);
      exit 2
  | exception Effect.Unhandled Dummy ->
      print_endline "failed: Effect.Unhandled";
      exit 2

(* A handler that lets the effect through at toplevel must raise
   [Effect.Unhandled] at the point where the effect was performed. *)
let unhandled_reperform () =
  match
    try_with
      (fun () ->
        match perform Dummy with
        | () -> "performed"
        | exception Effect.Unhandled Dummy -> "raised at perform")
      ()
      { effc = (fun (type a) (_ : a Effect.t) -> None) }
  with
  | "raised at perform" -> ()
  | s ->
      print_endline ("failed: " ^ s);
      exit 2
  | exception Effect.Unhandled Dummy ->
      print_endline "failed: Effect.Unhandled escaped";
      exit 2

let () =
  nested_handler ();
  unhandled_reperform ();
  print_endline "ok"
