open Js_of_ocaml
open Effect
open Effect.Deep

type _ Effect.t += Dummy : unit t

(* Effects cannot be performed across JavaScript frames: as with C frames
   in the native OCaml runtime, this raises [Effect.Unhandled] inside the
   callback, whether or not a handler is installed outside the frame. *)
let callback : (unit -> unit) Js.callback =
  Js.wrap_callback (fun () ->
      match perform Dummy with
      | () ->
          print_endline "failed: handled across a JavaScript frame";
          exit 2
      | exception Effect.Unhandled Dummy -> ())

let call () = ignore (Js.Unsafe.fun_call callback [||])

let () =
  call ();
  try_with
    call
    ()
    { effc =
        (fun (type a) (e : a Effect.t) ->
          match e with
          | Dummy ->
              Some
                (fun (_ : (a, _) continuation) ->
                  print_endline "failed: outer handler ran";
                  exit 2)
          | _ -> None)
    };
  (* A handler installed inside the callback works as usual. *)
  let inner : (unit -> unit) Js.callback =
    Js.wrap_callback (fun () ->
        try_with
          (fun () -> perform Dummy)
          ()
          { effc =
              (fun (type a) (e : a Effect.t) ->
                match e with
                | Dummy -> Some (fun (k : (a, _) continuation) -> continue k ())
                | _ -> None)
          })
  in
  ignore (Js.Unsafe.fun_call inner [||]);
  print_endline "ok"
