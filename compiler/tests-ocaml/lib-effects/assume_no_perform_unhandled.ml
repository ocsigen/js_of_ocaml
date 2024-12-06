open Printf
open Effect
open Effect.Deep

type _ Effect.t += Dummy : unit t

let f () =
  try_with
    (fun () ->
      Js_of_ocaml.Js.Effect.assume_no_perform (fun () ->
        perform Dummy
      )
    )
    ()
    { effc =
        (fun (type a) (e : a Effect.t) ->
          match e with
          | Dummy -> Some (fun (k : (a, _) continuation) -> continue k ())
          | _ -> None)
    }

let () =
  try
    (* When double translation is not enabled, [f] should not raise *)
    f (); print_endline "ok"
  with Effect.Unhandled Dummy -> (
    print_endline "failed";
    exit 2
  )
