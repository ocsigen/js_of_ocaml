open Effect
open Effect.Deep

type _ Effect.t += Dummy : unit t

let must_raise () =
  try_with
    (fun () ->
      Js_of_ocaml.Effect_js.assume_no_perform (fun () ->
          (* Should raise [Effect.Unhandled] despite the installed handler *)
          perform Dummy))
    ()
    { effc =
        (fun (type a) (e : a Effect.t) ->
          match e with
          | Dummy -> Some (fun (k : (a, _) continuation) -> continue k ())
          | _ -> None)
    }

let () =
  try
    must_raise ();
    print_endline "failed";
    exit 2
  with Effect.Unhandled Dummy -> print_endline "ok"
