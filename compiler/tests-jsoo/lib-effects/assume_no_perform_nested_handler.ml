open Effect
open Effect.Deep

type _ Effect.t += Dummy : unit t

let () =
  try_with
    (fun () ->
      Jsoo_runtime.Effect.assume_no_perform (fun () ->
          try_with (fun () -> ()) () { effc = (fun (type a) (_ : a Effect.t) -> None) });
      perform Dummy)
    ()
    { effc =
        (fun (type a) (e : a Effect.t) ->
          match e with
          | Dummy ->
              Some
                (fun (k : (a, _) continuation) ->
                  print_endline "ok";
                  continue k ())
          | _ -> None)
    }
