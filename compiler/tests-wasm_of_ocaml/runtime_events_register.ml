(* Regression test for [caml_runtime_events_user_register] (wasm runtime,
   runtime_events.wat). The event record { id; name; typ; tag } was built
   with [typ] and [tag] swapped, so [Runtime_events.User.tag] returned
   the event type instead of the tag. *)

type Runtime_events.User.tag += My_tag

let () =
  let ev = Runtime_events.User.register "myev" My_tag Runtime_events.Type.span in
  assert (Runtime_events.User.name ev = "myev");
  match Runtime_events.User.tag ev with
  | My_tag -> ()
  | _ -> assert false
