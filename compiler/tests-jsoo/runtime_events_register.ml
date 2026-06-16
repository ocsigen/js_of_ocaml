(* Regression test for [caml_runtime_events_user_register]. The wasm runtime
   (runtime_events.wat) built the event record { id; name; typ; tag } with
   [typ] and [tag] swapped, so [Runtime_events.User.tag] returned the event
   type instead of the tag. Running under js, wasm and native checks the jsoo
   runtimes against the native behaviour. *)

type Runtime_events.User.tag += My_tag

let () =
  let ev = Runtime_events.User.register "myev" My_tag Runtime_events.Type.span in
  assert (Runtime_events.User.name ev = "myev");
  match Runtime_events.User.tag ev with
  | My_tag -> ()
  | _ -> assert false
