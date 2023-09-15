external unregister : string -> unit = "caml_unregister_named_value"

let () =
  (* Make sure Printexc is linked *)
  let _ = Printexc.to_string Not_found in
  match Array.to_list Sys.argv with
  | _ :: "unregister" :: _ -> unregister "Printexc.handle_uncaught_exception"
  | _ -> ()

[@@@ocaml.warning "-8"]

let _ =
  match 3 with
  | 2 -> ()
