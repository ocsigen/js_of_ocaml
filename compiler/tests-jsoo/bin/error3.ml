external unregister : string -> unit = "caml_unregister_named_value"

let () =
  match Array.to_list Sys.argv with
  | _ :: "unregister" :: _ -> unregister "Printexc.handle_uncaught_exception"
  | _ -> ()

let _ = (Obj.magic Js_of_ocaml.Js.null : int -> int -> unit) 1 2
