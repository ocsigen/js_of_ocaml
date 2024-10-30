external unregister : string -> unit = "caml_unregister_named_value"

let () =
  match Array.to_list Sys.argv with
  | _ :: "unregister" :: _ -> unregister "Printexc.handle_uncaught_exception"
  | _ -> ()

exception D of int * string * Int64.t

let _ =
  Printexc.register_printer (function
    | D _ -> Some "custom printer"
    | _ -> None)

let _ = raise (D (2, "test", 43L))
