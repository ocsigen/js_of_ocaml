let () =
  match Array.to_list Sys.argv with
  | _ :: "unregister" :: _ ->
      let null = Array.unsafe_get [| 1 |] 1 in
      Callback.register "Printexc.handle_uncaught_exception" null
  | _ -> ()

exception D of int * string * Int64.t

let _ =
  Printexc.register_printer (function
      | D _ -> Some "custom printer"
      | _ -> None)

let _ = raise (D (2, "test", 43L))
