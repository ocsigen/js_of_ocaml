let () =
  (* Make sure Printexc is linked *)
  let _ = Printexc.to_string Not_found in
  match Array.to_list Sys.argv with
  | _ :: "unregister" :: _ ->
      let null = Array.unsafe_get [| 1 |] 1 in
      Callback.register "Printexc.handle_uncaught_exception" null
  | _ -> ()

[@@@ocaml.warning "-8"]

let _ =
  match 3 with
  | 2 -> ()
