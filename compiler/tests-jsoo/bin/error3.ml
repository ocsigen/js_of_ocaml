let () =
  match Array.to_list Sys.argv with
  | _ :: "unregister" :: _ ->
      let null = Array.unsafe_get [| 1 |] 1 in
      Callback.register "Printexc.handle_uncaught_exception" null
  | _ -> ()

let null : _ -> _ -> _ = Array.unsafe_get [||] 0

let _ = null 1 2
