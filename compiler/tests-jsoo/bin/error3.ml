[@@@ocaml.alert "-unsafe_multidomain"]

external register : string -> 'a -> unit = "caml_register_named_value"
external unregister : string -> unit = "caml_unregister_named_value"

let () =
  (* Push "Printexc.handle_uncaught_exception" out of the head of its hash
     bucket, so unregistering it must remove a non-head chain entry. *)
  for i = 0 to 199 do
    register (Printf.sprintf "nv_test_%d" i) (Obj.repr (ref i))
  done;
  match Array.to_list Sys.argv with
  | _ :: "unregister" :: _ -> unregister "Printexc.handle_uncaught_exception"
  | _ -> ()

exception D of int * string * Int64.t

let _ =
  Printexc.register_printer (function
    | D _ -> Some "custom printer"
    | _ -> None)

let _ = raise (D (2, "test", 43L))
