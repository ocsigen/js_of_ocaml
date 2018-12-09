let () =
  match Sys.getenv "A" with
  | "A" -> ()
  | _ -> assert false

let () =
  match Sys.getenv "B" with
  | exception Not_found -> ()
  | _ -> assert false

let () =
  match Sys.getenv_opt "A" with
  | Some "A" -> ()
  | _ -> assert false

let () =
  match Sys.getenv_opt "B" with
  | None -> ()
  | Some _ -> assert false


let () = assert("\u{00b1}" = (Sys.getenv "D"))
