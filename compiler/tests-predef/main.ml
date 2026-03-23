let () =
  Printf.printf "%d\n" Not_found.x;
  match Sys.getenv "DOES_NOT_EXIST" with
  | _ -> ()
  | exception Not_found -> print_endline "caught Not_found"
