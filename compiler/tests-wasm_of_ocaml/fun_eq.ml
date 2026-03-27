let () =
  let f = (fun () -> ()) == fun () -> () in
  Printf.printf "%b\n" f
