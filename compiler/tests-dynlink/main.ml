let () = print_endline "hello"

let () = Dynlink.loadfile "./plugin.cmo"
