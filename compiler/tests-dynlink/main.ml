let () = print_endline "hello"

let () = Dynlink.loadfile "/static/plugin.cmo"
