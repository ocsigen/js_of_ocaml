let hello () = print_endline "hello"

let () = at_exit (fun () -> print_endline "Lib1_mod1 was linked")
