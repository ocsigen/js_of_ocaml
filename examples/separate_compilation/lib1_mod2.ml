let hi () = print_endline "hi"

let () = at_exit (fun () -> print_endline "Lib1_mod2 was linked")
