external a : unit -> string = "test_a"

external b : unit -> string = "test_b"

external c : unit -> string = "test_c"

let () = print_endline (a ())

let () = print_endline (b ())

let () = print_endline (c ())
