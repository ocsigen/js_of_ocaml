(* The runtime must implement the system constants, which are usually
   computed statically (here, compiled with --disable staticeval) *)

external backend_type : unit -> Sys.backend_type = "caml_sys_const_backend_type"

external big_endian : unit -> bool = "caml_sys_const_big_endian"

external naked_pointers_checked : unit -> bool = "caml_sys_const_naked_pointers_checked"

let () =
  Printf.printf
    "%s %b %b\n"
    (match backend_type () with
    | Native -> "native"
    | Bytecode -> "bytecode"
    | Other _ -> "other")
    (big_endian ())
    (naked_pointers_checked ())
