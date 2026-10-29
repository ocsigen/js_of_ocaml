(* A primitive provided by an extra runtime file (extra_runtime.wat), in
   separate compilation: the unused import of that file must not end up in
   the runtime, even without Binaryen to remove it. *)

external identity : int -> int = "caml_extra_identity"

let () = Printf.printf "%d\n" (identity 42)
