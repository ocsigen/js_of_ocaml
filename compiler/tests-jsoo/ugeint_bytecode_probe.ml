(* Used only by the bytecode-presence dune rule to confirm that
   OxCaml emits standalone UGEINT for [%int_unsigned_greaterequal].
   See [test_ugeint.ml] for the behavioural regression test. *)

external uge : int -> int -> bool = "%int_unsigned_greaterequal"

let () = if uge (Sys.opaque_identity 5) (Sys.opaque_identity 5) then print_endline "y"
