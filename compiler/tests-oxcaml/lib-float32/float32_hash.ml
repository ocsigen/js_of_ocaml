(* float32_hash must normalize negative zero (and NaNs) so that equal
   float32 values hash identically, like caml_hash_mix_float. *)
open Stdlib_stable

let () =
  let pos_zero = Float32.of_float 0.0 in
  let neg_zero = Float32.of_float (-0.0) in
  Printf.printf "zero: %b\n" (Hashtbl.hash pos_zero = Hashtbl.hash neg_zero)
