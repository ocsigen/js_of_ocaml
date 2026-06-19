(* float32_hash must normalize negative zero (and NaNs) so that equal
   float32 values hash identically, like caml_hash_mix_float. *)
open Stdlib_stable

let () =
  let pos_zero = Float32.of_float 0.0 in
  let neg_zero = Float32.of_float (-0.0) in
  Printf.printf "zero: %b\n" (Hashtbl.hash pos_zero = Hashtbl.hash neg_zero);
  (* Distinct NaN bit patterns (different mantissa and sign) are all equal as
     float32 NaNs, so they must hash identically too. *)
  let nan1 = Float32.of_bits 0x7fc00000l in
  let nan2 = Float32.of_bits 0xff800001l in
  Printf.printf "nan: %b\n" (Hashtbl.hash nan1 = Hashtbl.hash nan2)
