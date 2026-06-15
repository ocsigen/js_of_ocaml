(* Regression test for [caml_parse_float] (wasm runtime, float.wat).

   - "nin" used to trap with an uncatchable exception: the "nan" branch
     advanced the cursor, then the "inf" check -- which was not an [else]
     of the "nan" check -- read past the end of the string.
   - "0b101"/"0o17" were accepted through JS number coercion (returning
     5. / 15.) where the native runtime raises [Failure]. *)

let fails s =
  match float_of_string s with
  | (_ : float) -> false
  | exception Failure _ -> true

let () =
  (* Used to trap instead of raising. *)
  assert (fails "nin");
  assert (fails "nif");
  assert (fails "ina");
  (* Used to be wrongly accepted. *)
  assert (fails "0b101");
  assert (fails "0o17");
  assert (fails "1d0");
  assert (fails "");
  (* Valid values must still parse. *)
  assert (float_of_string "1.5" = 1.5);
  assert (float_of_string "1e3" = 1000.);
  assert (float_of_string ".5" = 0.5);
  assert (float_of_string "5." = 5.);
  assert (float_of_string "-2.5" = -2.5);
  assert (float_of_string "1_000.5" = 1000.5);
  assert (Float.is_nan (float_of_string "nan"));
  assert (Float.is_nan (float_of_string "-nan"));
  assert (float_of_string "inf" = infinity);
  assert (float_of_string "-inf" = neg_infinity);
  assert (float_of_string "infinity" = infinity);
  assert (float_of_string "0x1p4" = 16.)
