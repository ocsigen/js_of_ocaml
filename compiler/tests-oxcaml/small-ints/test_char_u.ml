(* TEST *)

(* Untagged char literals.  A [char#] literal above 127 has to behave like
   the same character built at run time: equal to it, ordered the same way,
   and with [code] reading back the unsigned value.  Nothing else covers
   [char#]. *)

module Char_u = Stdlib_stable.Char_u

let nul : char# = #'\x00'

let del : char# = #'\x7f'

let high : char# = #'\x80'

let top : char# = #'\xff'

(* [Sys.opaque_identity] keeps these out of the constant path, so each
   comparison below pits a constant against a computed value. *)
let computed c = Char_u.of_char (Sys.opaque_identity c)

let check lit ch code =
  assert (Char_u.code lit = code);
  assert (Char_u.equal lit (computed ch));
  assert (Char_u.compare lit (computed ch) = 0);
  assert (Char_u.to_char lit = ch)

let () =
  check nul '\x00' 0;
  check del '\x7f' 127;
  (* The cases the sign extension actually changes. *)
  check high '\x80' 128;
  check top '\xff' 255;
  (* Distinct high literals stay distinct. *)
  assert (not (Char_u.equal high top));
  assert (Char_u.compare high top < 0);
  assert (Char_u.compare top high > 0);
  (* And a high literal is not confused with a low one. *)
  assert (not (Char_u.equal high nul));
  print_endline "OK"
