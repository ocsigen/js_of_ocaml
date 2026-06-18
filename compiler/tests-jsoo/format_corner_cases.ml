(* Corner cases of the integer/float formatting primitives that [Printf]
   never produces but that are reachable by calling the externals directly.
   Run on js, wasm and native (the oracle). *)

external format_int : string -> int -> string = "caml_format_int"

external format_int64 : string -> int64 -> string = "caml_int64_format"

external format_float : string -> float -> string = "caml_format_float"

(* The "#" flag is ignored for base 10 (as in C) and must not overwrite the
   sign of a negative number; for octal/hex it adds the 0/0x prefix. *)
let%expect_test "format_int alternate flag and sign" =
  let p fmt n = print_endline (format_int fmt n) in
  p "%#d" (-5);
  p "%#d" 5;
  p "%#x" 255;
  p "%#o" 8;
  p "%#d" 0;
  [%expect {|
    -5
    5
    0xff
    010
    0
    |}]

let%expect_test "format_int64 alternate flag and sign" =
  let p fmt n = print_endline (format_int64 fmt n) in
  p "%#d" (-5L);
  p "%#x" 255L;
  p "%#o" 8L;
  [%expect {|
    -5
    0xff
    010
    |}]

(* "%+f" / "% f" with no precision: the conversion letter must still be
   recognised (it used to be mistaken for the sign flag and raise). *)
let%expect_test "format_float sign flag without precision" =
  let p fmt x = print_endline (format_float fmt x) in
  p "%+f" 1.5;
  p "% f" 1.5;
  p "%+e" 1.5;
  p "%+g" 1.5;
  [%expect {|
    +1.500000
     1.500000
    +1.500000e+00
    +1.5
    |}]
