(* Corner cases of the integer/float formatting primitives that [Printf]
   never produces but that are reachable by calling the externals directly.
   Run on js, wasm and native (the oracle). *)

external format_int : string -> int -> string = "caml_format_int"

external format_int64 : string -> int64 -> string = "caml_int64_format"

external format_float : string -> float -> string = "caml_format_float"

(* Native printf on Windows (mingw) prints exponents with three digits
   ("1.5e+000"); glibc and the js/wasm runtimes use the minimal two
   ("1.5e+00"). Collapse to (at least) two digits so the native oracle
   agrees on every platform. *)
let normalize_exp s =
  match String.index_opt s 'e' with
  | Some i when i + 1 < String.length s && (s.[i + 1] = '+' || s.[i + 1] = '-') ->
      let exp = String.sub s (i + 2) (String.length s - i - 2) in
      let n = String.length exp in
      let j = ref 0 in
      while !j < n - 2 && exp.[!j] = '0' do
        incr j
      done;
      String.sub s 0 (i + 2) ^ String.sub exp !j (n - !j)
  | Some _ | None -> s

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
  let p fmt x = print_endline (normalize_exp (format_float fmt x)) in
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
