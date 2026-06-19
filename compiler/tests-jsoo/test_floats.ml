(* Js_of_ocaml compiler
 * http://www.ocsigen.org/js_of_ocaml/
 * Copyright (C) 2019 Hugo Heuzard
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU Lesser General Public License as published by
 * the Free Software Foundation, with linking exception;
 * either version 2.1 of the License, or (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
 *)

let%expect_test _ =
  (* copied from https://github.com/ocaml/ocaml/pull/1794 *)
  let z =
    let x = -0. and y = 0. in
    if mod_float x 1. >= 0. then x else if false then x else y
  in
  Printf.printf "%g\n" (1. /. z);
  [%expect {|-inf|}]

let print f =
  match Float.classify_float f with
  | FP_nan -> print_endline "nan"
  | _ -> Printf.printf "%f\n" f

let%expect_test "acosh" =
  let p x = print (Float.acosh x) in
  p (-1.0);
  [%expect {| nan |}];
  p 0.0;
  [%expect {| nan |}];
  p 0.5;
  [%expect {| nan |}];
  p 1.0;
  [%expect {| 0.000000 |}];
  p 2.0;
  [%expect {| 1.316958 |}]

let%expect_test "asinh" =
  let p x =
    let r = Float.asinh x in
    match Float.classify_float r with
    (* asinh(0) returns (-0) on windows *)
    | FP_zero -> print 0.
    | _ -> print r
  in
  p 1.0;
  [%expect {| 0.881374 |}];
  p 0.0;
  [%expect {| 0.000000 |}];
  p (-1.0);
  [%expect {| -0.881374 |}];
  p 2.0;
  [%expect {| 1.443635 |}]

let%expect_test "atanh" =
  let p x = print (Float.atanh x) in
  p (-2.0);
  [%expect {| nan |}];
  p (-1.0);
  [%expect {| -inf |}];
  p 0.0;
  [%expect {| 0.000000 |}];
  p 0.5;
  [%expect {| 0.549306 |}];
  p 1.0;
  [%expect {| inf |}]

let%expect_test "erf" =
  let p x = print (Float.erf x) in
  p (-2.0);
  [%expect {| -0.995322 |}];
  p (-1.0);
  [%expect {| -0.842701 |}];
  p 0.0;
  [%expect {| 0.000000 |}];
  p 0.5;
  [%expect {| 0.520500 |}];
  p 1.0;
  [%expect {| 0.842701 |}];
  p 10.0;
  [%expect {| 1.000000 |}]

let%expect_test "erfc" =
  let p x = print (Float.erfc x) in
  p (-2.0);
  [%expect {| 1.995322 |}];
  p (-1.0);
  [%expect {| 1.842701 |}];
  p 0.0;
  [%expect {| 1.000000 |}];
  p 0.5;
  [%expect {| 0.479500 |}];
  p 1.0;
  [%expect {| 0.157299 |}];
  p 10.0;
  [%expect {| 0.000000 |}]

let%expect_test "cbrt" =
  let p x = print (Float.cbrt x) in
  p Float.nan;
  [%expect {| nan |}];
  p (-1.0);
  [%expect {| -1.000000 |}];
  p (-0.0);
  [%expect {| -0.000000 |}];
  p Float.neg_infinity;
  [%expect {| -inf |}];
  p 0.0;
  [%expect {| 0.000000 |}];
  p 1.0;
  [%expect {| 1.000000 |}];
  p Float.infinity;
  [%expect {| inf |}];
  p 2.0;
  [%expect {| 1.259921 |}]

let%expect_test "exp2" =
  let p x = print (Float.exp2 x) in
  p Float.nan;
  [%expect {| nan |}];
  p (-1.0);
  [%expect {| 0.500000 |}];
  p (-0.0);
  [%expect {| 1.000000 |}];
  p Float.neg_infinity;
  [%expect {| 0.000000 |}];
  p 0.0;
  [%expect {| 1.000000 |}];
  p 1.0;
  [%expect {| 2.000000 |}];
  p Float.infinity;
  [%expect {| inf |}];
  p 2.0;
  [%expect {| 4.000000 |}]

let%expect_test "log2" =
  let p x = print (Float.log2 x) in
  p 3.0;
  [%expect {| 1.584963 |}];
  p 2.0;
  [%expect {| 1.000000 |}];
  p 1.0;
  [%expect {| 0.000000 |}];
  p 0.0;
  [%expect {| -inf |}];
  p (-2.0);
  [%expect {| nan |}];
  p 1024.0;
  [%expect {| 10.000000 |}]

let print' f = try print (f ()) with e -> print_endline (Printexc.to_string e)

let%expect_test "of_string" =
  let x = "0x1.1" in
  print' (fun () -> float_of_string x);
  [%expect {| 1.062500 |}];
  let x = "0x1.1p-1" in
  print' (fun () -> float_of_string x);
  [%expect {| 0.531250 |}];
  let x = "  0x1.1" in
  print' (fun () -> float_of_string x);
  [%expect {| 1.062500 |}];
  let x = "  0x1.1 " in
  print' (fun () -> float_of_string x);
  [%expect {| Failure("float_of_string") |}];
  let x = "0x1.1 p-1" in
  print' (fun () -> float_of_string x);
  [%expect {| Failure("float_of_string") |}];
  let x = " -0x1.1" in
  print' (fun () -> float_of_string x);
  [%expect {| -1.062500 |}];
  let x = " +0x1.1" in
  print' (fun () -> float_of_string x);
  [%expect {| 1.062500 |}];
  let x = " -inf" in
  print' (fun () -> float_of_string x);
  [%expect {| -inf |}];
  let x = " +inf" in
  print' (fun () -> float_of_string x);
  [%expect {| inf |}];
  let x = " nan" in
  print' (fun () -> float_of_string x);
  [%expect {| nan |}]

let%expect_test "of_string" =
  let x = "3.14" in
  print' (fun () -> float_of_string x);
  [%expect {| 3.140000 |}];
  let x = " 3.14" in
  print' (fun () -> float_of_string x);
  [%expect {| 3.140000 |}];
  let x = "\t3.14" in
  print' (fun () -> float_of_string x);
  [%expect {| 3.140000 |}];
  let x = "3. 14" in
  print' (fun () -> float_of_string x);
  [%expect {| Failure("float_of_string") |}];
  let x = "3.1 4" in
  print' (fun () -> float_of_string x);
  [%expect {| Failure("float_of_string") |}];
  let x = "3.14 " in
  print' (fun () -> float_of_string x);
  [%expect {| Failure("float_of_string") |}]

(* [caml_parse_float] used to trap on "nin" (the nan/inf detection read
   past the end of the string) and to accept JavaScript binary/octal
   literals like "0b101"/"0o17"; native raises [Failure]. *)
let%expect_test "float_of_string rejects bad input" =
  let p s = print' (fun () -> float_of_string s) in
  p "nin";
  [%expect {| Failure("float_of_string") |}];
  p "nif";
  [%expect {| Failure("float_of_string") |}];
  p "ina";
  [%expect {| Failure("float_of_string") |}];
  p "0b101";
  [%expect {| Failure("float_of_string") |}];
  p "0o17";
  [%expect {| Failure("float_of_string") |}];
  p "1d0";
  [%expect {| Failure("float_of_string") |}];
  p "";
  [%expect {| Failure("float_of_string") |}];
  (* Valid values must still parse. *)
  p "1.5";
  [%expect {| 1.500000 |}];
  p "1e3";
  [%expect {| 1000.000000 |}];
  p ".5";
  [%expect {| 0.500000 |}];
  p "1_000.5";
  [%expect {| 1000.500000 |}];
  p "nan";
  [%expect {| nan |}];
  p "-inf";
  [%expect {| -inf |}];
  p "0x1p4";
  [%expect {| 16.000000 |}]

let%expect_test "floatarray ops keep tag 254" =
  let a = Float.Array.make 3 1.5 in
  let p label fa = Printf.printf "%s=%d\n" label (Obj.tag (Obj.repr fa)) in
  p "make" a;
  p "sub" (Float.Array.sub a 0 2);
  p "append" (Float.Array.append a a);
  p "concat" (Float.Array.concat [ a; a ]);
  [%expect {|
    make=254
    sub=254
    append=254
    concat=254
    |}]

let%expect_test "Array.make negative length" =
  let n = Sys.opaque_identity (-1) in
  (try
     let a = Array.make n 0 in
     Printf.printf "len=%d\n" (Array.length a)
   with Invalid_argument m -> print_endline m);
  [%expect {| Array.make |}]

(* hash.c has a dedicated Double_array_tag case (elements mixed
   directly, no header word) and skips Abstract_tag blocks. *)
let%expect_test "Hashtbl.hash of float arrays" =
  let p (x : Obj.t) = Printf.printf "%08x\n" (Hashtbl.hash x) in
  p (Obj.repr [| 1.5 |]);
  p (Obj.repr [| 1.5; 2.5 |]);
  (* A long float array exercises the per-element budget (only the first
     elements are mixed before [num] runs out). Build it with
     [create_float] so the runtime representation is a flat float array
     (Double_array_tag) on every OCaml version: [Array.init]'s result is
     only unboxed by some stdlib versions, leaving the array generic and
     the hash version-dependent. *)
  let long = Array.create_float 20 in
  for i = 0 to Array.length long - 1 do
    long.(i) <- float_of_int i
  done;
  p (Obj.repr long);
  p (Obj.repr [| nan |]);
  p (Obj.repr [| -0. |]);
  p (Obj.repr (1, [| 1.5 |], 2));
  p (Obj.new_block 251 3);
  [%expect
    {|
    30d3d2e3
    07e28e3a
    1ab69813
    3228858d
    0f478b8c
    3498f980
    00000000
    |}]

(* [ldexp] must round once: scaling in several steps can round twice
   on the way into the subnormal range. *)
let%expect_test "ldexp into subnormals" =
  let p x = Printf.printf "%h\n" x in
  p (ldexp (1. +. epsilon_float) (-1075));
  p (ldexp 1. (-1074));
  p (ldexp max_float (-2098));
  p (ldexp 1. (-1075));
  p (ldexp 1. 1024);
  [%expect
    {|
    0x0.0000000000001p-1022
    0x0.0000000000001p-1022
    0x0.0000000000001p-1022
    0x0p+0
    infinity
    |}]

(* Hex float literals must be parsed exactly: scaling the mantissa
   with a single multiplication underflows for exponents below -1074
   and double-rounds mantissas wider than 53 bits. *)
let%expect_test "hex float literals" =
  let p s =
    match float_of_string s with
    | x -> Printf.printf "%h\n" x
    | exception Failure _ -> print_endline "failure"
  in
  p "0x1.123456789abcdp-1023";
  p "0x10p-1078";
  p "0x1.00000000000008p0";
  p "0x1.000000000000080000000000001p0";
  p "0x.8p1";
  p "0x8.p0";
  p "0x1.fffffffffffff8p1023";
  p "0x1.fffffffffffff7p1023";
  p "-0x0p0";
  p "0xp1";
  p "0x1p";
  [%expect
    {|
    0x0.891a2b3c4d5e6p-1022
    0x0.0000000000001p-1022
    0x1p+0
    0x1.0000000000001p+0
    0x1p+0
    0x1p+3
    infinity
    0x1.fffffffffffffp+1023
    -0x0p+0
    failure
    failure
    |}]

(* A hex-float exponent that overflows the accumulator must saturate to
   infinity / 0, not wrap around modulo 2^32. "0x1p12884901890" used to
   return 4.0 on the Wasm runtime (the exponent wrapped to 2). *)
let%expect_test "hex float exponent overflow" =
  let p s =
    match float_of_string s with
    | x -> Printf.printf "%h\n" x
    | exception Failure _ -> print_endline "failure"
  in
  p "0x1p12884901890";
  p "0x1p-12884901890";
  p "0x1p99999999999";
  p "0x1p-99999999999";
  p "0x1p1024";
  p "0x1p-1075";
  [%expect
    {|
    infinity
    0x0p+0
    infinity
    0x0p+0
    infinity
    0x0p+0
    |}]

(* Formatting with precisions above 100 (toFixed/toExponential's
   limit) and %f of values >= 1e21 must print the exact decimal
   expansion like native. *)
let%expect_test "format with large precision" =
  let p (fmt : _ format) x =
    match Printf.sprintf fmt x with
    | s -> print_endline s
    | exception Failure _ -> print_endline "failure"
  in
  p "%.150e" 0.1;
  p "%.110f" 0.1;
  p "%.0f" 1.2345678e22;
  p "%.2f" 1.2345678e22;
  p "%.150e" 2.;
  p "%.110e" 0.;
  p "%.105g" 0.1;
  [%expect
    {|
    1.000000000000000055511151231257827021181583404541015625000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000e-01
    0.10000000000000000555111512312578270211815834045410156250000000000000000000000000000000000000000000000000000000
    12345678000000000327680
    12345678000000000327680.00
    2.000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000e+00
    0.00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000e+00
    0.1000000000000000055511151231257827021181583404541015625
    |}]
