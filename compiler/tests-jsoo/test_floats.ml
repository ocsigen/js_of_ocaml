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

let%expect_test "erf and erfc accuracy" =
  (* Reference values computed with 40 significant digits, then rounded.
     The JavaScript and Wasm runtimes stay within 2.4 ulps of the exact
     values; a crude approximation used to be off by up to 1e-7 in
     absolute terms, so that erf 0 <> 0 and erfc 10 = 0. *)
  let ulp x =
    let x = Float.abs x in
    Float.succ x -. x
  in
  let check name f x expected =
    let r = f x in
    if not (Float.abs (r -. expected) <= 4. *. ulp expected)
    then Printf.printf "%s %h = %h, expected %h\n" name x r expected
  in
  List.iter
    (fun (x, erf, erfc) ->
      check "erf" Float.erf x erf;
      check "erfc" Float.erfc x erfc)
    [ 1e-300, 1.1283791670955126e-300, 1.0
    ; 1e-10, 1.1283791670955126e-10, 0.999999999887162
    ; 0.1, 0.1124629160182849, 0.887537083981715
    ; 0.5, 0.5204998778130465, 0.4795001221869535
    ; 0.8, 0.7421009647076605, 0.2578990352923395
    ; 0.9, 0.7969082124228322, 0.20309178757716786
    ; 1.0, 0.8427007929497149, 0.15729920705028513
    ; 1.2, 0.9103139782296353, 0.08968602177036464
    ; 1.5, 0.9661051464753108, 0.033894853524689274
    ; 2.0, 0.9953222650189527, 0.004677734981047266
    ; 2.5, 0.999593047982555, 0.0004069520174449589
    ; 3.0, 0.9999779095030014, 2.209049699858544e-05
    ; 4.0, 0.9999999845827421, 1.541725790028002e-08
    ; 5.0, 0.9999999999984626, 1.537459794428035e-12
    ; 5.9, 0.9999999999999999, 7.190409783550478e-17
    ; 6.5, 1.0, 3.8421483271206475e-20
    ; 10.0, 1.0, 2.088487583762545e-45
    ; 15.0, 1.0, 7.212994172451207e-100
    ; 20.0, 1.0, 5.395865611607901e-176
    ; 26.0, 1.0, 5.663192408856143e-296
    ; 26.5, 1.0, 2.2109076642637343e-307
    ; 27.2, 1.0, 1e-323
    ; 27.5, 1.0, 0.0
    ; -1e-10, -1.1283791670955126e-10, 1.000000000112838
    ; -0.5, -0.5204998778130465, 1.5204998778130465
    ; -1.0, -0.8427007929497149, 1.8427007929497148
    ; -3.0, -0.9999779095030014, 1.9999779095030015
    ; -10.0, -1.0, 2.0
    ];
  [%expect {||}];
  List.iter
    (fun x -> Printf.printf "%h: erf %h, erfc %h\n" x (Float.erf x) (Float.erfc x))
    [ 0.; -0.; infinity; neg_infinity; nan; 0x1p-1074 ];
  [%expect
    {|
    0x0p+0: erf 0x0p+0, erfc 0x1p+0
    -0x0p+0: erf -0x0p+0, erfc 0x1p+0
    infinity: erf 0x1p+0, erfc 0x0p+0
    -infinity: erf -0x1p+0, erfc 0x1p+1
    nan: erf nan, erfc nan
    0x0.0000000000001p-1022: erf 0x0.0000000000001p-1022, erfc 0x1p+0
    |}]

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
  (* Negative values rounding to the smallest subnormal or to zero
     (wasi-libc's strtod returned +0 for them) *)
  let x = "-2.47032822920623272089e-324" in
  print_string (Printf.sprintf "%h" (float_of_string x));
  [%expect {| -0x0.0000000000001p-1022 |}];
  let x =
    "-2.4703282292062327208828439643411068618252990130716238221279284125033775363510437593264991818081799618989828234772285886546332835517796989819938739800539093906315035659515570226392290858392449105184435931802849936536152500319370457678249219365623669863658480757001585769269903706311928279558551332927834338409351978015531246597263579574622766465272827220056374006485499977096599470454020828166226237857393450736339007967761930577506740176324673600968951340535537458516661134223766678604162159680461914467291840300530057530849048765391711386591646239524912623653881879636239373280423891018672348497668235089863388587925628302755995657524455507255189313690836254779186948667994968324049705821028513185451396213837722826145437693412532098591327667236328125e-324"
  in
  print_string (Printf.sprintf "%h" (float_of_string x));
  [%expect {| -0x0p+0 |}];
  let x = "0x1p-+5" in
  print' (fun () -> float_of_string x);
  [%expect {| Failure("float_of_string") |}];
  let x = "0x1p+-5" in
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

(* %h of extreme values (the JS runtime used to normalize them with a
   loop of up to 1023 multiplications or divisions by 2) *)
let%expect_test "%h of extreme values" =
  let p x = Printf.printf "%h %.3h %.0h %+.1h\n" x x x x in
  p max_float;
  p (-.max_float);
  p 1e300;
  p min_float;
  p (Float.pred min_float);
  p 5e-324;
  p (-5e-324);
  p 1e-300;
  p (-0.);
  [%expect
    {|
    0x1.fffffffffffffp+1023 0x2.000p+1023 0x2p+1023 +0x2.0p+1023
    -0x1.fffffffffffffp+1023 -0x2.000p+1023 -0x2p+1023 -0x2.0p+1023
    0x1.7e43c8800759cp+996 0x1.7e4p+996 0x1p+996 +0x1.8p+996
    0x1p-1022 0x1.000p-1022 0x1p-1022 +0x1.0p-1022
    0x0.fffffffffffffp-1022 0x1.000p-1022 0x1p-1022 +0x1.0p-1022
    0x0.0000000000001p-1022 0x0.000p-1022 0x0p-1022 +0x0.0p-1022
    -0x0.0000000000001p-1022 -0x0.000p-1022 -0x0p-1022 -0x0.0p-1022
    0x1.56e1fc2f8f359p-997 0x1.56ep-997 0x1p-997 +0x1.5p-997
    -0x0p+0 -0x0.000p+0 -0x0p+0 -0x0.0p+0
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

let%expect_test "hexadecimal printing" =
  (* No point with precision 0, mantissa rounded to nearest with ties to
     even, and sign or style character for infinities and NaNs, as in C *)
  let b = Int64.float_of_bits in
  Printf.printf "%.0h %+.0H % .0h %.0h %.0h\n" 1e15 355521.0 (-0.) 1.5 2.5;
  Printf.printf
    "%.1h %.1h %.1h %.2h\n"
    (b 0x3ff2800000000000L)
    (b 0x3ff1800000000000L)
    (b 0x3fff800000000000L)
    (b 0x3ff0080000000000L);
  Printf.printf "%+H % h %+h %h %+h\n" infinity nan neg_infinity infinity 0.;
  [%expect
    {|
    0x2p+49 +0X1P+18 -0x0p+0 0x2p+0 0x1p+1
    0x1.2p+0 0x1.2p+0 0x2.0p+0 0x1.00p+0
    +INFINITY  nan -infinity infinity +0x0p+0
    |}]

let%expect_test "next_after" =
  let p x y = Printf.printf "%h\n" (Float.next_after x y) in
  (* No carry between the two 32-bit halves of the bit pattern *)
  p 1. 2.;
  [%expect {| 0x1.0000000000001p+0 |}];
  p 1. 0.;
  [%expect {| 0x1.fffffffffffffp-1 |}];
  p (-1.) (-2.);
  [%expect {| -0x1.0000000000001p+0 |}];
  p (-1.) 0.;
  [%expect {| -0x1.fffffffffffffp-1 |}];
  (* Carry into the high half *)
  p 0x1.00000ffffffffp0 2.;
  [%expect {| 0x1.00001p+0 |}];
  p (-0x1.00000ffffffffp0) (-2.);
  [%expect {| -0x1.00001p+0 |}];
  p 0x1.fffffffffffffp0 infinity;
  [%expect {| 0x1p+1 |}];
  p (-0x1.fffffffffffffp0) neg_infinity;
  [%expect {| -0x1p+1 |}];
  (* Borrow from the high half *)
  p 0x1.00001p0 0.;
  [%expect {| 0x1.00000ffffffffp+0 |}];
  p (-0x1.00001p0) 0.;
  [%expect {| -0x1.00000ffffffffp+0 |}];
  p 2. 0.;
  [%expect {| 0x1.fffffffffffffp+0 |}];
  p (-2.) 0.;
  [%expect {| -0x1.fffffffffffffp+0 |}];
  (* Infinities *)
  p Float.max_float infinity;
  [%expect {| infinity |}];
  p (-.Float.max_float) neg_infinity;
  [%expect {| -infinity |}];
  p infinity 0.;
  [%expect {| 0x1.fffffffffffffp+1023 |}];
  p neg_infinity 0.;
  [%expect {| -0x1.fffffffffffffp+1023 |}];
  p infinity infinity;
  [%expect {| infinity |}];
  p infinity neg_infinity;
  [%expect {| 0x1.fffffffffffffp+1023 |}];
  (* Zeros and subnormals *)
  p 0. 1.;
  [%expect {| 0x0.0000000000001p-1022 |}];
  p 0. (-1.);
  [%expect {| -0x0.0000000000001p-1022 |}];
  p (-0.) 1.;
  [%expect {| 0x0.0000000000001p-1022 |}];
  p (-0.) (-1.);
  [%expect {| -0x0.0000000000001p-1022 |}];
  p 0x0.0000000000001p-1022 0.;
  [%expect {| 0x0p+0 |}];
  p (-0x0.0000000000001p-1022) 0.;
  [%expect {| -0x0p+0 |}];
  p 0x0.0000000000001p-1022 1.;
  [%expect {| 0x0.0000000000002p-1022 |}];
  p Float.min_float 0.;
  [%expect {| 0x0.fffffffffffffp-1022 |}];
  p 0x0.fffffffffffffp-1022 1.;
  [%expect {| 0x1p-1022 |}];
  p (-.Float.min_float) 0.;
  [%expect {| -0x0.fffffffffffffp-1022 |}];
  p 0x0.00000ffffffffp-1022 1.;
  [%expect {| 0x0.00001p-1022 |}];
  p 0x0.0000100000000p-1022 0.;
  [%expect {| 0x0.00000ffffffffp-1022 |}];
  (* NaN *)
  p nan 1.;
  [%expect {| nan |}];
  p 1. nan;
  [%expect {| nan |}]

let%expect_test ("next_after between signed zeros" [@when not (native && win32)]) =
  (* [next_after x y] is [y] when [x = y]. The C library used by OCaml on
     Windows returns the smallest subnormal towards [y] instead. *)
  let p x y = Printf.printf "%h\n" (Float.next_after x y) in
  p 0. (-0.);
  [%expect {| -0x0p+0 |}];
  p (-0.) 0.;
  [%expect {| 0x0p+0 |}]
