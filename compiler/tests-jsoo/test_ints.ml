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

let printl l = print_endline (String.concat ", " (List.map string_of_int l))

let%expect_test _ =
  Scanf.sscanf "0.97.0" "%u.%u.%u" (fun major minor patch ->
      printl [ major; minor; patch ]);
  [%expect {| 0, 97, 0 |}];
  (try
     Scanf.sscanf "0.-97.0" "%u.%u.%u" (fun major minor patch ->
         printl [ major; minor; patch ])
   with Scanf.Scan_failure s -> print_endline s);
  [%expect {| scanf: bad input at char number 2: character '-' is not a decimal digit |}];
  Scanf.sscanf "0.-97.0" "%u.-%u.%u" (fun major minor patch ->
      printl [ major; minor; patch ]);
  [%expect {| 0, 97, 0 |}];
  Scanf.sscanf "0.-97.0" "%d.%d.%d" (fun major minor patch ->
      printl [ major; minor; patch ]);
  [%expect {| 0, -97, 0 |}]

let%expect_test _ =
  Printf.printf "%d\n" (int_of_string "0u123");
  [%expect {| 123 |}];
  Printf.printf "%d\n" (int_of_string "0U123");
  [%expect {| 123 |}]

let%expect_test _ =
  Printf.printf "%d\n" (int_of_string "-0u123");
  [%expect {| -123 |}];
  Printf.printf "%d\n" (int_of_string "-0U123");
  [%expect {| -123 |}]

let%expect_test _ =
  Printf.printf "%ld\n" (Int32.of_string "-0u2147483648");
  [%expect {| -2147483648 |}];
  Printf.printf "%ld\n" (Int32.of_string "-0U2147483648");
  [%expect {| -2147483648 |}];
  Printf.printf "%ld\n" (Int32.of_string "0u2147483648");
  [%expect {| -2147483648 |}];
  Printf.printf "%ld\n" (Int32.of_string "0U2147483648");
  [%expect {| -2147483648 |}]

let%expect_test _ =
  Printf.printf "%Ld\n" (Int64.of_string "-0u17965325103354776696");
  [%expect {| 481418970354774920 |}];
  Printf.printf "%Ld\n" (Int64.of_string "-0U17965325103354776696");
  [%expect {| 481418970354774920 |}];
  Printf.printf "%Ld\n" (Int64.of_string "0u17965325103354776696");
  [%expect {| -481418970354774920 |}];
  Printf.printf "%Ld\n" (Int64.of_string "0U17965325103354776696");
  [%expect {| -481418970354774920 |}]

let%expect_test _ =
  let check_fail x =
    try Printf.printf "%Lx\n" (Int64.of_string x)
    with Failure _ -> Format.printf "overflow\n"
  in
  Printf.printf "%Ld\n" (Int64.of_string "9223372036854775807");
  [%expect {| 9223372036854775807 |}];
  Printf.printf "%Ld\n" (Int64.of_string "-9223372036854775808");
  [%expect {| -9223372036854775808 |}];
  check_fail "9223372036854775808";
  [%expect {| overflow |}];
  check_fail "-9223372036854775809";
  [%expect {| overflow |}]

(* Arithmetic shifts must sign-extend into the low 24-bit limb for
   shift counts 41..47. *)
let%expect_test "Int64.shift_right, shifts 40..48" =
  List.iter
    (fun s ->
      Printf.printf
        "%d: %Lx %Lx %Lx\n"
        s
        (Int64.shift_right (-1L) s)
        (Int64.shift_right Int64.min_int s)
        (Int64.shift_right 0x123456789abcdef0L s))
    [ 40; 41; 44; 47; 48 ];
  [%expect
    {|
    40: ffffffffffffffff ffffffffff800000 123456
    41: ffffffffffffffff ffffffffffc00000 91a2b
    44: ffffffffffffffff fffffffffff80000 12345
    47: ffffffffffffffff ffffffffffff0000 2468
    48: ffffffffffffffff ffffffffffff8000 1234
    |}]

(* The '#' (alternate) flag must not add a base prefix to zero: native
   prints "0" for %#x / %#X / %#o of 0, like C printf. *)
let%expect_test "alternate flag with zero" =
  Printf.printf "[%#x]\n" 0;
  [%expect {| [0] |}];
  Printf.printf "[%#X]\n" 0;
  [%expect {| [0] |}];
  Printf.printf "[%#o]\n" 0;
  [%expect {| [0] |}];
  Printf.printf "[%#6x]\n" 0;
  [%expect {| [     0] |}];
  Printf.printf "[%#lx]\n" 0l;
  [%expect {| [0] |}];
  Printf.printf "[%#Lx]\n" 0L;
  [%expect {| [0] |}];
  (* non-zero values keep the prefix *)
  Printf.printf "[%#x]\n" 255;
  [%expect {| [0xff] |}];
  Printf.printf "[%#o]\n" 8;
  [%expect {| [010] |}]

(* Comparing an immediate against a custom block (e.g. an int64) must
   not call the custom's compare op with a raw number: native treats
   the immediate as strictly less than the block. *)
let%expect_test "compare immediate vs custom" =
  let a : Obj.t = Obj.repr 1 in
  let b : Obj.t = Obj.repr 1L in
  let p label f =
    match f () with
    | v -> Printf.printf "%s = %d\n" label v
    | exception e -> Printf.printf "%s raised %s\n" label (Printexc.to_string e)
  in
  p "cmp 1 1L" (fun () -> compare a b);
  p "cmp 1L 1" (fun () -> compare b a);
  p "cmp 2 1L" (fun () -> compare (Obj.repr 2) b);
  p "eq 1 1L" (fun () -> if a = b then 1 else 0);
  [%expect {|
    cmp 1 1L = -1
    cmp 1L 1 = 1
    cmp 2 1L = -1
    eq 1 1L = 0
    |}]

let%expect_test "failure messages of of_string" =
  let show f = try ignore (f ()) with Failure s -> print_endline s in
  show (fun () -> int_of_string "x");
  show (fun () -> Int32.of_string "4294967296");
  show (fun () -> Int32.of_string "x");
  show (fun () -> Nativeint.of_string "0x1_0000_0000_0000_0000");
  show (fun () -> Int64.of_string "0x1_0000_0000_0000_0000");
  [%expect
    {|
    int_of_string
    Int32.of_string
    Int32.of_string
    Nativeint.of_string
    Int64.of_string
    |}]

(* The JavaScript runtime formats int64 values below 2^53 directly, and
   larger ones as two chunks (the low one padded with zeros) *)
let%expect_test "int64 formatting around 2^53 and padded chunks" =
  List.iter
    (fun x ->
      Printf.printf "%Ld %Lu %Lx %LX %Lo %#Lx\n" x x x x x x;
      print_endline (Int64.to_string x))
    [ 0x1f_ffff_ffff_ffffL
    ; 0x20_0000_0000_0000L
    ; 0x20_0000_0000_0001L
    ; -0x20_0000_0000_0000L
    ; 0x8ac7_2304_89e8_0001L (* 10000000000000000001 *)
    ; 1234567890000000007L
    ; Int64.max_int
    ; Int64.min_int
    ; -1L
    ];
  [%expect
    {|
           9007199254740991 9007199254740991 1fffffffffffff 1FFFFFFFFFFFFF 377777777777777777 0x1fffffffffffff
           9007199254740991
           9007199254740992 9007199254740992 20000000000000 20000000000000 400000000000000000 0x20000000000000
           9007199254740992
           9007199254740993 9007199254740993 20000000000001 20000000000001 400000000000000001 0x20000000000001
           9007199254740993
           -9007199254740992 18437736874454810624 ffe0000000000000 FFE0000000000000 1777400000000000000000 0xffe0000000000000
           -9007199254740992
           -8446744073709551615 10000000000000000001 8ac7230489e80001 8AC7230489E80001 1053071060221172000001 0x8ac7230489e80001
           -8446744073709551615
           1234567890000000007 1234567890000000007 112210f4768db407 112210F4768DB407 104420417216643332007 0x112210f4768db407
           1234567890000000007
           9223372036854775807 9223372036854775807 7fffffffffffffff 7FFFFFFFFFFFFFFF 777777777777777777777 0x7fffffffffffffff
           9223372036854775807
           -9223372036854775808 9223372036854775808 8000000000000000 8000000000000000 1000000000000000000000 0x8000000000000000
           -9223372036854775808
           -1 18446744073709551615 ffffffffffffffff FFFFFFFFFFFFFFFF 1777777777777777777777 0xffffffffffffffff
           -1
           |}]

(* The JavaScript runtime divides by divisors below 2^29 one limb at a
   time, and by larger ones from a float estimate of the quotient *)
let%expect_test "int64 division" =
  let divisors =
    [ 1L
    ; 7L
    ; 10L
    ; 0x1fff_ffffL
    ; 0x2000_0000L
    ; 0x2000_0001L
    ; 1_000_000_007L
    ; 0x7fff_ffff_ffffL
    ; 0x1_0000_0000_0001L
    ; Int64.max_int
    ; Int64.min_int
    ; -1L
    ; -10L
    ; -0x2000_0001L
    ]
  in
  let dividends =
    [ 0L
    ; 1L
    ; -1L
    ; 0x2000_0000L
    ; 0x1f_ffff_ffff_ffffL
    ; 0x7fff_ffff_ffff_fffeL
    ; Int64.max_int
    ; Int64.min_int
    ; Int64.succ Int64.min_int
    ; -0x1234_5678_9abcL
    ]
  in
  let h = ref 0 in
  List.iter
    (fun x ->
      List.iter
        (fun d ->
          let q = Int64.div x d
          and r = Int64.rem x d
          and uq = Int64.unsigned_div x d
          and ur = Int64.unsigned_rem x d in
          assert (Int64.add (Int64.mul q d) r = x);
          assert (Int64.add (Int64.mul uq d) ur = x);
          h := Hashtbl.hash (!h, q, r, uq, ur))
        divisors)
    dividends;
  Printf.printf "%d\n" !h;
  Printf.printf
    "%Ld %Ld\n"
    (Int64.div Int64.min_int (-1L))
    (Int64.rem Int64.min_int (-1L));
  Printf.printf
    "%Lu %Lu\n"
    (Int64.unsigned_div (-1L) 0x2000_0000L)
    (Int64.unsigned_rem (-1L) 0x2000_0001L);
  Printf.printf
    "%Ld %Ld\n"
    (Int64.div 0x7fff_ffff_ffff_fffeL 0x7fff_ffffL)
    (Int64.rem 0x7fff_ffff_ffff_fffeL 0x7fff_ffffL);
  [%expect
    {|
           323016542
           -9223372036854775808 0
           34359738367 63
           4294967298 0
           |}]

(* The JavaScript runtime accumulates the first digits in a float, then
   continues with int64 arithmetic *)
let%expect_test "int64 parsing across 2^53" =
  let show s =
    match Int64.of_string s with
    | x -> Printf.printf "%s: %Ld\n" s x
    | exception Failure m -> Printf.printf "%s: %s\n" s m
  in
  List.iter
    show
    [ "900719925474099"
    ; "9007199254740991"
    ; "9007199254740992"
    ; "9007199254740993"
    ; "90071992547409931"
    ; "9_007_199_254_740_993"
    ; "900719925474099_3_"
    ; "9223372036854775807"
    ; "9223372036854775808"
    ; "-9223372036854775808"
    ; "-9223372036854775809"
    ; "0u18446744073709551615"
    ; "0u18446744073709551616"
    ; "0x1fffffffffffff"
    ; "0x20000000000001"
    ; "0xffffffffffffffff"
    ; "0o1777777777777777777777"
    ; "0o2000000000000000000000"
    ; "0b1111111111111111111111111111111111111111111111111111111111111111"
    ; "00000000000000000000000000000000012"
    ; "12345678901234567x"
    ];
  [%expect
    {|
           900719925474099: 900719925474099
           9007199254740991: 9007199254740991
           9007199254740992: 9007199254740992
           9007199254740993: 9007199254740993
           90071992547409931: 90071992547409931
           9_007_199_254_740_993: 9007199254740993
           900719925474099_3_: 9007199254740993
           9223372036854775807: 9223372036854775807
           9223372036854775808: Int64.of_string
           -9223372036854775808: -9223372036854775808
           -9223372036854775809: Int64.of_string
           0u18446744073709551615: -1
           0u18446744073709551616: Int64.of_string
           0x1fffffffffffff: 9007199254740991
           0x20000000000001: 9007199254740993
           0xffffffffffffffff: -1
           0o1777777777777777777777: -1
           0o2000000000000000000000: Int64.of_string
           0b1111111111111111111111111111111111111111111111111111111111111111: -1
           00000000000000000000000000000000012: 12
           12345678901234567x: Int64.of_string
           |}]

(* The Wasm runtime shifts rather than divides in base 8 and 16 *)
let%expect_test "int32 formatting in base 8 and 16" =
  List.iter
    (fun x -> Printf.printf "%lx %lX %lo %#lx %#lo %lu %+ld %li\n" x x x x x x x x)
    [ 0l; 1l; -1l; Int32.max_int; Int32.min_int; 0x12345678l ];
  [%expect
    {|
           0 0 0 0 0 0 +0 0
           1 1 1 0x1 01 1 +1 1
           ffffffff FFFFFFFF 37777777777 0xffffffff 037777777777 4294967295 -1 -1
           7fffffff 7FFFFFFF 17777777777 0x7fffffff 017777777777 2147483647 +2147483647 2147483647
           80000000 80000000 20000000000 0x80000000 020000000000 2147483648 -2147483648 -2147483648
           12345678 12345678 2215053170 0x12345678 02215053170 305419896 +305419896 305419896
           |}]
