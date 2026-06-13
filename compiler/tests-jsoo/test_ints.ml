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
  let p label f = match f () with
    | v -> Printf.printf "%s = %d\n" label v
    | exception e -> Printf.printf "%s raised %s\n" label (Printexc.to_string e)
  in
  p "cmp 1 1L" (fun () -> compare a b);
  p "cmp 1L 1" (fun () -> compare b a);
  p "cmp 2 1L" (fun () -> compare (Obj.repr 2) b);
  p "eq 1 1L" (fun () -> if a = b then 1 else 0);
  [%expect
    {|
    cmp 1 1L raised Failure("TypeError: x.compare is not a function")
    cmp 1L 1 = 1
    cmp 2 1L raised Failure("TypeError: x.compare is not a function")
    eq 1 1L = 0
    |}]
