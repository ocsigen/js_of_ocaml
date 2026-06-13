(* Js_of_ocaml tests
 * http://www.ocsigen.org/js_of_ocaml/
 * Copyright (C) 2026 Hugo Heuzard
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

(* The SIMPLEOPT/SIMPLESTAR/SIMPLEPLUS opcodes of the Str engine must
   not read past the end of the string: an out-of-bounds read tests
   the membership of '\000' in the character class, which negated
   classes contain. *)

let%expect_test "negated class at end of string" =
  let b = Str.string_partial_match (Str.regexp "[^;]?;") "" 0 in
  Printf.printf "%b\n" b;
  [%expect {| true |}];
  let b = Str.string_partial_match (Str.regexp "a[^;]?;") "a" 0 in
  Printf.printf "%b\n" b;
  [%expect {| true |}]

(* These used to loop forever: the negated class kept matching the
   out-of-bounds read at the end of the string. *)
let%expect_test "simple star and plus at end of string" =
  let b = Str.string_match (Str.regexp "[^;]*;") "no semicolon" 0 in
  Printf.printf "%b\n" b;
  [%expect {| false |}];
  let b = Str.string_match (Str.regexp "[^;]+;") "no semicolon" 0 in
  Printf.printf "%b\n" b;
  [%expect {| false |}];
  let b = Str.string_match (Str.regexp "[^;]*;") "stop; go" 0 in
  Printf.printf "%b %s\n" b (Str.matched_string "stop; go");
  [%expect {| true stop; |}];
  let b = Str.string_match (Str.regexp "x[^;]+") "xabc" 0 in
  Printf.printf "%b %s\n" b (Str.matched_string "xabc");
  [%expect {| true xabc |}]

(* A regexp with many alternatives produces more than 256 constant-pool
   entries; matching a late alternative indexes a high pool slot, which
   must not be truncated to 8 bits. *)
let%expect_test "large alternation" =
  let alts = List.init 300 (fun i -> Printf.sprintf "x%03d" i) in
  let re = Str.regexp (String.concat "\\|" alts) in
  let ok = ref 0 in
  List.iter
    (fun a -> if Str.string_match re a 0 && Str.matched_string a = a then incr ok)
    alts;
  Printf.printf "%d/300\n" !ok;
  [%expect {| 300/300 |}]
