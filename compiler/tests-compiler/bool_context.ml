(* Js_of_ocaml tests
 * http://www.ocsigen.org/js_of_ocaml/
 * Copyright (C) 2026 Hugo Heuzard
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2 of the License, or
 * (at your option) any later version.
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

(* The result of comparing an integer with zero can be the integer itself
   when it is only tested ([0 !== e] is written [e]). This is not possible
   when it is negated and the negation is used as a value, since the
   negation is then compiled to [1 - e]. *)

open! Util

let prog =
  {|
let show b = print_string (if b then "true " else "false ")

(* The negation is used as a value *)
let not_value (a : int) =
  let c = compare a 0 <> 0 in
  let r = not c in
  (Obj.magic r : int)

let not_stored (a : int) =
  let c = compare a 0 <> 0 in
  let r = not c in
  [| r |]

(* The negation is only tested *)
let not_tested (a : int) =
  let c = compare a 0 <> 0 in
  if not c then 10 else 20

(* The negation reaches a value use through a join *)
let not_join (a : int) (b : bool) =
  let c = compare a 0 <> 0 in
  let d = if b then c else compare a 1 <> 0 in
  let r = not d in
  [| r |]

let () =
  print_int (not_value 5); print_char ' ';
  print_int (not_value 0); print_char ' ';
  show (not_stored 5).(0);
  show (not_stored 0).(0);
  print_int (not_tested 5); print_char ' ';
  print_int (not_tested 0); print_char ' ';
  show (not_join 5 true).(0);
  show (not_join 0 true).(0);
  show (not_join 1 false).(0);
  print_newline ()
|}

(* FIXME: should be [0 1 false true 20 10 false true true] *)
let%expect_test "behaviour" =
  compile_and_run prog;
  [%expect {| -4 1 true true 20 10 true true true |}]

let program = lazy (compile_and_parse prog)

let print name = print_fun_decl (Stdlib.Lazy.force program) (Some name)

let%expect_test "negation used as a value" =
  print "not_value";
  [%expect {|
    function not_value(a){var c = a, r = 1 - c; return r;}
    //end
    |}];
  print "not_stored";
  [%expect
    {|
    function not_stored(a){var c = a, r = 1 - c; return [0, r];}
    //end
    |}]

let%expect_test "negation only tested" =
  print "not_tested";
  [%expect {|
    function not_tested(a){var c = a; return c ? 20 : 10;}
    //end
    |}]

let%expect_test "negation of a join" =
  print "not_join";
  [%expect
    {|
    function not_join(a, b){
     var c = a, d = b ? c : a !== 1, r = 1 - d;
     return [0, r];
    }
    //end
    |}]
