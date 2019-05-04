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

open Util

let%expect_test _ =
  compile_and_run
    {| Scanf.sscanf "0.97.0" "%u.%u.%u" (fun major minor patch -> [major; minor; patch]) |};
  [%expect
    {|
    /tmp/jsoo_test8e4b75.js:6122
        function bad_input(s){throw [0,Scan_failure,s]}
                              ^
    0,248,Stdlib.Scanf.Scan_failure,5,scanf: bad input at char number 1: int_of_string

    process exited with error code 1
     node /tmp/jsoo_test8e4b75.js |}]

let%expect_test _ =
  Printf.printf "%d\n" (int_of_string "0u123");
  [%expect {| 123 |}];
  Printf.printf "%d\n" (int_of_string "0U123");
  [%expect {| 123 |}]
