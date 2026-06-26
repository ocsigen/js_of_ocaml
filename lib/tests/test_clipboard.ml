(* Js_of_ocaml
 * http://www.ocsigen.org/js_of_ocaml/
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
 *)

open Js_of_ocaml

(* [navigator.clipboard] is not generally available (and requires a secure
   context) in the test engines, so the suite only checks that the capability
   probe returns a bool without throwing. *)

let%expect_test "is_supported returns a bool" =
  let (_ : bool) = Clipboard.is_supported () in
  print_endline "PASSED";
  [%expect {| PASSED |}]
