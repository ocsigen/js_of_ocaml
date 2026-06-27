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

(* The Notification API is not generally available in the test engines, so the
   suite only exercises the parts that do not depend on the global being
   present: building the options record and checking that the capability probe
   returns without throwing. *)

let%expect_test "options record can be populated" =
  let o = Notification.empty_options () in
  o##.body := Js.string "Hello";
  o##.tag := Js.string "greeting";
  o##.requireInteraction := Js._true;
  (* The fields are write-only; read them back through an unsafe coercion to
     check they were stored on the object. *)
  print_endline (Js.to_string (Js.Unsafe.coerce o)##.body);
  print_endline (Js.to_string (Js.Unsafe.coerce o)##.tag);
  [%expect {|
    Hello
    greeting |}]

let%expect_test "is_supported returns a bool" =
  let (_ : bool) = Notification.is_supported () in
  print_endline "PASSED";
  [%expect {| PASSED |}]
