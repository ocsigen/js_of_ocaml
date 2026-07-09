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

(* No WebGL context is available in the test engines, so the suite exercises
   the parts that do not need one: the context-attributes record and the
   module surfaces. *)

let%expect_test "context attributes record can be populated" =
  let a = WebGL.defaultContextAttributes in
  a##.antialias := Js._false;
  a##.powerPreference := Js.string "high-performance";
  a##.desynchronized := Js._true;
  print_endline (string_of_bool (Js.to_bool a##.antialias));
  print_endline (Js.to_string a##.powerPreference);
  print_endline (string_of_bool (Js.to_bool a##.desynchronized));
  print_endline (string_of_bool (Js.to_bool a##.xrCompatible));
  [%expect {|
    false
    high-performance
    true
    false |}]

let%expect_test "context constructors are exposed" =
  let (_ : Dom_html.canvasElement Js.t -> WebGL.renderingContext Js.t Js.opt) =
    WebGL.getContext
  in
  let (_ : Dom_html.canvasElement Js.t -> WebGL2.renderingContext Js.t Js.opt) =
    WebGL2.getContext
  in
  let (_
        :    Dom_html.canvasElement Js.t
          -> WebGL.contextAttributes Js.t
          -> WebGL2.renderingContext Js.t Js.opt) =
    WebGL2.getContextWithAttributes
  in
  print_endline "PASSED";
  [%expect {| PASSED |}]
