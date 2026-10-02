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

(* The test engines have no DOM: the form is a plain object with the
   properties that [Form] reads. Each input is [(type, name, value)]. *)
let form (inputs : (string * string * string) list) : Dom_html.formElement Js.t =
  let make =
    Js.Unsafe.js_expr
      {|(function (inputs) {
          var elements = inputs.map(function (i) {
            return { tagName: "INPUT", type: i[0], name: i[1], value: i[2],
                     checked: false, disabled: false };
          });
          return { elements: { length: elements.length,
                               item: function (n) { return elements[n]; } } };
        })|}
  in
  let inputs =
    Js.array
      (Array.of_list
         (List.map
            (fun (typ, name, value) ->
              Js.array [| Js.string typ; Js.string name; Js.string value |])
            inputs))
  in
  Js.Unsafe.fun_call make [| Js.Unsafe.inject inputs |]

let print_fields fields =
  List.iter (fun (name, value) -> Printf.printf "%s=%s\n" name value) fields

(* [form.elements] has no image inputs, so they are not listed here. *)
let%expect_test "buttons are not fields" =
  let form =
    form
      [ "text", "t", "x"
      ; "submit", "s", "S"
      ; "reset", "r", "R"
      ; "button", "b", "B"
      ; "hidden", "h", "y"
      ]
  in
  print_fields (Form.get_form_contents form);
  [%expect {|
    t=x
    h=y
    |}];
  print_fields
    (List.map
       (function
         | name, `String value -> name, Js.to_string value
         | name, `File _ -> name, "<file>")
       (Form.form_elements form));
  [%expect {|
    t=x
    h=y
    |}]
