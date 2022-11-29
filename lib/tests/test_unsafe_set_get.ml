(* Js_of_ocaml tests
 * http://www.ocsigen.org/js_of_ocaml/
 * Copyright (C) 2020 Hugo Heuzard
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

open Js_of_ocaml

let s x =
  let to_string =
    Js.Unsafe.eval_string
      {|
(function(x){
    if(x === null)
      return "null"
    if(x === undefined)
      return "undefined"
    if(typeof x === "function")
      return "function#" + x.length
    if(x.toString() == "[object Arguments]")
      return "(Arguments: " + Array.prototype.slice.call(x).toString() + ")";
    if(typeof x === "object")
      return "(Object: " + Object.keys(x).toString() + ")";
    return x.toString()
})
|}
  in
  Js.to_string (Js.Unsafe.fun_call to_string [| Js.Unsafe.inject x |])

let%expect_test _ =
  let o = object%js end in
  Js.Unsafe.set o "ascii1" 1;
  Js.Unsafe.set o (Js.string "ascii2") 2;
  print_endline (s o);
  [%expect {| (Object: ascii1,ascii2) |}];
  let o = object%js end in
  Js.Unsafe.set o "a•›" 1;
  Js.Unsafe.set o (Js.string "b•›") 2;
  print_endline (s o);
  [%expect {| (Object: aâ¢âº,b•›) |}];
  let o = object%js end in
  let prefix = "prefix:" in
  let f s g t = Js.Unsafe.set o (g (prefix ^ s)) t in
  f "a•›" (fun s -> s) 1;
  f "b•›" Js.string 2;
  f "c•›" Js.bytestring 2;
  print_endline (s o);
  [%expect {| (Object: prefix:aâ¢âº,prefix:b•›,prefix:câ¢âº) |}]
