(* Js_of_ocaml compiler
 * http://www.ocsigen.org/js_of_ocaml/
 * Copyright (C) 2026 Jérôme Vouillon
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

let%expect_test "Js_traverse.simpl ordering" =
  let p =
    {|
let f () =
  let rec process b s = if b then s ^ "" else process true s in
  let maybe_process ?(b = false) string =
    if b then process false string else string in
  let wrap f = fun ?b string -> f (maybe_process ?b string) in
  let print_endline = wrap print_endline in
  fun string -> print_endline string
   |}
  in
  let p = compile_and_parse ~flags:[ "--debug=invariant" ] p in
  print_fun_decl p (Some "f");
  [%expect
    {|
    function f(param){
     var f = Stdlib[46];
     return function(string){var _a_ = string; return caml_call1(f, _a_);};
    }
    //end
    |}]
