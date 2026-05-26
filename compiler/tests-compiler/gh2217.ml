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

let%expect_test "conditional simplification" =
  let p =
    {|
let g () =
  let f b = let x = if b then "a" else "b" in (b, x) in
  (fst (f true), fst (f false))
   |}
  in
  let p = compile_and_parse ~flags:[ "--debug=invariant" ] p in
  print_fun_decl p (Some "g");
  [%expect
    {|
    function g(_a_){
     function f(b){return [0, b];}
     var _a_ = f(0)[1];
     return [0, f(1)[1], _a_];
    }
    //end
    |}]
