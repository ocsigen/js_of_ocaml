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

(* The compiler allocates a global slot for each occurrence of a
   literal. Slots holding identical literals are bound to a single
   variable. *)

let%expect_test "identical literals" =
  let prog =
    {|
let f () = (1, 2)
let g () = (1, 2)
let h () = [ "abc"; "def" ]
let i () = [ "abc"; "def" ]
let j () = 1L
let k () = 1L
|}
  in
  let program = Util.compile_and_parse prog in
  Util.print_program program;
  [%expect
    {|
    (function(globalThis){
       "use strict";
       var
        runtime = globalThis.jsoo_runtime,
        caml_int64_create_lo_mi_hi = runtime.caml_int64_create_lo_mi_hi,
        caml_string_of_jsbytes = runtime.caml_string_of_jsbytes,
        _c_ = caml_int64_create_lo_mi_hi(1, 0, 0),
        _b_ =
          [0,
           caml_string_of_jsbytes("abc"),
           [0, caml_string_of_jsbytes("def"), 0]],
        _a_ = [0, 1, 2];
       function f(param){return _a_;}
       function g(param){return _a_;}
       function h(param){return _b_;}
       function i(param){return _b_;}
       function j(param){return _c_;}
       function k(param){return _c_;}
       runtime.caml_register_global([0, f, g, h, i, j, k], "Test");
       return;
      }
      (globalThis));
    //end
    |}]
