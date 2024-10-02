(* Js_of_ocaml tests
 * http://www.ocsigen.org/js_of_ocaml/
 * Copyright (C) 2024 Hugo Heuzard
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

(* Testing renaming for backward edges with the default [--enable compact] *)

let%expect_test _ =
  let prog =
    {|
let rec f x y z =
  match x,y,z with
  | 0, 0, 0 -> true
  | _       -> f (x + z) (y - z) (z + x + y)

|}
  in
  let program = Util.compile_and_parse ~pretty:false prog in
  Util.print_program program;
  [%expect
    {|
    (function(a){
       "use strict";
       var b = a.jsoo_runtime;
       b.caml_register_global
        (0,
         [0,
          function(a, b, c){
           var f = a, e = b, d = c;
           for(;;){
            if(0 === f && 0 === e && 0 === d) return 1;
            var g = (d + f | 0) + e | 0;
            f = f + d | 0;
            e = e - d | 0;
            d = g;
           }
          }],
         "Test");
       return;
      }
      (globalThis));
    //end |}]
