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

open! Util

(* OCaml 5.6 tells which blocks are immutable. Tuples which are always
   bound to immutable blocks can be unboxed even when a function call
   happens between their field accesses. *)
[@@@if ocaml_version >= (5, 6, 0)]

let%expect_test _ =
  let program =
    compile_and_parse
      ~flags:[ "--no-inline" ]
      {|
      let f b x y =
        let p = if b then (x, y) else (y, x) in
        print_int (fst p); print_int (snd p)
    |}
  in
  print_fun_decl program (Some "f");
  [%expect
    {|
           function f(b, x, y){
            if(b) var _b_ = y, _a_ = x; else{_b_ = x; _a_ = y;}
            caml_call1(Stdlib[44], _a_);
            return caml_call1(Stdlib[44], _b_);
           }
           //end
           |}]
