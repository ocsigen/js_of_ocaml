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

let%expect_test "parallel renaming" =
  let p =
    {|
let rec f n x y = if n = 0 then (x, y) else f (n - 1) y x
   |}
  in
  let p = compile_and_parse p in
  print_fun_decl p (Some "f");
  [%expect
    {|
    function f(n$1, x$0, y$0){
     var n = n$1, x = x$0, y = y$0;
     for(;;){
      if(0 === n) return [0, x, y];
      var n$0 = n - 1 | 0, _a_ = _a_;
      n = n$0;
      _a_ = y;
      y = x;
      x = _a_;
     }
    }
    //end
    |}]
