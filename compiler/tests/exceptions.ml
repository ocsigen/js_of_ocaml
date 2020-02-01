(* Js_of_ocaml compiler
 * http://www.ocsigen.org/js_of_ocaml/
 * Copyright (C) 2019 Hugo Heuzard
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

(* https://github.com/ocsigen/js_of_ocaml/issues/829 *)

let%expect_test _ =
  let program ~debug =
    compile_and_parse
      ~debug
      {|
let some_name () = raise (try try raise Not_found with x -> x with i -> i)
let prevent_inline = some_name
      |}
  in
  print_fun_decl (program ~debug:true) None;
  [%expect
    {|
    function some_name(param)
     {try
       {try {throw Stdlib[8]}catch(x){x = caml_wrap_exception(x);var i=x}}
      catch(i$0){i$0 = caml_wrap_exception(i$0);var i=i$0}
      throw i} |}];
  print_fun_decl (program ~debug:false) None;
  [%expect
    {|
    function _a_(_b_)
     {try
       {try
         {throw Stdlib[8]}
        catch(_e_){_e_ = caml_wrap_exception(_e_);var _c_=_e_}}
      catch(_d_){_d_ = caml_wrap_exception(_d_);throw _d_}
      throw _c_} |}]
