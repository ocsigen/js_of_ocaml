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
    function some_name(param){
     try{
      try{throw caml_maybe_attach_backtrace(Stdlib[8], 1);}
      catch(x$0){var x = caml_wrap_exception(x$0), i = x;}
     }
     catch(i$0){var i = caml_wrap_exception(i$0);}
     throw caml_maybe_attach_backtrace(i, 1);
    }
    //end
    |}];
  print_fun_decl (program ~debug:false) None;
  [%expect
    {|
    function _a_(_c_){
     try{
      try{throw caml_maybe_attach_backtrace(Stdlib[8], 1);}
      catch(_c_){var _b_ = caml_wrap_exception(_c_);}
     }
     catch(_c_){
      var _a_ = caml_wrap_exception(_c_);
      throw caml_maybe_attach_backtrace(_a_, 1);
     }
     throw caml_maybe_attach_backtrace(_b_, 1);
    }
    //end
    |}]
