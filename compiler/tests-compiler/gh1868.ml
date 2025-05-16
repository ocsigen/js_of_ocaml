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

let%expect_test "" =
  let program =
    compile_and_parse
      ~effects:`Double_translation
      {|
exception Nested of exn
let wrap f =
  try f () with exn ->
  let rec unwrap exn =
    match exn with
    | Nested e -> unwrap e
    | _ -> raise exn
  in
  unwrap exn
|}
  in
  print_double_fun_decl program "wrap";
  [%expect
    {|
    function wrap$0(f){
     try{var _a_ = caml_call1(f, 0); return _a_;}
     catch(exn$1){
      var exn = caml_wrap_exception(exn$1);
      for(;;){
       var tag = exn[1];
       if(tag !== Nested) throw caml_maybe_attach_backtrace(exn, 1);
       var exn$0 = exn[2];
       exn = exn$0;
      }
     }
    }
    //end
    function wrap$1(f, cont){
     function _a_(exn$1){
      var tag = exn$1[1];
      if(tag === Nested){
       var exn$0 = exn$1[2];
       return caml_exact_trampoline_call1(_a_, exn$0);
      }
      var raise = caml_pop_trap(), exn = caml_maybe_attach_backtrace(exn$1, 1);
      return raise(exn);
     }
     runtime.caml_push_trap(_a_);
     return caml_trampoline_cps_call2
             (f, 0, function(_a_){caml_pop_trap(); return cont(_a_);});
    }
    //end
    var wrap = runtime.caml_cps_closure(wrap$0, wrap$1);
    //end
    |}]
