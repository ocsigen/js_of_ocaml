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

let%expect_test "static eval of string get" =
  let program =
    compile_and_parse
      {|
    [@@@ocaml.warning "-3"]
    let my_is_block x = Obj.is_block (Obj.repr x)
    let my_is_int x = Obj.is_int (Obj.repr x)
    let my_tag x = Obj.tag (Obj.repr [x])
    let my_size x = Obj.size (Obj.repr x)
    let my_field x i = Obj.field (Obj.repr x) i
    let my_set_field x i o = Obj.set_field (Obj.repr x) i o
    let my_set_tag x t = Obj.set_tag (Obj.repr [x]) t
    let my_new_block x l = Obj.new_block (x + 1) 3
    let my_dup t = Obj.dup (Obj.repr [t])
    let my_truncate t i = Obj.truncate (Obj.repr [t]) i
  |}
  in
  print_fun_decl program (Some "my_is_block");
  print_fun_decl program (Some "my_is_int");
  print_fun_decl program (Some "my_tag");
  print_fun_decl program (Some "my_size");
  print_fun_decl program (Some "my_field");
  print_fun_decl program (Some "my_set_field");
  print_fun_decl program (Some "my_set_tag");
  print_fun_decl program (Some "my_new_block");
  print_fun_decl program (Some "my_dup");
  print_fun_decl program (Some "my_truncate");
  [%expect
    {|
    function my_is_block(x){return caml_call1(Stdlib_Obj[1],x)}
    function my_is_int(x){return typeof x === "number"?1:0}
    function my_tag(x){return runtime.caml_obj_tag([0,x,0])}
    function my_size(x){return x.length - 1}
    function my_field(x,i){return x[1 + i]}
    function my_set_field(x,i,o){x[1 + i] = o;return 0}
    function my_set_tag(x,t){return runtime.caml_obj_set_tag([0,x,0],t)}
    function my_new_block(x,l){return runtime.caml_obj_block(x + 1 | 0,3)}
    function my_dup(t){return [0,t,0].slice()}
    function my_truncate(t,i){return runtime.caml_obj_truncate([0,t,0],i)} |}]
