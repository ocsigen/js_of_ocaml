(* Js_of_ocaml tests
 * http://www.ocsigen.org/js_of_ocaml/
 * Copyright (C) 2023 Hugo Heuzard
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2 of the License, or
 * (at your option) any later version.
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

let header =
  {|
module Js = struct
  type t

  type 'a js_array = t

  type ('a, 'b) meth_callback = t

  external string : string -> t = "caml_jsstring_of_string"

  external to_string : t -> string = "caml_string_of_jsstring"

  external bytestring : string -> t = "caml_jsbytes_of_string"

  external to_bytestring : t -> string = "caml_string_of_jsbytes"

  external bool : bool -> t = "caml_js_from_bool"

  external to_bool : t -> bool = "caml_js_to_bool"

  external array : 'a array -> t = "caml_js_from_array"

  external to_array : t -> 'a array = "caml_js_to_array"

  external number_of_float : float -> t = "caml_js_from_float"

  external float_of_number : t -> float = "caml_js_to_float"

  external typeof : t -> t = "caml_js_typeof"

  external instanceof : t -> t -> bool = "caml_js_instanceof"

  external debugger : unit -> unit = "debugger"

  external get : t -> t -> t = "caml_js_get"

  external set : t -> t -> t -> unit = "caml_js_set"

  external delete : t -> t -> unit = "caml_js_delete"

  external call : t -> t -> t array -> t = "caml_js_call"

  external fun_call : t -> t array -> t = "caml_js_fun_call"

  external meth_call : t -> string -> t array -> t = "caml_js_meth_call"

  external new_obj : t -> t array -> t = "caml_js_new"

  external new_obj_arr : t -> t js_array -> t = "caml_ojs_new_arr"

  external obj : (string * t) array -> t = "caml_js_object"

  external equals : t -> t -> bool = "caml_js_equals"

  external pure_expr : (unit -> 'a) -> 'a = "caml_js_pure_expr"

  external eval_string : string -> 'a = "caml_js_eval_string"

  external js_expr : string -> 'a = "caml_js_expr"

  external pure_js_expr : string -> 'a = "caml_pure_js_expr"

  external callback : ('b -> 'a) -> ('b, 'a) meth_callback
    = "caml_js_wrap_callback_unsafe"

  external callback_with_arguments :
    (t js_array -> 'b) -> ('c, t js_array -> 'b) meth_callback
    = "caml_js_wrap_callback_arguments"

  external callback_with_arity : int -> ('a -> 'b) -> ('c, 'a -> 'b) meth_callback
    = "caml_js_wrap_callback_strict"

  external meth_callback : ('b -> 'a) -> ('b, 'a) meth_callback
    = "caml_js_wrap_meth_callback_unsafe"

  external meth_callback_with_arity : int -> ('b -> 'a) -> ('b, 'a) meth_callback
    = "caml_js_wrap_meth_callback_strict"

  external meth_callback_with_arguments :
    ('b -> t js_array -> 'a) -> ('b, t js_array -> 'a) meth_callback
    = "caml_js_wrap_meth_callback_arguments"

  external wrap_callback : ('a -> 'b) -> ('c, 'a -> 'b) meth_callback
    = "caml_js_wrap_callback"

  external wrap_meth_callback : ('a -> 'b) -> ('a, 'b) meth_callback
    = "caml_js_wrap_meth_callback"
end
|}

let compile_and_parse s = compile_and_parse (header ^ "\n" ^ s)

let%expect_test "object" =
  let program =
    compile_and_parse
      {|
      let obj_literal =
        Js.obj
         [| "aaa", Obj.magic 'a'
          ; "bbb", Obj.magic (Js.string "test")
          ; "cc dd", Obj.magic 2
          ; "npiπ", Obj.magic 5
         |]
|}
  in
  print_var_decl program "obj_literal";
  [%expect
    {|
    var obj_literal = ({aaa:97,bbb:"test","cc dd":2,"npiπ":5});
    //end |}]

let%expect_test "get" =
  let program =
    compile_and_parse
      {|
      let get_a o =
        Js.get o (Js.string "aaa")
       let get_b o =
        Js.get o (Js.string "a b")
       let get_c o =
        Js.get o (Js.string "npiπ")
      |}
  in
  print_fun_decl program (Some "get_a");
  print_fun_decl program (Some "get_b");
  print_fun_decl program (Some "get_c");
  [%expect
    {|
    function get_a(o){return o.aaa}
    //end
    function get_b(o){return o["a b"]}
    //end
    function get_c(o){return o["npiπ"]}
    //end |}]

let%expect_test "set" =
  let program =
    compile_and_parse
      {|
      let set_a o x =
        Js.set o (Js.string "aaa") x
       let set_b o x =
        Js.set o (Js.string "a b") x
       let set_c o x =
        Js.set o (Js.string "npiπ") x
      |}
  in
  print_fun_decl program (Some "set_a");
  print_fun_decl program (Some "set_b");
  print_fun_decl program (Some "set_c");
  [%expect
    {|
    function set_a(o,x){return o.aaa = x}
    //end
    function set_b(o,x){return o["a b"] = x}
    //end
    function set_c(o,x){return o["npiπ"] = x}
    //end |}]

let%expect_test "delete" =
  let program =
    compile_and_parse
      {|
      let delete_a o =
        Js.delete o (Js.string "aaa") 
       let delete_b o  =
        Js.delete o (Js.string "a b") 
       let delete_c o  =
        Js.delete o (Js.string "npiπ") 
      |}
  in
  print_fun_decl program (Some "delete_a");
  print_fun_decl program (Some "delete_b");
  print_fun_decl program (Some "delete_c");
  [%expect
    {|
    function delete_a(o){return delete o.aaa}
    //end
    function delete_b(o){return delete o["a b"]}
    //end
    function delete_c(o){return delete o["npiπ"]}
    //end |}]

let%expect_test "meth_call1" =
  let program =
    compile_and_parse
      {|
      let meth_call_a o x =
        Js.meth_call o "aaa" x
       let meth_call_b o x =
        Js.meth_call o "a b" x
       let meth_call_c o x =
        Js.meth_call o "npiπ" x
      |}
  in
  print_fun_decl program (Some "meth_call_a");
  print_fun_decl program (Some "meth_call_b");
  print_fun_decl program (Some "meth_call_c");
  [%expect
    {|
    function meth_call_a(o,x){return caml_js_meth_call(o,cst_aaa,x)}
    //end
    function meth_call_b(o,x){return caml_js_meth_call(o,cst_a_b,x)}
    //end
    function meth_call_c(o,x){return caml_js_meth_call(o,cst_npi,x)}
    //end |}]

let%expect_test "meth_call2" =
  let program =
    compile_and_parse
      {|
      let meth_call_a o x =
        Js.meth_call o "aaa" [| x |]
       let meth_call_b o x =
        Js.meth_call o "a b" [| x |]
       let meth_call_c o x =
        Js.meth_call o "npiπ" [| x |]
      |}
  in
  print_fun_decl program (Some "meth_call_a");
  print_fun_decl program (Some "meth_call_b");
  print_fun_decl program (Some "meth_call_c");
  [%expect
    {|
    function meth_call_a(o,x){return o.aaa(x)}
    //end
    function meth_call_b(o,x){return caml_js_meth_call(o,cst_a_b,[0,x])}
    //end
    function meth_call_c(o,x){return caml_js_meth_call(o,cst_npi,[0,x])}
    //end |}]
