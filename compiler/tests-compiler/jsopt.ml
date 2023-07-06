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

  external number_of_int32 : int32 -> t = "caml_js_from_int32"

  external int32_of_number : t -> int32 = "caml_js_to_int32"

  external number_of_nativeint : nativeint -> t = "caml_js_from_nativeint"

  external nativeint_of_number : t -> nativeint = "caml_js_to_nativeint"

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

let compile_and_parse ?flags ?use_js_string s =
  compile_and_parse ?flags ?use_js_string (header ^ "\n" ^ s)

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
          ; {u|\uD83D\uDC2B|u}, Obj.magic 3
          ; {u|\u{1F42B}|u}, Obj.magic 1
          ; {u|\u{1ee62}|u}, Obj.magic 1
          ; "\u{1F42B}", Obj.magic 1
          ; "\u{1ee62}", Obj.magic 1
         |]
|}
  in
  print_var_decl program "obj_literal";
  [%expect
    {|
    var obj_literal = ({aaa: 97,
      bbb: "test",
      "cc dd": 2,
      npiπ: 5,
      "\uD83D\uDC2B": 3,
      "\u{1F42B}": 1,
      "\u{1ee62}": 1,
      "🐫": 1,
      𞹢: 1});
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
       let get_d o =
        Js.get o (Js.string "\u{1F42B}")
       let get_e o =
        Js.get o (Js.string "\u{1ee62}")

      |}
  in
  print_fun_decl program (Some "get_a");
  print_fun_decl program (Some "get_b");
  print_fun_decl program (Some "get_c");
  print_fun_decl program (Some "get_d");
  print_fun_decl program (Some "get_e");
  [%expect
    {|
    function get_a(o){return o.aaa;}
    //end
    function get_b(o){return o["a b"];}
    //end
    function get_c(o){return o.npiπ;}
    //end
    function get_d(o){return o["🐫"];}
    //end
    function get_e(o){return o.𞹢;}
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
    function set_a(o, x){return o.aaa = x;}
    //end
    function set_b(o, x){return o["a b"] = x;}
    //end
    function set_c(o, x){return o.npiπ = x;}
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
    function delete_a(o){return delete o.aaa;}
    //end
    function delete_b(o){return delete o["a b"];}
    //end
    function delete_c(o){return delete o.npiπ;}
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
    function meth_call_a(o, x){return caml_js_meth_call(o, cst_aaa, x);}
    //end
    function meth_call_b(o, x){return caml_js_meth_call(o, cst_a_b, x);}
    //end
    function meth_call_c(o, x){return caml_js_meth_call(o, cst_npi, x);}
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
    function meth_call_a(o, x){return o.aaa(x);}
    //end
    function meth_call_b(o, x){
     return runtime.caml_js_meth_call(o, cst_a_b, [0, x]);
    }
    //end
    function meth_call_c(o, x){return o.npiπ(x);}
    //end |}]

let%expect_test "jstring / bytestring " =
  let program =
    compile_and_parse
      {|
      let s1 = Js.string "a"
      let s2 = Js.bytestring "a"
      let s3 = Js.string "npiπ"
      let s4 = Js.bytestring "npiπ"
      |}
  in
  print_var_decl program "s1";
  print_var_decl program "s2";
  print_var_decl program "s3";
  print_var_decl program "s4";
  [%expect
    {|
    var s1 = "a";
    //end
    var s2 = "a";
    //end
    var s3 = "npiπ";
    //end
    var s4 = "npi\xcf\x80";
    //end |}]

let string_sharing_prog =
  {|
      let s1 = Js.string "abcdef"
      let s2 = Js.bytestring "abcdef"
      let s3 = "abcdef"
      let s4 = Js.string "npiπ"
      let s5 = Js.bytestring "npiπ"
      let s6 = "npiπ"
      let s7 = Js.string "abc\\def"
      let s8 = Js.bytestring "abc\\def"
      let s9 = "abc\\def"
      let s1_bis = Js.string "abcdef"
      let s2_bis = Js.bytestring "abcdef"
      let s3_bis = "abcdef"
      let s4_bis = Js.string "npiπ"
      let s5_bis = Js.bytestring "npiπ"
      let s6_bis = "npiπ"
      let s7_bis = Js.string "abc\\def"
      let s8_bis = Js.bytestring "abc\\def"
      let s9_bis = "abc\\def"
      |}

let%expect_test "string sharing" =
  let program ~share ~js_string =
    compile_and_parse
      ~flags:[ (if share then "--enable" else "--disable"); "share" ]
      ~use_js_string:js_string
      string_sharing_prog
  in
  print_program (program ~share:true ~js_string:true);
  [%expect
    {|
    (function(globalThis){
       "use strict";
       var
        str_npi_xcf_x80 = "npi\xcf\x80",
        str_abcdef = "abcdef",
        str_npi = "npiπ",
        str_abc_def = "abc\\def",
        runtime = globalThis.jsoo_runtime,
        s3 = str_abcdef,
        s6 = str_npi_xcf_x80,
        s9 = str_abc_def,
        s3_bis = str_abcdef,
        s6_bis = str_npi_xcf_x80,
        s9_bis = str_abc_def,
        Js = [0],
        s1 = str_abcdef,
        s2 = str_abcdef,
        s4 = str_npi,
        s5 = str_npi_xcf_x80,
        s7 = str_abc_def,
        s8 = str_abc_def,
        s1_bis = str_abcdef,
        s2_bis = str_abcdef,
        s4_bis = str_npi,
        s5_bis = str_npi_xcf_x80,
        s7_bis = str_abc_def,
        s8_bis = str_abc_def,
        Test =
          [0,
           Js,
           s1,
           s2,
           s3,
           s4,
           s5,
           s6,
           s7,
           s8,
           s9,
           s1_bis,
           s2_bis,
           s3_bis,
           s4_bis,
           s5_bis,
           s6_bis,
           s7_bis,
           s8_bis,
           s9_bis];
       runtime.caml_register_global(18, Test, "Test");
       return;
      }
      (globalThis));
    //end |}];
  print_program (program ~share:false ~js_string:true);
  [%expect
    {|

    (function(globalThis){
       "use strict";
       var
        runtime = globalThis.jsoo_runtime,
        cst_npi = "npi\xcf\x80",
        cst_abc_def = "abc\\def",
        cst_abcdef = "abcdef",
        cst_npi$0 = "npiπ",
        s3 = cst_abcdef,
        s6 = cst_npi,
        s9 = cst_abc_def,
        s3_bis = cst_abcdef,
        s6_bis = cst_npi,
        s9_bis = cst_abc_def,
        Js = [0],
        s1 = cst_abcdef,
        s2 = cst_abcdef,
        s4 = cst_npi$0,
        s5 = cst_npi,
        s7 = cst_abc_def,
        s8 = cst_abc_def,
        s1_bis = cst_abcdef,
        s2_bis = cst_abcdef,
        s4_bis = cst_npi$0,
        s5_bis = cst_npi,
        s7_bis = cst_abc_def,
        s8_bis = cst_abc_def,
        Test =
          [0,
           Js,
           s1,
           s2,
           s3,
           s4,
           s5,
           s6,
           s7,
           s8,
           s9,
           s1_bis,
           s2_bis,
           s3_bis,
           s4_bis,
           s5_bis,
           s6_bis,
           s7_bis,
           s8_bis,
           s9_bis];
       runtime.caml_register_global(18, Test, "Test");
       return;
      }
      (globalThis));
    //end |}];
  print_program (program ~share:true ~js_string:false);
  [%expect
    {|
    (function(globalThis){
       "use strict";
       var
        str_npi_xcf_x80 = "npi\xcf\x80",
        str_abcdef = "abcdef",
        str_npi = "npiπ",
        str_abc_def = "abc\\def",
        runtime = globalThis.jsoo_runtime,
        caml_string_of_jsbytes = runtime.caml_string_of_jsbytes,
        s3 = caml_string_of_jsbytes(str_abcdef),
        s6 = caml_string_of_jsbytes(str_npi_xcf_x80),
        s9 = caml_string_of_jsbytes(str_abc_def),
        s3_bis = caml_string_of_jsbytes(str_abcdef),
        s6_bis = caml_string_of_jsbytes(str_npi_xcf_x80),
        s9_bis = caml_string_of_jsbytes(str_abc_def),
        Js = [0],
        s1 = str_abcdef,
        s2 = str_abcdef,
        s4 = str_npi,
        s5 = str_npi_xcf_x80,
        s7 = str_abc_def,
        s8 = str_abc_def,
        s1_bis = str_abcdef,
        s2_bis = str_abcdef,
        s4_bis = str_npi,
        s5_bis = str_npi_xcf_x80,
        s7_bis = str_abc_def,
        s8_bis = str_abc_def,
        Test =
          [0,
           Js,
           s1,
           s2,
           s3,
           s4,
           s5,
           s6,
           s7,
           s8,
           s9,
           s1_bis,
           s2_bis,
           s3_bis,
           s4_bis,
           s5_bis,
           s6_bis,
           s7_bis,
           s8_bis,
           s9_bis];
       runtime.caml_register_global(18, Test, "Test");
       return;
      }
      (globalThis));
    //end |}];
  print_program (program ~share:false ~js_string:false);
  [%expect
    {|
    (function(globalThis){
       "use strict";
       var
        runtime = globalThis.jsoo_runtime,
        caml_string_of_jsbytes = runtime.caml_string_of_jsbytes,
        s3 = caml_string_of_jsbytes("abcdef"),
        s6 = caml_string_of_jsbytes("npi\xcf\x80"),
        s9 = caml_string_of_jsbytes("abc\\def"),
        s3_bis = caml_string_of_jsbytes("abcdef"),
        s6_bis = caml_string_of_jsbytes("npi\xcf\x80"),
        s9_bis = caml_string_of_jsbytes("abc\\def"),
        Js = [0],
        s1 = "abcdef",
        s2 = "abcdef",
        s4 = "npiπ",
        s5 = "npi\xcf\x80",
        s7 = "abc\\def",
        s8 = "abc\\def",
        s1_bis = "abcdef",
        s2_bis = "abcdef",
        s4_bis = "npiπ",
        s5_bis = "npi\xcf\x80",
        s7_bis = "abc\\def",
        s8_bis = "abc\\def",
        Test =
          [0,
           Js,
           s1,
           s2,
           s3,
           s4,
           s5,
           s6,
           s7,
           s8,
           s9,
           s1_bis,
           s2_bis,
           s3_bis,
           s4_bis,
           s5_bis,
           s6_bis,
           s7_bis,
           s8_bis,
           s9_bis];
       runtime.caml_register_global(18, Test, "Test");
       return;
      }
      (globalThis));
    //end |}]
