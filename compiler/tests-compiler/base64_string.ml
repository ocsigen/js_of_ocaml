(* Js_of_ocaml tests
 * http://www.ocsigen.org/js_of_ocaml/
 * Copyright (C) 2026 Hugo Heuzard
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

(* Large binary string constants are emitted as base64 literals, decoded
   once in the shared header; short ones stay escaped literals. *)

let%expect_test _ =
  let program =
    compile_and_parse
      ~debug:false
      ~use_js_string:true
      {|
let big = "\000\001\002\003\004\005\006\007\008\009\010\011\012\013\014\015\016\017\018\019\020\021\022\023\024\025\026\027\028\029\030\031\032\033\034\035\036\037\038\039\040\041\042\043\044\045\046\047\048\049\050\051\052\053\054\055\056\057\058\059\060\061\062\063"

let small = "\xc3\xa9"

let tables = [| "\000\001\000\255\000\002\000\255\000\001\000\255\000\002\000\255\000\001\000\255\000\002\000\255\000\001\000\255\000\002\000\255\000\001\000\255\000\002\000\255\000\001\000\255\000\002\000\255"; "\000\001\000\255\000\002\000\255\000\001\000\255\000\002\000\255\000\001\000\255\000\002\000\255\000\001\000\255\000\002\000\255\000\001\000\255\000\002\000\255\000\001\000\255\000\002\000\255" |]

let () = print_string big; print_string small; print_string tables.(0)
|}
  in
  print_program program;
  [%expect
    {|
    (function(globalThis){
       "use strict";
       var
        runtime = globalThis.jsoo_runtime,
        caml_string_of_base64 = runtime.caml_string_of_base64,
        _a_ =
          caml_string_of_base64
           ("AAEA/wACAP8AAQD/AAIA/wABAP8AAgD/AAEA/wACAP8AAQD/AAIA/wABAP8AAgD/"),
        _b_ =
          caml_string_of_base64
           ("AAECAwQFBgcICQoLDA0ODxAREhMUFRYXGBkaGxwdHh8gISIjJCUmJygpKissLS4vMDEyMzQ1Njc4OTo7PD0+Pw");
       function caml_call1(f, a0){
        return (f.l >= 0 ? f.l : f.l = f.length) === 1
                ? f(a0)
                : runtime.caml_call_gen(f, [a0]);
       }
       var Stdlib = runtime.caml_get_global("Stdlib");
       caml_call1(Stdlib[42], _b_);
       var cst = "\xc3\xa9";
       caml_call1(Stdlib[42], cst);
       _a_ = [0, _a_, _a_];
       var _c_ = runtime.caml_check_bound(_a_, 0)[1];
       caml_call1(Stdlib[42], _c_);
       runtime.caml_register_global([0, _b_, cst, _a_], "Test");
       return;
      }
      (globalThis));
    //end
    |}]

let%expect_test _ =
  let program =
    compile_and_parse
      ~debug:false
      ~use_js_string:false
      {|
let big = "\000\001\002\003\004\005\006\007\008\009\010\011\012\013\014\015\016\017\018\019\020\021\022\023\024\025\026\027\028\029\030\031\032\033\034\035\036\037\038\039\040\041\042\043\044\045\046\047\048\049\050\051\052\053\054\055\056\057\058\059\060\061\062\063"

let small = "\xc3\xa9"

let tables = [| "\000\001\000\255\000\002\000\255\000\001\000\255\000\002\000\255\000\001\000\255\000\002\000\255\000\001\000\255\000\002\000\255\000\001\000\255\000\002\000\255\000\001\000\255\000\002\000\255"; "\000\001\000\255\000\002\000\255\000\001\000\255\000\002\000\255\000\001\000\255\000\002\000\255\000\001\000\255\000\002\000\255\000\001\000\255\000\002\000\255\000\001\000\255\000\002\000\255" |]

let () = print_string big; print_string small; print_string tables.(0)
|}
  in
  print_program program;
  [%expect
    {|
    (function(globalThis){
       "use strict";
       var
        runtime = globalThis.jsoo_runtime,
        caml_string_of_base64 = runtime.caml_string_of_base64,
        _a_ =
          caml_string_of_base64
           ("AAEA/wACAP8AAQD/AAIA/wABAP8AAgD/AAEA/wACAP8AAQD/AAIA/wABAP8AAgD/"),
        _b_ =
          caml_string_of_base64
           ("AAECAwQFBgcICQoLDA0ODxAREhMUFRYXGBkaGxwdHh8gISIjJCUmJygpKissLS4vMDEyMzQ1Njc4OTo7PD0+Pw");
       function caml_call1(f, a0){
        return (f.l >= 0 ? f.l : f.l = f.length) === 1
                ? f(a0)
                : runtime.caml_call_gen(f, [a0]);
       }
       var
        cst = runtime.caml_string_of_jsbytes("\xc3\xa9"),
        Stdlib = runtime.caml_get_global("Stdlib");
       caml_call1(Stdlib[42], _b_);
       caml_call1(Stdlib[42], cst);
       _a_ = [0, _a_, _a_];
       var _c_ = runtime.caml_check_bound(_a_, 0)[1];
       caml_call1(Stdlib[42], _c_);
       runtime.caml_register_global([0, _b_, cst, _a_], "Test");
       return;
      }
      (globalThis));
    //end
    |}]
