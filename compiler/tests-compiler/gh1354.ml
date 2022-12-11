(* Js_of_ocaml tests
 * http://www.ocsigen.org/js_of_ocaml/
 * Copyright (C) 2022 Hugo Heuzard
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

(* https://github.com/ocsigen/js_of_ocaml/issues/1354 *)

let%expect_test _ =
  let prog =
    {|
let x = ref 0 in
let p x = Format.eprintf "%d@." x in
try
  incr x;
  raise Exit
with Exit ->
  p (!x + 0)|}
  in
  let program =
    Util.compile_and_parse
      ~debug:false
      ~flags:
        (List.concat
           [ [ "--disable"; "inline" ]
           ; [ "--disable"; "deadcode" ]
           ; [ "--disable"; "staticeval" ]
           ])
      prog
  in
  Util.print_program program;
  [%expect
    {|
    (function(globalThis)
       {"use strict";
        var
         runtime=globalThis.jsoo_runtime,
         caml_wrap_exception=runtime.caml_wrap_exception;
        function caml_call2(f,a0,a1)
         {return f.length == 2?f(a0,a1):runtime.caml_call_gen(f,[a0,a1])}
        var
         global_data=runtime.caml_get_global_data(),
         Stdlib=global_data.Stdlib,
         Stdlib_Format=global_data.Stdlib__Format,
         _c_=[0,[4,0,0,0,[17,4,0]],runtime.caml_string_of_jsbytes("%d@.")],
         _a_=0;
        try
         {var _b_=_a_;0;_b_ = _a_ + 1 | 0;throw Stdlib[3]}
        catch(_d_)
         {_d_ = caml_wrap_exception(_d_);
          if(_d_ !== Stdlib[3])throw _d_;
          caml_call2(Stdlib_Format[131],_c_,_b_ | 0);
          var Test=[0];
          runtime.caml_register_global(3,Test,"Test");
          0;
          return}}
      (globalThis));
    //end |}];
  Util.compile_and_run ~debug:false prog;
  [%expect {|
    1 |}]

let%expect_test _ =
  let prog =
    {|
let x = ref 0 in
let y = 0 in
try
  incr x ;
  (try incr x ; raise Exit with _ -> Format.eprintf "%d %d@." (!x + 0) y);
  raise Exit
with Exit ->
  Format.eprintf "%d %d@." (!x + 0) y
|}
  in
  let program =
    Util.compile_and_parse
      ~debug:false
      ~flags:
        (List.concat
           [ [ "--disable"; "inline" ]
           ; [ "--disable"; "deadcode" ]
           ; [ "--disable"; "staticeval" ]
           ])
      prog
  in
  Util.print_program program;
  [%expect
    {|
    (function(globalThis)
       {"use strict";
        var
         runtime=globalThis.jsoo_runtime,
         caml_string_of_jsbytes=runtime.caml_string_of_jsbytes,
         caml_wrap_exception=runtime.caml_wrap_exception;
        function caml_call3(f,a0,a1,a2)
         {return f.length == 3?f(a0,a1,a2):runtime.caml_call_gen(f,[a0,a1,a2])}
        var
         global_data=runtime.caml_get_global_data(),
         Stdlib=global_data.Stdlib,
         Stdlib_Format=global_data.Stdlib__Format,
         _g_=
          [0,
           [4,0,0,0,[12,32,[4,0,0,0,[17,4,0]]]],
           caml_string_of_jsbytes("%d %d@.")],
         _d_=
          [0,
           [4,0,0,0,[12,32,[4,0,0,0,[17,4,0]]]],
           caml_string_of_jsbytes("%d %d@.")],
         _a_=0,
         _b_=0;
        try
         {var _c_=_a_,_e_=_a_ + 1 | 0;
          0;
          _c_ = _e_;
          try
           {var _f_=_e_,_h_=_e_ + 1 | 0;0;_f_ = _h_;_c_ = _h_;throw Stdlib[3]}
          catch(_j_)
           {caml_call3(Stdlib_Format[131],_g_,_f_ | 0,_b_);throw Stdlib[3]}}
        catch(_i_)
         {_i_ = caml_wrap_exception(_i_);
          if(_i_ !== Stdlib[3])throw _i_;
          caml_call3(Stdlib_Format[131],_d_,_c_ | 0,_b_);
          var Test=[0];
          runtime.caml_register_global(4,Test,"Test");
          0;
          return}}
      (globalThis));
    //end |}];
  Util.compile_and_run ~debug:false prog;
  [%expect {|
    2 0
    2 0|}]

let%expect_test _ =
  let prog =
    {|
let x = ref 0 in
try
  (try incr x ; raise Exit with _ -> Format.eprintf "%d@." !x);
  raise Exit
with Exit ->
  Format.eprintf "%d@." !x
|}
  in
  let program =
    Util.compile_and_parse
      ~debug:false
      ~flags:
        (List.concat
           [ [ "--disable"; "inline" ]
           ; [ "--disable"; "deadcode" ]
           ; [ "--disable"; "staticeval" ]
           ])
      prog
  in
  Util.print_program program;
  [%expect
    {|
    (function(globalThis)
       {"use strict";
        var
         runtime=globalThis.jsoo_runtime,
         caml_string_of_jsbytes=runtime.caml_string_of_jsbytes,
         caml_wrap_exception=runtime.caml_wrap_exception;
        function caml_call2(f,a0,a1)
         {return f.length == 2?f(a0,a1):runtime.caml_call_gen(f,[a0,a1])}
        var
         global_data=runtime.caml_get_global_data(),
         Stdlib=global_data.Stdlib,
         Stdlib_Format=global_data.Stdlib__Format,
         _e_=[0,[4,0,0,0,[17,4,0]],caml_string_of_jsbytes("%d@.")],
         _c_=[0,[4,0,0,0,[17,4,0]],caml_string_of_jsbytes("%d@.")],
         _a_=0;
        try
         {var _b_=_a_;
          try
           {var _d_=_a_,_f_=_a_ + 1 | 0;0;_d_ = _f_;_b_ = _f_;throw Stdlib[3]}
          catch(_h_){caml_call2(Stdlib_Format[131],_e_,_d_);throw Stdlib[3]}}
        catch(_g_)
         {_g_ = caml_wrap_exception(_g_);
          if(_g_ !== Stdlib[3])throw _g_;
          caml_call2(Stdlib_Format[131],_c_,_b_);
          var Test=[0];
          runtime.caml_register_global(4,Test,"Test");
          0;
          return}}
      (globalThis));
    //end |}];
  Util.compile_and_run ~debug:false prog;
  [%expect {|
    1
    1 |}]
