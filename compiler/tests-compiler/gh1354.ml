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
let p x = Printf.eprintf "%d\n" x in
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
    (function(globalThis){
       "use strict";
       var
        runtime = globalThis.jsoo_runtime,
        caml_maybe_attach_backtrace = runtime.caml_maybe_attach_backtrace,
        caml_wrap_exception = runtime.caml_wrap_exception;
       function caml_call2(f, a0, a1){
        return (f.l >= 0 ? f.l : f.l = f.length) === 2
                ? f(a0, a1)
                : runtime.caml_call_gen(f, [a0, a1]);
       }
       var
        global_data = runtime.caml_get_global_data(),
        Stdlib = global_data.Stdlib,
        Stdlib_Printf = global_data.Stdlib__Printf,
        _a_ = 0,
        _b_ = _a_,
        _d_ =
          [0, [4, 0, 0, 0, [12, 10, 0]], runtime.caml_string_of_jsbytes("%d\n")];
       try{0; _b_ = _a_ + 1 | 0; throw caml_maybe_attach_backtrace(Stdlib[3], 1);}
       catch(_e_){
        var _c_ = caml_wrap_exception(_e_);
        if(_c_ !== Stdlib[3]) throw caml_maybe_attach_backtrace(_c_, 0);
        caml_call2(Stdlib_Printf[3], _d_, _b_ | 0);
        var Test = [0];
        runtime.caml_register_global(3, Test, "Test");
        0;
        return;
       }
      }
      (globalThis));
    //end
    |}];
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
  (try incr x ; raise Exit with _ -> Printf.eprintf "%d %d\n" (!x + 0) y);
  raise Exit
with Exit ->
  Printf.eprintf "%d %d\n" (!x + 0) y
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
    (function(globalThis){
       "use strict";
       var
        runtime = globalThis.jsoo_runtime,
        caml_maybe_attach_backtrace = runtime.caml_maybe_attach_backtrace,
        caml_string_of_jsbytes = runtime.caml_string_of_jsbytes,
        caml_wrap_exception = runtime.caml_wrap_exception;
       function caml_call3(f, a0, a1, a2){
        return (f.l >= 0 ? f.l : f.l = f.length) === 3
                ? f(a0, a1, a2)
                : runtime.caml_call_gen(f, [a0, a1, a2]);
       }
       var
        global_data = runtime.caml_get_global_data(),
        Stdlib = global_data.Stdlib,
        Stdlib_Printf = global_data.Stdlib__Printf,
        _c_ = 0,
        _d_ = 0,
        _a_ = _c_,
        _i_ =
          [0,
           [4, 0, 0, 0, [12, 32, [4, 0, 0, 0, [12, 10, 0]]]],
           caml_string_of_jsbytes("%d %d\n")],
        _h_ =
          [0,
           [4, 0, 0, 0, [12, 32, [4, 0, 0, 0, [12, 10, 0]]]],
           caml_string_of_jsbytes("%d %d\n")];
       try{
        var _b_ = _c_ + 1 | 0;
        0;
        _a_ = _b_;
        var _f_ = _b_;
        try{
         var _g_ = _b_ + 1 | 0;
         0;
         _f_ = _g_;
         _a_ = _g_;
         throw caml_maybe_attach_backtrace(Stdlib[3], 1);
        }
        catch(_j_){
         caml_call3(Stdlib_Printf[3], _i_, _f_ | 0, _d_);
         throw caml_maybe_attach_backtrace(Stdlib[3], 1);
        }
       }
       catch(_i_){
        var _e_ = caml_wrap_exception(_i_);
        if(_e_ !== Stdlib[3]) throw caml_maybe_attach_backtrace(_e_, 0);
        caml_call3(Stdlib_Printf[3], _h_, _a_ | 0, _d_);
        var Test = [0];
        runtime.caml_register_global(4, Test, "Test");
        0;
        return;
       }
      }
      (globalThis));
    //end
    |}];
  Util.compile_and_run ~debug:false prog;
  [%expect {|
    2 0
    2 0|}]

let%expect_test _ =
  let prog =
    {|
let x = ref 0 in
try
  (try incr x ; raise Exit with _ -> Printf.eprintf "%d\n" !x);
  raise Exit
with Exit ->
  Printf.eprintf "%d\n" !x
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
    (function(globalThis){
       "use strict";
       var
        runtime = globalThis.jsoo_runtime,
        caml_maybe_attach_backtrace = runtime.caml_maybe_attach_backtrace,
        caml_string_of_jsbytes = runtime.caml_string_of_jsbytes,
        caml_wrap_exception = runtime.caml_wrap_exception;
       function caml_call2(f, a0, a1){
        return (f.l >= 0 ? f.l : f.l = f.length) === 2
                ? f(a0, a1)
                : runtime.caml_call_gen(f, [a0, a1]);
       }
       var
        global_data = runtime.caml_get_global_data(),
        Stdlib = global_data.Stdlib,
        Stdlib_Printf = global_data.Stdlib__Printf,
        _a_ = 0,
        _b_ = _a_,
        _g_ = [0, [4, 0, 0, 0, [12, 10, 0]], caml_string_of_jsbytes("%d\n")],
        _f_ = [0, [4, 0, 0, 0, [12, 10, 0]], caml_string_of_jsbytes("%d\n")];
       try{
        var _d_ = _a_;
        try{
         var _e_ = _a_ + 1 | 0;
         0;
         _d_ = _e_;
         _b_ = _e_;
         throw caml_maybe_attach_backtrace(Stdlib[3], 1);
        }
        catch(_h_){
         caml_call2(Stdlib_Printf[3], _g_, _d_);
         throw caml_maybe_attach_backtrace(Stdlib[3], 1);
        }
       }
       catch(_g_){
        var _c_ = caml_wrap_exception(_g_);
        if(_c_ !== Stdlib[3]) throw caml_maybe_attach_backtrace(_c_, 0);
        caml_call2(Stdlib_Printf[3], _f_, _b_);
        var Test = [0];
        runtime.caml_register_global(4, Test, "Test");
        0;
        return;
       }
      }
      (globalThis));
    //end
    |}];
  Util.compile_and_run ~debug:false prog;
  [%expect {|
    1
    1 |}]
