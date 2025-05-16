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

let%expect_test "test-compiler/lib-effects/test1.ml" =
  let code =
    compile_and_parse
      ~effects:`Double_translation
      {|
         (* Function calls at toplevel outside of loops do not use
            [caml_callback] when double translation is enabled. *)
         let g () = Printf.printf "abc" in
         let f () = for i = 1 to 5 do g () done in
         g (); f (); g ()
       |}
  in
  print_program code;
  [%expect
    {|
    (function(globalThis){
       "use strict";
       var
        runtime = globalThis.jsoo_runtime,
        caml_string_of_jsbytes = runtime.caml_string_of_jsbytes;
       function caml_call1(f, a0){
        return (f.l >= 0 ? f.l : f.l = f.length) === 1
                ? f(a0)
                : runtime.caml_call_gen(f, [a0]);
       }
       function caml_trampoline_cps_call2(f, a0, a1){
        return runtime.caml_stack_check_depth()
                ? f.cps
                  ? (f.cps.l
                      >= 0
                      ? f.cps.l
                      : f.cps.l = f.cps.length)
                    === 2
                    ? f.cps.call(null, a0, a1)
                    : runtime.caml_call_gen_cps(f, [a0, a1])
                  : a1
                    ((f.l >= 0 ? f.l : f.l = f.length) === 1
                      ? f(a0)
                      : runtime.caml_call_gen(f, [a0]))
                : runtime.caml_trampoline_return(f, [a0, a1], 0);
       }
       var
        dummy = 0,
        global_data = runtime.caml_get_global_data(),
        _a_ =
          [0,
           [11, caml_string_of_jsbytes("abc"), 0],
           caml_string_of_jsbytes("abc")],
        Stdlib_Printf = global_data.Stdlib__Printf;
       function g$0(param){return caml_call1(Stdlib_Printf[2], _a_);}
       function g$1(param, cont){
        return caml_trampoline_cps_call2(Stdlib_Printf[2], _a_, cont);
       }
       var g = runtime.caml_cps_closure(g$0, g$1);
       g();
       var i = 1;
       for(;;){
        g();
        var _b_ = i + 1 | 0;
        if(5 === i){
         g();
         var Test = [0];
         runtime.caml_register_global(2, Test, "Test");
         return;
        }
        i = _b_;
       }
      }
      (globalThis));
    //end
    |}]
