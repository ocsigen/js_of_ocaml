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
      ~effects:`Cps
      {|
         (* Function calls at toplevel outside of loops use
            [caml_callback]. *)
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
        caml_callback = runtime.caml_callback,
        caml_string_of_jsbytes = runtime.caml_string_of_jsbytes;
       function caml_exact_trampoline_call1(f, a0){
        return runtime.caml_stack_check_depth()
                ? f(a0)
                : runtime.caml_trampoline_return(f, [a0], 1);
       }
       function caml_trampoline_cps_call2(f, a0, a1){
        return runtime.caml_stack_check_depth()
                ? (f.l
                    >= 0
                    ? f.l
                    : f.l = f.length)
                  === 2
                  ? f(a0, a1)
                  : runtime.caml_call_gen(f, [a0, a1])
                : runtime.caml_trampoline_return(f, [a0, a1], 0);
       }
       function caml_exact_trampoline_cps_call(f, a0, a1){
        return runtime.caml_stack_check_depth()
                ? f(a0, a1)
                : runtime.caml_trampoline_return(f, [a0, a1], 0);
       }
       var
        dummy = 0,
        global_data = runtime.caml_get_global_data(),
        Stdlib_Printf = global_data.Stdlib__Printf,
        _a_ =
          [0,
           [11, caml_string_of_jsbytes("abc"), 0],
           caml_string_of_jsbytes("abc")];
       function g(param, cont){
        return caml_trampoline_cps_call2(Stdlib_Printf[2], _a_, cont);
       }
       function f(param, cont){
        function _a_(i){
         return caml_exact_trampoline_cps_call
                 (g,
                  dummy,
                  function(_c_){
                   var _b_ = i + 1 | 0;
                   return 5 !== i ? caml_exact_trampoline_call1(_a_, _b_) : cont();
                  });
        }
        return _a_(1);
       }
       caml_callback(g, [dummy]);
       caml_callback(f, [dummy]);
       caml_callback(g, [dummy]);
       var Test = [0];
       runtime.caml_register_global(2, Test, "Test");
       return;
      }
      (globalThis));
    //end
    |}]
