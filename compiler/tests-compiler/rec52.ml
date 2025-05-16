(* Js_of_ocaml compiler
 * http://www.ocsigen.org/js_of_ocaml/
 * Copyright (C) 2024 Hugo Heuzard
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

let%expect_test "let rec" =
  let p =
    {|
      let rec a x =
        (* syntactic function *)
        b x
      and b =
        (* non-syntactic function *)
        let tbl : (int, int) Hashtbl.t = Hashtbl.create 17 in
        fun x -> `T (tbl, c, a 0)
      and c =
        (* block *)
        Some (d, default)
      and d =
        (* 'dynamic' value (not recursive *)
        Array.make 5 0
      and default =
        (* constant, with (spurious) use
           of a recursive neighbor *)
        let _ = a in
        42
   |}
  in
  let p = compile_and_parse p in
  print_program p;
  let s = [%expect.output] in
  let s =
    Str.global_replace (Str.regexp "runtime.caml_make_vect") "runtime.caml_array_make" s
  in
  print_endline s;
  [%expect
    {|
    (function(globalThis){
       "use strict";
       var
        runtime = globalThis.jsoo_runtime,
        caml_update_dummy = runtime.caml_update_dummy;
       function caml_call2(f, a0, a1){
        return (f.l >= 0 ? f.l : f.l = f.length) === 2
                ? f(a0, a1)
                : runtime.caml_call_gen(f, [a0, a1]);
       }
       var
        global_data = runtime.caml_get_global_data(),
        Stdlib_Hashtbl = global_data.Stdlib__Hashtbl,
        letrec_function_context = [],
        c = [],
        d = runtime.caml_array_make(5, 0),
        default$ = 42;
       function a(x){return b(x);}
       function b(x){
        var _a_ = b(0);
        return [0, 84, [0, letrec_function_context[1], c, _a_]];
       }
       var tbl = caml_call2(Stdlib_Hashtbl[1], 0, 17);
       caml_update_dummy(letrec_function_context, [0, tbl]);
       caml_update_dummy(c, [0, [0, d, default$]]);
       var Test = [0, a, b, c, d, default$];
       runtime.caml_register_global(1, Test, "Test");
       return;
      }
      (globalThis));
    //end
    |}]
