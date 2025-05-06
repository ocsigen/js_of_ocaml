(* Js_of_ocaml tests
 * http://www.ocsigen.org/js_of_ocaml/
 * Copyright (C) 2024 Jérôme Vouillon
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

(* https://github.com/ocsigen/js_of_ocaml/pull/1768 *)

let%expect_test _ =
  let prog =
    {|
let () =
  let h x = x := (fun x y -> x + y) in
  let f () = ref (fun _ -> assert false) in
  let x = f() in
  let g () = !x 7 in
  h x;
  assert (g () 3 = 10)
|}
  in
  let program =
    Util.compile_and_parse
      ~flags:(List.concat [ [ "--opt"; "3" ]; [ "--no-inline" ] ])
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
        caml_string_of_jsbytes = runtime.caml_string_of_jsbytes;
       function caml_call1(f, a0){
        return (f.l >= 0 ? f.l : f.l = f.length) === 1
                ? f(a0)
                : runtime.caml_call_gen(f, [a0]);
       }
       var
        dummy = 0,
        global_data = runtime.caml_get_global_data(),
        Assert_failure = global_data.Assert_failure,
        _a_ = [0, caml_string_of_jsbytes("test.ml"), 4, 27];
       function h(x){x[1] = function(x, y){return x + y | 0;};}
       function f(param){
        return [0,
                function(param){
                 throw caml_maybe_attach_backtrace([0, Assert_failure, _a_], 1);
                }];
       }
       var x = f();
       function g(param){return caml_call1(x[1], 7);}
       h(x);
       var _b_ = [0, caml_string_of_jsbytes("test.ml"), 8, 2];
       if(10 !== caml_call1(g(), 3))
        throw caml_maybe_attach_backtrace([0, Assert_failure, _b_], 1);
       var Test = [0];
       runtime.caml_register_global(3, Test, "Test");
       return;
      }
      (globalThis));
    //end
    |}]
