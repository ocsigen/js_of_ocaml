(* Js_of_ocaml compiler
 * http://www.ocsigen.org/js_of_ocaml/
 * Copyright (C) 2025 Hugo Heuzard
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

let%expect_test "inlining" =
  let p =
    {|
module X : sig
end = struct
  external fun_to_js : (unit -> unit) -> unit = "foo"
  
  let[@tail_mod_cons] rec aux f = f () :: aux f
  
  let map_to_list f = aux (fun x -> f x)
  
  let embedded_input_file_handler _ =
    fun_to_js (fun _ ->
        let _ = map_to_list (fun _ -> assert false) in
        ())
  
  let _ = embedded_input_file_handler ()
end
   |}
  in
  let p = compile_and_parse ~flags:[ "--debug"; "js_assign" ] p in
  print_program p;
  [%expect.unreachable]
[@@expect.uncaught_exn {|
  (* CR expect_test_collector: This test expectation appears to contain a backtrace.
     This is strongly discouraged as backtraces are fragile.
     Please change this test to not include a backtrace. *)
  (Failure "non-zero exit code")
  Raised at Stdlib__Buffer.add_channel in file "buffer.ml", line 213, characters 18-35
  Called from Jsoo_compiler_expect_tests_helper__Util.channel_to_string.loop in file "compiler/tests-compiler/util/util.ml", line 169, characters 4-52
  Called from Jsoo_compiler_expect_tests_helper__Util.channel_to_string in file "compiler/tests-compiler/util/util.ml", line 172, characters 7-14

  Trailing output
  ---------------
  (function(globalThis){
     "use strict";
     var
      runtime = globalThis.jsoo_runtime,
      caml_maybe_attach_backtrace = runtime.caml_maybe_attach_backtrace;
     function caml_call1(f, a0){
      return (f.l >= 0 ? f.l : f.l = f.length) === 1
              ? f(a0)
              : runtime.caml_call_gen(f, [a0]);
     }
     var
      global_data = runtime.caml_get_global_data(),
      Assert_failure = global_data.Assert_failure,
      _a_ = [0, runtime.caml_string_of_jsbytes("test.ml"), 12, 38],
      dummy = 0;
     runtime.foo
      (function(param){
        caml_call1(<v30{f}>, dummy);
        throw caml_maybe_attach_backtrace([0, Assert_failure, _a_], 1);
       });
     var X = [0], Test = [0, X];
     runtime.caml_register_global(2, Test, "Test");
     return;
    }
    (globalThis));
  Some variables escaped: <v30{f}>
  /home/hugo/js_of_ocaml/_build/default/compiler/bin-js_of_ocaml/js_of_ocaml.exe: You found a bug. Please report it at https://github.com/ocsigen/js_of_ocaml/issues :
  Error: File "compiler/lib/js_assign.ml", line 503, characters 5-11: Assertion failed

  process exited with error code 125
   /home/hugo/js_of_ocaml/_build/default/compiler/bin-js_of_ocaml/js_of_ocaml.exe --pretty --debug var --sourcemap --effects=disabled --disable=use-js-string --disable header --debug js_assign --Werror test.cmo -o test.js
  |}]
