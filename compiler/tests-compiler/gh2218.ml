(* Js_of_ocaml compiler
 * http://www.ocsigen.org/js_of_ocaml/
 * Copyright (C) 2026 Jérôme Vouillon
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

let%expect_test "Js_traverse.simpl ordering" =
  let p =
    {|
let f () =
  let rec process b s = if b then s ^ "" else process true s in
  let maybe_process ?(b = false) string =
    if b then process false string else string in
  let wrap f = fun ?b string -> f (maybe_process ?b string) in
  let print_endline = wrap print_endline in
  fun string -> print_endline string
   |}
  in
  let p = compile_and_parse ~flags:[ "--debug=invariant" ] p in
  print_fun_decl p (Some "f");
  [%expect.unreachable]
[@@expect.uncaught_exn
  {|
  (* CR expect_test_collector: This test expectation appears to contain a backtrace.
     This is strongly discouraged as backtraces are fragile.
     Please change this test to not include a backtrace. *)
  (Failure "non-zero exit code")
  Raised at Stdlib__Buffer.add_channel in file "buffer.ml", line 213, characters 18-35
  Called from Jsoo_compiler_expect_tests_helper__Util.channel_to_string.loop in file "compiler/tests-compiler/util/util.ml", line 169, characters 4-52
  Called from Jsoo_compiler_expect_tests_helper__Util.channel_to_string in file "compiler/tests-compiler/util/util.ml", line 172, characters 7-14

  Trailing output
  ---------------
  Some variables escaped (#1). Use [--debug js_assign] for more info.
  /home/jerome/js_of_ocaml/_build/default/compiler/bin-js_of_ocaml/js_of_ocaml.exe: You found a bug. Please report it at https://github.com/ocsigen/js_of_ocaml/issues :
  Error: File "compiler/lib/js_assign.ml", line 503, characters 5-11: Assertion failed

  process exited with error code 125
   /home/jerome/js_of_ocaml/_build/default/compiler/bin-js_of_ocaml/js_of_ocaml.exe --pretty --debug var --sourcemap --effects=disabled --disable=use-js-string --disable header --debug=invariant --Werror test.cmo -o test.js
  |}]
