(* Js_of_ocaml tests
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

(* https://github.com/ocsigen/js_of_ocaml/issues/1559 *)

let%expect_test _ =
  let prog =
    {|
let my_ref = ref 1

module _ : sig end = struct
  type 'a thing =
    | Thing of 'a
    | No

  let f2 t =
    match t with
    | Thing 1 -> true
    | Thing _ | No -> false
  ;;

  let length = function
    | Thing i -> i
    | No -> -1
  ;;

  let () =
    let init = Thing 1 in
    let nesting = 1 in
    let rec handle_state t =
      let this_will_be_undefined () = if f2 t then 1 else 2 in
      match length t with
      | 0 -> this_will_be_undefined ()
      | 1 -> if Stdlib.Int.equal nesting 0 then nesting else this_will_be_undefined ()
      | _ -> handle_state (Thing 0)
    in
    print_endline (Int.to_string (handle_state init))
  ;;

  let _ : _ thing = No
end

let () = my_ref := 2
|}
  in
  Util.compile_and_run prog;
  [%expect {|
    1 |}];
  let program = Util.compile_and_parse prog in
  Util.print_program program;
  [%expect.unreachable]
[@@expect.uncaught_exn
  {|
  (* CR expect_test_collector: This test expectation appears to contain a backtrace.
     This is strongly discouraged as backtraces are fragile.
     Please change this test to not include a backtrace. *)

  (Failure "non-zero exit code")
  Raised at Stdlib__Buffer.add_channel in file "buffer.ml", line 211, characters 18-35
  Called from Jsoo_compiler_expect_tests_helper__Util.channel_to_string.loop in file "compiler/tests-compiler/util/util.ml", line 169, characters 4-52
  Called from Jsoo_compiler_expect_tests_helper__Util.channel_to_string in file "compiler/tests-compiler/util/util.ml", line 172, characters 7-14

  Trailing output
  ---------------
  Some variables escaped (#1). Use [--debug js_assign] for more info.
  /home/hugo/js_of_ocaml/_build/default/compiler/bin-js_of_ocaml/js_of_ocaml.exe: You found a bug. Please report it at https://github.com/ocsigen/js_of_ocaml/issues :
  Error: File "compiler/lib/js_assign.ml", line 442, characters 5-11: Assertion failed

  process exited with error code 125
   /home/hugo/js_of_ocaml/_build/default/compiler/bin-js_of_ocaml/js_of_ocaml.exe --pretty --sourcemap --disable=effects --disable=use-js-string --disable header test.cmo -o test.js |}]
