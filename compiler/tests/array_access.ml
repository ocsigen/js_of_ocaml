(* Js_of_ocaml tests
 * http://www.ocsigen.org/js_of_ocaml/
 * Copyright (C) 2019 Hugo Heuzard
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

let array_set =
  {|
   let some_name a n =
     let x = a.(n) <- n in
     x = ()
   let a = [|1;2;3|]
   let () = assert (some_name a 2)
   |}

let%expect_test "array_set" =
  let compile s =
    s
    |> Filetype.ocaml_text_of_string
    |> Filetype.write_ocaml
    |> compile_ocaml_to_cmo
    |> compile_cmo_to_javascript ~pretty:true
    |> fst
    |> parse_js
  in
  let program = compile array_set in
  print_fun_decl program "some_name";
  [%expect
    {|
    function some_name(a,n)
     {var x=runtime.caml_check_bound(a,n)[1 + n] = n;return 0 === x?1:0} |}]

let%expect_test "array_set" =
  compile_and_run array_set;
  [%expect.unreachable]
  [@@expect.uncaught_exn
    {|
  (* CR expect_test_collector: This test expectation appears to contain a backtrace.
     This is strongly discouraged as backtraces are fragile.
     Please change this test to not include a backtrace. *)

  (Failure  "process exited with error code 1\
           \n node /tmp/jsoo_testb07018.js")
  Raised at file "stdlib.ml", line 33, characters 17-33
  Called from file "compiler/tests/util/util.ml", line 265, characters 2-148
  Called from file "compiler/tests/array_access.ml", line 56, characters 2-27
  Called from file "collector/expect_test_collector.ml", line 225, characters 12-19

  Trailing output
  --------------- |}]
