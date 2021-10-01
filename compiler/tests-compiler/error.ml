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

let%expect_test "uncaugh error" =
  let prog = {| let _ = raise Not_found |} in
  compile_and_run prog;
  [%expect
    {|
    /tmp/build76e80b.dune/jsoo-test5e0747/test.js:5368
        throw Not_found}
        ^
    248,Not_found,-7

    process exited with error code 1
     node test.js |}];
  compile_and_run_bytecode prog;
  [%expect
    {|
    Fatal error: exception Not_found

    process exited with error code 2
     ocamlrun test.bc |}];
  (* Test caml_format_exception by un-registeting  "Printexc.handle_uncaught_exception". Note that this hack unly work with jsoo *)
  let prog =
    {|
let null = Array.unsafe_get [|1|] 1
let () = Callback.register "Printexc.handle_uncaught_exception" null
exception C
let _ = raise C |}
  in
  compile_and_run prog;
  [%expect
    {|
    /tmp/build76e80b.dune/jsoo-test6c676e/test.js:5370
        throw C}
        ^
    248,Test.C,7

    process exited with error code 1
     node test.js |}];
  let prog =
    {|
let null = Array.unsafe_get [|1|] 1
let () = Callback.register "Printexc.handle_uncaught_exception" null
exception D of int * string * Int64.t
let _ = raise (D(2,"test",43L))
              |}
  in
  compile_and_run prog;
  [%expect
    {|
    /tmp/build76e80b.dune/jsoo-testf98b41/test.js:5372
        throw [0,D,2,_cN_,_cM_]}
        ^
    0,248,Test.D,7,2,test,[object Object]

    process exited with error code 1
     node test.js |}];
  let prog =
    {|
let null = Array.unsafe_get [|1|] 1
let () = Callback.register "Printexc.handle_uncaught_exception" null
let _ = assert false |}
  in
  compile_and_run prog;
  [%expect
    {|
    /tmp/build76e80b.dune/jsoo-testfe7fd7/test.js:5369
        throw [0,Assert_failure,_cL_]}
        ^
    0,248,Assert_failure,-11,0,test.ml,4,8

    process exited with error code 1
     node test.js |}];
  let prog =
    {|
let null = Array.unsafe_get [|1|] 1
let () = Callback.register "Printexc.handle_uncaught_exception" null
 [@@@ocaml.warning "-8"] let _ = match 3 with 2 -> () |}
  in
  compile_and_run prog;
  [%expect
    {|
    /tmp/build76e80b.dune/jsoo-test75a355/test.js:5369
        throw [0,Match_failure,_cL_]}
        ^
    0,248,Match_failure,-8,0,test.ml,4,33

    process exited with error code 1
     node test.js |}];

  (* Uncaught javascript exception *)
  let prog =
    {|
let null : _ -> _ -> _ = Array.unsafe_get [||] 0
let () = Callback.register "Printexc.handle_uncaught_exception" null
exception D of int * string * Int64.t
let _ = null 1 2
             |}
  in
  compile_and_run prog;
  let s = [%expect.output] in
  (try
     ignore (Str.search_forward (Str.regexp "TypeError:[^\n]+") s 0);
     print_endline (Str.matched_string s)
   with Not_found -> print_endline s);
  [%expect {|
    TypeError: Cannot read property 'length' of undefined |}]
