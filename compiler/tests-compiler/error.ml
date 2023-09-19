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

let normalize x =
  x
  |> Str.global_replace (Str.regexp "ocamlrun\\(.exe\\)?") "%{OCAMLRUN}"
  |> Str.global_replace (Str.regexp "node\\(.exe\\)?") "%{NODE}"

let%expect_test "uncaugh error" =
  let prog = {| let _ = raise Not_found |} in
  compile_and_run prog;
  print_endline (normalize [%expect.output]);
  [%expect
    {|
    Fatal error: exception Not_found

    process exited with error code 2
     %{NODE} test.js |}];
  compile_and_run_bytecode prog;
  print_endline (normalize [%expect.output]);
  [%expect
    {|
    Fatal error: exception Not_found

    process exited with error code 2
     %{OCAMLRUN} test.bc |}];
  (* Test caml_format_exception by un-registeting  "Printexc.handle_uncaught_exception". Note that this hack unly work with jsoo *)
  let prog =
    {|
let null = Array.unsafe_get [|1|] 1
let () = Callback.register "Printexc.handle_uncaught_exception" null
exception C
let _ = raise C |}
  in
  compile_and_run prog;
  print_endline (normalize [%expect.output]);
  [%expect
    {|
    Fatal error: exception Test.C

    process exited with error code 2
     %{NODE} test.js |}];
  let prog =
    {|
let null = Array.unsafe_get [|1|] 1
let () = Callback.register "Printexc.handle_uncaught_exception" null
exception D of int * string * Int64.t
let _ = raise (D(2,"test",43L))
              |}
  in
  compile_and_run prog;
  print_endline (normalize [%expect.output]);
  [%expect
    {|
    Fatal error: exception Test.D(2, "test", _)

    process exited with error code 2
     %{NODE} test.js |}];
  let prog =
    {|
let null = Array.unsafe_get [|1|] 1
let () = Callback.register "Printexc.handle_uncaught_exception" null
let _ = assert false |}
  in
  compile_and_run prog;
  print_endline (normalize [%expect.output]);
  [%expect
    {|
    Fatal error: exception Assert_failure("test.ml", 4, 8)

    process exited with error code 2
     %{NODE} test.js |}];
  let prog =
    {|
let null = Array.unsafe_get [|1|] 1
let () = Callback.register "Printexc.handle_uncaught_exception" null
 [@@@ocaml.warning "-8"] let _ = match 3 with 2 -> () |}
  in
  compile_and_run prog;
  print_endline (normalize [%expect.output]);
  [%expect
    {|
    Fatal error: exception Match_failure("test.ml", 4, 33)

    process exited with error code 2
     %{NODE} test.js |}];

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
  let s = normalize [%expect.output] in
  (try ignore (Str.search_forward (Str.regexp "TypeError: Cannot read") s 0)
   with Not_found -> print_endline s);
  [%expect {||}]
