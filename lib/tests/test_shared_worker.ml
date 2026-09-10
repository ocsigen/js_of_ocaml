(* Js_of_ocaml
 * http://www.ocsigen.org/js_of_ocaml/
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
 *)

open Js_of_ocaml

(* [SharedWorker] is browser-only (neither Node.js nor QuickJS provide it), so
   the suite only exercises the parts that do not depend on the global being
   present: building the options record and the capability probes. *)

let%expect_test "options record can be populated" =
  let o = SharedWorker.empty_worker_options () in
  o##.name := Js.string "shared";
  o##._type := Js.string "module";
  o##.credentials := Js.string "same-origin";
  (* The fields are write-only; read them back through an unsafe coercion to
     check they were stored on the object. *)
  print_endline (Js.to_string (Js.Unsafe.coerce o)##.name);
  print_endline (Js.to_string (Js.Unsafe.coerce o)##._type);
  print_endline (Js.to_string (Js.Unsafe.coerce o)##.credentials);
  [%expect {|
    shared
    module
    same-origin
    |}]

let%expect_test "is_supported returns a bool" =
  let (_ : bool) = SharedWorker.is_supported () in
  print_endline "PASSED";
  [%expect {| PASSED |}]

let%expect_test "set_onconnect rejects a non-worker scope" =
  (* Outside a shared worker there is no [onconnect] global. *)
  (match SharedWorker.set_onconnect (fun _ -> ()) with
  | () -> print_endline "no exception"
  | exception Invalid_argument _ -> print_endline "Invalid_argument");
  [%expect {| Invalid_argument |}]
