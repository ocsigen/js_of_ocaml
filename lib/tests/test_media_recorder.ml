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

(* [MediaRecorder] is browser-only, so the suite only exercises the parts
   that do not depend on the global being present: building the options
   record and the capability probes. *)

let%expect_test "options record can be populated" =
  let o = MediaRecorder.empty_recorder_options () in
  o##.mimeType := Js.string "audio/webm";
  o##.audioBitsPerSecond := 128000;
  (* The fields are write-only; read them back through an unsafe coercion to
     check they were stored on the object. *)
  print_endline (Js.to_string (Js.Unsafe.coerce o)##.mimeType);
  print_endline (string_of_int (Js.Unsafe.coerce o)##.audioBitsPerSecond);
  [%expect {|
    audio/webm
    128000
    |}]

let%expect_test "probes return without throwing" =
  let (_ : bool) = MediaRecorder.is_supported () in
  (* Must be [false] rather than a crash when the global is missing. *)
  print_endline (string_of_bool (MediaRecorder.is_type_supported "audio/webm"));
  [%expect {| false |}]
