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

(* [getUserMedia] and friends are browser-only, so the suite only exercises
   the parts that do not depend on the globals being present: building the
   constraint records and the capability probe. *)

let%expect_test "track constraints record can be populated" =
  let c = MediaCapture.empty_track_constraints () in
  c##.facingMode := Js.string "user";
  c##.width := 1280;
  c##.echoCancellation := Js._true;
  (* The fields are write-only; read them back through an unsafe coercion to
     check they were stored on the object. *)
  print_endline (Js.to_string (Js.Unsafe.coerce c)##.facingMode);
  print_endline (string_of_int (Js.Unsafe.coerce c)##.width);
  print_endline (string_of_bool (Js.to_bool (Js.Unsafe.coerce c)##.echoCancellation));
  [%expect {|
    user
    1280
    true
    |}]

let%expect_test "stream constraints: plain and per-track forms share a field" =
  let c = MediaCapture.empty_stream_constraints () in
  c##.audio := Js._true;
  let tc = MediaCapture.empty_track_constraints () in
  tc##.facingMode := Js.string "environment";
  (* [video_constr] must write the underlying [video] field. *)
  c##.video_constr := tc;
  print_endline (string_of_bool (Js.to_bool (Js.Unsafe.coerce c)##.audio));
  print_endline (Js.to_string (Js.Unsafe.coerce c)##.video##.facingMode);
  [%expect {|
    true
    environment
    |}]

let%expect_test "is_supported returns a bool" =
  let (_ : bool) = MediaCapture.is_supported () in
  print_endline "PASSED";
  [%expect {| PASSED |}]
