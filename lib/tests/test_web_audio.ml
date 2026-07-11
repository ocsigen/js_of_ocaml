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

(* [AudioContext] is not available in the test engines, so the suite only
   exercises the parts that do not depend on the global being present:
   building the options record and the capability probe. *)

let%expect_test "options record can be populated" =
  let o = WebAudio.empty_audio_context_options () in
  o##.latencyHint := Js.string "playback";
  o##.sampleRate := Js.number_of_float 48000.;
  (* The fields are write-only; read them back through an unsafe coercion to
     check they were stored on the object. *)
  print_endline (Js.to_string (Js.Unsafe.coerce o)##.latencyHint);
  print_endline (string_of_float (Js.float_of_number (Js.Unsafe.coerce o)##.sampleRate));
  [%expect {|
    playback
    48000.
    |}]

let%expect_test "latencyHint_seconds writes the latencyHint field" =
  let o = WebAudio.empty_audio_context_options () in
  o##.latencyHint_seconds := Js.number_of_float 0.05;
  print_endline (string_of_float (Js.float_of_number (Js.Unsafe.coerce o)##.latencyHint));
  [%expect {| 0.05 |}]

let%expect_test "is_supported returns a bool" =
  let (_ : bool) = WebAudio.is_supported () in
  print_endline "PASSED";
  [%expect {| PASSED |}]
