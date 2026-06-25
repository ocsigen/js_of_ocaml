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

(* [MessageChannel]/[MessagePort]/[MessageEvent] are not available under
   QuickJS, so the whole suite is gated with [@when not quickjs]. *)

let is_message_port any =
  Js.instanceof
    (Js.Unsafe.coerce any : _ Js.t)
    (Js.Unsafe.global##._MessagePort : _ Js.constr)

let%expect_test ("is_supported" [@when not quickjs]) =
  print_endline (if MessageChannel.is_supported () then "PASSED" else "FAILED");
  [%expect {| PASSED |}]

let%expect_test ("channel exposes two distinct MessagePorts" [@when not quickjs]) =
  let chan = new%js MessageChannel.messageChannel in
  print_endline (string_of_bool (is_message_port chan##.port1));
  print_endline (string_of_bool (is_message_port chan##.port2));
  print_endline (string_of_bool (chan##.port1 != chan##.port2));
  [%expect {|
    true
    true
    true
    |}]

let%expect_test ("ports expose the messaging methods" [@when not quickjs]) =
  let chan = new%js MessageChannel.messageChannel in
  let port = chan##.port1 in
  (* [start] then [postMessage] must not raise on an open port. *)
  port##start;
  port##postMessage (Js.string "ping");
  port##close;
  print_endline "PASSED";
  [%expect {| PASSED |}]

let%expect_test ("MessageEvent constructor carries its data" [@when not quickjs]) =
  let init = MessageChannel.empty_message_event_init () in
  init##.data := Js.Unsafe.inject (Js.string "payload");
  init##.origin := Js.string "https://example.invalid";
  init##.lastEventId := Js.string "42";
  let ev = new%js MessageChannel.messageEvent_with_init (Js.string "message") init in
  print_endline (Js.to_string ev##._type);
  print_endline (Js.to_string (Js.Unsafe.coerce ev##.data));
  print_endline (Js.to_string ev##.origin);
  print_endline (Js.to_string ev##.lastEventId);
  print_endline (string_of_int ev##.ports##.length);
  [%expect {|
    message
    payload
    https://example.invalid
    42
    0
    |}]
