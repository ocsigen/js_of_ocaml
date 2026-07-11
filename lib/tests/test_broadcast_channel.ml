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

(* [BroadcastChannel] is not available under QuickJS, so the whole suite is
   gated with [@when not quickjs]. *)

let%expect_test ("is_supported" [@when not quickjs]) =
  print_endline (if BroadcastChannel.is_supported () then "PASSED" else "FAILED");
  [%expect {| PASSED |}]

let%expect_test ("channel carries its name" [@when not quickjs]) =
  let chan = new%js BroadcastChannel.broadcastChannel (Js.string "jsoo-test") in
  let is_broadcast_channel =
    Js.instanceof chan (Js.Unsafe.global##._BroadcastChannel : _ Js.constr)
  in
  print_endline (string_of_bool is_broadcast_channel);
  print_endline (Js.to_string chan##.name);
  chan##close;
  [%expect {|
    true
    jsoo-test
    |}]

let%expect_test ("channels expose the messaging methods" [@when not quickjs]) =
  let chan = new%js BroadcastChannel.broadcastChannel (Js.string "jsoo-test") in
  (* [postMessage] must not raise on an open channel. *)
  chan##postMessage (Js.string "ping");
  chan##close;
  print_endline "PASSED";
  [%expect {| PASSED |}]
