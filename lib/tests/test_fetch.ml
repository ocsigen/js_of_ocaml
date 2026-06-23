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

let is_promise any =
  Js.instanceof
    (Js.Unsafe.coerce any : _ Js.t)
    (Js.Unsafe.global##._Promise : _ Js.constr)

let get_or_none o = Js.Opt.case o (fun () -> "<none>") Js.to_string

(* The fetch API (including the support probe) is not available under QuickJS,
   so the whole suite is gated with [@when not quickjs]. *)

let%expect_test ("is_supported" [@when not quickjs]) =
  print_endline (if Fetch.is_supported () then "PASSED" else "FAILED");
  [%expect {| PASSED |}]

(* {1 Headers} *)

let%expect_test ("headers constructor (no arg) starts empty" [@when not quickjs]) =
  let h = new%js Fetch.headers in
  print_endline (string_of_bool (Js.to_bool (h##has (Js.string "x"))));
  h##append (Js.string "x") (Js.string "1");
  print_endline (get_or_none (h##get (Js.string "x")));
  [%expect {|
    false
    1
    |}]

let%expect_test ("headers_of_list round-trips pairs" [@when not quickjs]) =
  let h = Fetch.headers_of_list [ "content-type", "text/plain"; "x-trace", "abc" ] in
  print_endline (get_or_none (h##get (Js.string "content-type")));
  print_endline (get_or_none (h##get (Js.string "x-trace")));
  print_endline (string_of_bool (Js.to_bool (h##has (Js.string "x-missing"))));
  [%expect {|
    text/plain
    abc
    false
    |}]

let%expect_test ("headers append/set/delete and forEach" [@when not quickjs]) =
  let h = Fetch.headers_of_list [ "x-a", "1" ] in
  h##set (Js.string "x-a") (Js.string "2");
  print_endline (get_or_none (h##get (Js.string "x-a")));
  h##append (Js.string "x-a") (Js.string "3");
  print_endline (get_or_none (h##get (Js.string "x-a")));
  h##delete (Js.string "x-a");
  print_endline (string_of_bool (Js.to_bool (h##has (Js.string "x-a"))));
  let h = Fetch.headers_of_list [ "x-a", "1"; "x-b", "2" ] in
  let names = ref [] in
  h##forEach (Js.wrap_callback (fun _v k _ -> names := Js.to_string k :: !names));
  print_endline (String.concat ", " (List.sort compare !names));
  [%expect {|
    2
    2, 3
    false
    x-a, x-b
    |}]

(* {1 Request} *)

let%expect_test ("request constructor (no init) defaults to GET" [@when not quickjs]) =
  let req = new%js Fetch.request (Js.string "https://example.invalid/x") in
  print_endline (Js.to_string req##.url);
  print_endline (Js.to_string req##._method);
  [%expect {|
    https://example.invalid/x
    GET
    |}]

let%expect_test ("requestInit fields round-trip through Request" [@when not quickjs]) =
  let init = Fetch.empty_request_init () in
  init##._method := Js.string "POST";
  init##.body := Js.Unsafe.inject (Js.string "hello");
  init##.headers := Fetch.headers_of_list [ "x-trace", "abc" ];
  init##.mode := Js.string "cors";
  init##.credentials := Js.string "include";
  init##.cache := Js.string "no-store";
  init##.redirect := Js.string "manual";
  init##.referrer := Js.string "";
  init##.referrerPolicy := Js.string "no-referrer";
  init##.integrity := Js.string "";
  init##.keepalive := Js._true;
  let controller = new%js Abort.controller in
  init##.signal := controller##.signal;
  let req = new%js Fetch.request_with_init (Js.string "https://example.invalid/x") init in
  print_endline (Js.to_string req##._method);
  print_endline (Js.to_string req##.mode);
  print_endline (Js.to_string req##.credentials);
  print_endline (Js.to_string req##.cache);
  print_endline (Js.to_string req##.redirect);
  print_endline (Js.to_string req##.referrer);
  print_endline (Js.to_string req##.referrerPolicy);
  print_endline (string_of_bool (Js.to_bool req##.keepalive));
  print_endline (get_or_none (req##.headers##get (Js.string "x-trace")));
  print_endline (string_of_bool (Js.to_bool req##.signal##.aborted));
  [%expect
    {|
    POST
    cors
    include
    no-store
    manual

    no-referrer
    true
    abc
    false
    |}]

let%expect_test ("request other readonly fields are accessible" [@when not quickjs]) =
  let req = new%js Fetch.request (Js.string "https://example.invalid/x") in
  (* [destination] is the empty string for a Request not driven by a
     resource fetch (e.g. a non-script-initiated subresource). *)
  print_endline ("destination=" ^ Js.to_string req##.destination);
  print_endline ("integrity=" ^ Js.to_string req##.integrity);
  print_endline ("bodyUsed=" ^ string_of_bool (Js.to_bool req##.bodyUsed));
  print_endline
    ("signal is AbortSignal: "
    ^ string_of_bool
        (Js.instanceof req##.signal (Js.Unsafe.global##._AbortSignal : _ Js.constr)));
  [%expect
    {|
    destination=
    integrity=
    bodyUsed=false
    signal is AbortSignal: true
    |}]

let%expect_test ("request.clone returns a distinct Request" [@when not quickjs]) =
  let req = new%js Fetch.request (Js.string "https://example.invalid/x") in
  let cloned = req##clone in
  print_endline (Js.to_string cloned##.url);
  print_endline (string_of_bool (req != cloned));
  [%expect {|
    https://example.invalid/x
    true
    |}]

(* {1 Response} *)

let%expect_test ("response constructor exposes defaults" [@when not quickjs]) =
  let resp = new%js Fetch.response (Js.Unsafe.inject (Js.string "hello")) in
  print_endline ("status=" ^ string_of_int resp##.status);
  print_endline ("ok=" ^ string_of_bool (Js.to_bool resp##.ok));
  print_endline ("statusText=" ^ Js.to_string resp##.statusText);
  print_endline ("type=" ^ Js.to_string resp##._type);
  print_endline ("redirected=" ^ string_of_bool (Js.to_bool resp##.redirected));
  print_endline ("url=" ^ Js.to_string resp##.url);
  print_endline ("bodyUsed=" ^ string_of_bool (Js.to_bool resp##.bodyUsed));
  [%expect
    {|
    status=200
    ok=true
    statusText=
    type=default
    redirected=false
    url=
    bodyUsed=false
    |}]

let%expect_test
    ("response_with_init applies status/statusText/headers" [@when not quickjs]) =
  let init = Fetch.empty_response_init () in
  init##.status := 201;
  init##.statusText := Js.string "Created";
  init##.headers := Fetch.headers_of_list [ "x-from-init", "v" ];
  let resp = new%js Fetch.response_with_init (Js.Unsafe.inject (Js.string "hi")) init in
  print_endline ("status=" ^ string_of_int resp##.status);
  print_endline ("ok=" ^ string_of_bool (Js.to_bool resp##.ok));
  print_endline ("statusText=" ^ Js.to_string resp##.statusText);
  print_endline
    ("x-from-init=" ^ get_or_none (resp##.headers##get (Js.string "x-from-init")));
  [%expect {|
    status=201
    ok=true
    statusText=Created
    x-from-init=v
    |}]

let%expect_test ("response status outside 2xx flips ok" [@when not quickjs]) =
  let init = Fetch.empty_response_init () in
  init##.status := 404;
  let resp = new%js Fetch.response_with_init (Js.Unsafe.inject (Js.string "")) init in
  print_endline (string_of_int resp##.status);
  print_endline (string_of_bool (Js.to_bool resp##.ok));
  [%expect {|
    404
    false
    |}]

let%expect_test ("response.clone returns a distinct Response" [@when not quickjs]) =
  let resp = new%js Fetch.response (Js.Unsafe.inject (Js.string "hi")) in
  let cloned = resp##clone in
  print_endline (string_of_int cloned##.status);
  print_endline (string_of_bool (resp != cloned));
  [%expect {|
    200
    true
    |}]

(* {1 Body methods (sync shape — async behavior covered in tests-browser)} *)

let%expect_test ("all body methods return Promise instances" [@when not quickjs]) =
  let resp = new%js Fetch.response (Js.Unsafe.inject (Js.string "hello")) in
  print_endline (string_of_bool (is_promise (Promise.to_any resp##text)));
  let resp = new%js Fetch.response (Js.Unsafe.inject (Js.string "hello")) in
  print_endline (string_of_bool (is_promise (Promise.to_any resp##arrayBuffer)));
  let resp = new%js Fetch.response (Js.Unsafe.inject (Js.string "hello")) in
  print_endline (string_of_bool (is_promise (Promise.to_any resp##blob)));
  let resp = new%js Fetch.response (Js.Unsafe.inject (Js.string "null")) in
  print_endline (string_of_bool (is_promise (Promise.to_any resp##json)));
  [%expect {|
    true
    true
    true
    true
    |}]

(* [formData] is not universally implemented on synthetic Responses, so
   we only assert that the binding exists and returns *something* (which
   will be a [Promise] in all conforming environments). *)
let%expect_test ("body.formData is callable" [@when not quickjs]) =
  let resp = new%js Fetch.response (Js.Unsafe.inject (Js.string "")) in
  let p = Promise.to_any resp##formData in
  print_endline (string_of_bool (is_promise p));
  [%expect {| true |}]
