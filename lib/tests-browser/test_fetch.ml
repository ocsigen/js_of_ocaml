open Js_of_ocaml

let pass_count = ref 0

let fail_count = ref 0

let log_row status label detail =
  let el = Dom_html.document##getElementById (Js.string "log") in
  Js.Opt.iter el (fun el ->
      let row = Dom_html.document##createElement (Js.string "div") in
      row##.className := Js.string ("row " ^ status);
      let mark = Dom_html.document##createElement (Js.string "span") in
      mark##.className := Js.string ("mark " ^ status);
      mark##.textContent :=
        Js.some
          (Js.string
             (if String.equal status "pass" then "\xe2\x9c\x93" else "\xe2\x9c\x97"));
      let txt = Dom_html.document##createElement (Js.string "span") in
      txt##.className := Js.string "label";
      txt##.textContent := Js.some (Js.string label);
      let det = Dom_html.document##createElement (Js.string "span") in
      det##.className := Js.string "detail";
      det##.textContent := Js.some (Js.string detail);
      Dom.appendChild row mark;
      Dom.appendChild row txt;
      Dom.appendChild row det;
      Dom.appendChild el row)

let check label cond detail =
  if cond
  then (
    incr pass_count;
    log_row "pass" label detail)
  else (
    incr fail_count;
    log_row "fail" label detail)

let summarize () =
  let el = Dom_html.document##getElementById (Js.string "summary") in
  Js.Opt.iter el (fun el ->
      let status = if !fail_count = 0 then "pass" else "fail" in
      el##.className := Js.string ("summary " ^ status);
      el##.textContent :=
        Js.some
          (Js.string (Printf.sprintf "%d passed, %d failed" !pass_count !fail_count)))

let ( >>= ) p f = Promise.then_ f p

let return = Promise.resolve

let stringify any = Js.to_string (Js.Unsafe.fun_call Js.Unsafe.global##._String [| any |])

(* Wrap any value in a [Response] so we can exercise [body.*] methods. *)
let response_of_string s : Fetch.response Js.t =
  new%js Fetch.response (Js.Unsafe.inject (Js.string s))

let starts_with s ~prefix =
  String.length s >= String.length prefix
  && String.sub s 0 (String.length prefix) = prefix

let test_supported_and_self_fetch self_url =
  check "Fetch.is_supported ()" (Fetch.is_supported ()) "fetch global available";
  Fetch.fetch self_url
  >>= fun resp ->
  check
    "fetch self -> 200 OK"
    (resp##.status = 200 && Js.to_bool resp##.ok)
    (Printf.sprintf "status=%d ok=%b" resp##.status (Js.to_bool resp##.ok));
  let ct =
    Js.Opt.case
      (resp##.headers##get (Js.string "content-type"))
      (fun () -> "")
      Js.to_string
  in
  check
    "response content-type is text/html"
    (starts_with ct ~prefix:"text/html")
    (Printf.sprintf "content-type=%s" ct);
  check "response.url is set" (Js.to_string resp##.url <> "") (Js.to_string resp##.url);
  check
    "response._type is basic for same-origin"
    (Js.to_string resp##._type = "basic")
    (Js.to_string resp##._type);
  check
    "response.redirected is false"
    (not (Js.to_bool resp##.redirected))
    (string_of_bool (Js.to_bool resp##.redirected));
  check
    "bodyUsed false before read"
    (not (Js.to_bool resp##.bodyUsed))
    (string_of_bool (Js.to_bool resp##.bodyUsed));
  resp##text
  >>= fun body ->
  let n = String.length (Js.to_string body) in
  check "response.text resolves with body" (n > 0) (Printf.sprintf "body length=%d" n);
  check
    "bodyUsed true after read"
    (Js.to_bool resp##.bodyUsed)
    (string_of_bool (Js.to_bool resp##.bodyUsed));
  return ()

let test_fetch_with_init self_url =
  let init = Fetch.empty_request_init () in
  init##.headers := Fetch.headers_of_list [ "x-trace", "browser-test" ];
  Fetch.fetch_with_init self_url init
  >>= fun resp ->
  check
    "fetch_with_init -> 200"
    (resp##.status = 200)
    (Printf.sprintf "status=%d" resp##.status);
  return ()

let test_fetch_request self_url =
  let init = Fetch.empty_request_init () in
  init##._method := Js.string "GET";
  let req = new%js Fetch.request_with_init self_url init in
  check
    "Request.url round-trips"
    (Js.to_string req##.url = Js.to_string self_url)
    (Js.to_string req##.url);
  check
    "Request.method round-trips"
    (Js.to_string req##._method = "GET")
    (Js.to_string req##._method);
  let cloned = req##clone in
  check
    "Request.clone is a distinct object with same URL"
    (req != cloned && Js.to_string cloned##.url = Js.to_string self_url)
    (Js.to_string cloned##.url);
  Fetch.fetch_request req
  >>= fun resp ->
  check
    "fetch_request -> 200"
    (resp##.status = 200)
    (Printf.sprintf "status=%d" resp##.status);
  return ()

let test_text () =
  let resp = response_of_string "hello world" in
  resp##text
  >>= fun s ->
  check
    "Response#text resolves with the body"
    (Js.to_string s = "hello world")
    (Js.to_string s);
  return ()

let test_json () =
  let resp = response_of_string {|{"a":1,"b":"two"}|} in
  resp##json
  >>= fun any ->
  let obj : < a : int Js.readonly_prop ; b : Js.js_string Js.t Js.readonly_prop > Js.t =
    Js.Unsafe.coerce any
  in
  check
    "Response#json parses JSON"
    (obj##.a = 1 && Js.to_string obj##.b = "two")
    (Printf.sprintf "{a=%d; b=%s}" obj##.a (Js.to_string obj##.b));
  return ()

let test_array_buffer () =
  let resp = response_of_string "abc" in
  resp##arrayBuffer
  >>= fun buf ->
  let n = buf##.byteLength in
  check
    "Response#arrayBuffer byteLength matches"
    (n = 3)
    (Printf.sprintf "byteLength=%d" n);
  return ()

let test_blob () =
  let resp = response_of_string "abcdef" in
  resp##blob
  >>= fun blob ->
  let size : int = (Js.Unsafe.coerce blob)##.size in
  check "Response#blob.size matches" (size = 6) (Printf.sprintf "size=%d" size);
  return ()

let test_clone_double_read () =
  let resp = response_of_string "twice" in
  let twin = resp##clone in
  resp##text
  >>= fun a ->
  twin##text
  >>= fun b ->
  check
    "Response#clone allows reading body twice"
    (Js.to_string a = "twice" && Js.to_string b = "twice")
    (Printf.sprintf "orig=%s clone=%s" (Js.to_string a) (Js.to_string b));
  return ()

let test_rejection_invalid_host () =
  let bogus = Js.string "http://nonexistent.invalid./" in
  Promise.catch
    (fun e ->
      let reason = stringify (Promise.error_to_any e) in
      check
        "fetch invalid host rejects with TypeError"
        (starts_with reason ~prefix:"TypeError")
        reason;
      return ())
    (Fetch.fetch bogus
    >>= fun resp ->
    check
      "fetch invalid host rejects with TypeError"
      false
      (Printf.sprintf "unexpectedly resolved status=%d" resp##.status);
    return ())

let test_abort_signal self_url =
  let controller = new%js Abort.controller in
  let init = Fetch.empty_request_init () in
  init##.signal := controller##.signal;
  let p = Fetch.fetch_with_init self_url init in
  controller##abort;
  check
    "AbortSignal.aborted true after controller.abort"
    (Js.to_bool controller##.signal##.aborted)
    "";
  Promise.catch
    (fun e ->
      let reason = stringify (Promise.error_to_any e) in
      check
        "aborted fetch rejects"
        (starts_with reason ~prefix:"AbortError"
        || starts_with reason ~prefix:"DOMException"
        || starts_with reason ~prefix:"Error")
        reason;
      return ())
    (p
    >>= fun resp ->
    check
      "aborted fetch rejects"
      false
      (Printf.sprintf "unexpectedly resolved status=%d" resp##.status);
    return ())

let run () =
  let self_url = Dom_html.window##.location##.href in
  let _ : unit Promise.t =
    test_supported_and_self_fetch self_url
    >>= (fun () -> test_fetch_with_init self_url)
    >>= (fun () -> test_fetch_request self_url)
    >>= test_text
    >>= test_json
    >>= test_array_buffer
    >>= test_blob
    >>= test_clone_double_read
    >>= test_rejection_invalid_host
    >>= (fun () -> test_abort_signal self_url)
    >>= fun () ->
    summarize ();
    return ()
  in
  ()

let () = run ()
