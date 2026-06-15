
# API reference

The Js\_of\_ocaml distribution is made of several packages. This page gathers the public modules of each; see the [documentation home](./index.md) for the manual.


## Js\_of\_ocaml — the base library

Core runtime and JavaScript values:

[`Js_of_ocaml.Js`](./Js_of_ocaml-Js.md) Javascript binding
[`Js_of_ocaml.Jstable`](./Js_of_ocaml-Jstable.md) A minimal table implementation specialized for Js.js\_string keys. This is faster than regular OCaml hashtables.
[`Js_of_ocaml.Sys_js`](./Js_of_ocaml-Sys_js.md) Javascript specific Sys functions.
[`Js_of_ocaml.Typed_array`](./Js_of_ocaml-Typed_array.md) Typed Array binding
Browser APIs:

[`Js_of_ocaml.CSS`](./Js_of_ocaml-CSS.md) This module contains a few types and values to ease the use of CSS properties and such. If you think a feature is missing, consider sending a patch or an RFE to the mailing list.
[`Js_of_ocaml.Dom`](./Js_of_ocaml-Dom.md) DOM binding
[`Js_of_ocaml.Dom_events`](./Js_of_ocaml-Dom_events.md) Javascript events
[`Js_of_ocaml.Dom_html`](./Js_of_ocaml-Dom_html.md) DOM HTML binding
[`Js_of_ocaml.Dom_svg`](./Js_of_ocaml-Dom_svg.md) DOM SVG binding
[`Js_of_ocaml.EventSource`](./Js_of_ocaml-EventSource.md) EventSource binding
[`Js_of_ocaml.File`](./Js_of_ocaml-File.md) File API
[`Js_of_ocaml.Firebug`](./Js_of_ocaml-Console.md) 
[`Js_of_ocaml.Form`](./Js_of_ocaml-Form.md) 
[`Js_of_ocaml.Geolocation`](./Js_of_ocaml-Geolocation.md) Geolocation API
[`Js_of_ocaml.Intl`](./Js_of_ocaml-Intl.md) Internationalization API
[`Js_of_ocaml.Json`](./Js_of_ocaml-Json.md) Unsafe IO. (See Deriving\_Json for typesafe IO)
[`Js_of_ocaml.MutationObserver`](./Js_of_ocaml-MutationObserver.md) MutationObserver API
[`Js_of_ocaml.Regexp`](./Js_of_ocaml-Regexp.md) Types for regexps.
[`Js_of_ocaml.Url`](./Js_of_ocaml-Url.md) This module provides functions for tampering with Url. It's main goal is to allow one to stay in the Ocaml realm without wandering into the Dom\_html.window\##.location object.
[`Js_of_ocaml.WebGL`](./Js_of_ocaml-WebGL.md) WebGL binding
[`Js_of_ocaml.WebSockets`](./Js_of_ocaml-WebSockets.md) WebSocket binding
[`Js_of_ocaml.Worker`](./Js_of_ocaml-Worker.md) Low-level bindgins to javascript Web Workers.
[`Js_of_ocaml.XmlHttpRequest`](./Js_of_ocaml-XmlHttpRequest.md) XmlHttpRequest object.

## Js\_of\_ocaml-lwt — Lwt support

- `Js_of_ocaml_lwt.Lwt_js` — sleeping and yielding
- `Js_of_ocaml_lwt.Lwt_js_events` — DOM events as Lwt threads
- `Js_of_ocaml_lwt.XmlHttpRequest` — XMLHttpRequest with Lwt
- `Js_of_ocaml_lwt.Jsonp` — JSONP requests
- `Js_of_ocaml_lwt.File` — reading files with Lwt

## Js\_of\_ocaml-ppx\_deriving\_json — JSON derivation

[`Deriving_Json`](./Deriving_Json.md) Typesafe IO (based on the deriving library).

## Js\_of\_ocaml-tyxml — TyXML support

- `Js_of_ocaml_tyxml.Tyxml_js` — build and manipulate DOM trees with TyXML
- `Js_of_ocaml_tyxml.Tyxml_cast` — cast between TyXML and Dom nodes
- `Js_of_ocaml_tyxml.Tyxml_cast_sigs` — signatures for the casts

## Js\_of\_ocaml-toplevel — toplevel and Dynlink

- `Js_of_ocaml_toplevel.JsooTop` — run an OCaml toplevel in the browser