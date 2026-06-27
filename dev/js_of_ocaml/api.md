
# API reference

The Js\_of\_ocaml distribution ships several opam packages, each providing one or more libraries. This page lists the public modules of every package; see the [documentation home](./index.md) for the manual.

The top-level module of each library also states, in its own documentation, which opam package provides it.


## `js_of_ocaml` — the base library

Provided by the `js_of_ocaml` opam package (library `js_of_ocaml`).

Core runtime and JavaScript values:

[`Js_of_ocaml.Js`](./Js_of_ocaml-Js.md) Javascript binding
[`Js_of_ocaml.Js_error`](./Js_of_ocaml-Js-Js_error.md) 
[`Js_of_ocaml.Jstable`](./Js_of_ocaml-Jstable.md) A minimal table implementation specialized for Js.js\_string keys. This is faster than regular OCaml hashtables.
[`Js_of_ocaml.Sys_js`](./Js_of_ocaml-Sys_js.md) Javascript specific Sys functions.
[`Js_of_ocaml.Typed_array`](./Js_of_ocaml-Typed_array.md) Typed Array binding
[`Js_of_ocaml.Effect_js`](./Js_of_ocaml-Effect_js.md) Javascript-specific effect functions.
Browser APIs:

[`Js_of_ocaml.Abort`](./Js_of_ocaml-Abort.md) AbortController / AbortSignal.
[`Js_of_ocaml.CSS`](./Js_of_ocaml-CSS.md) This module contains a few types and values to ease the use of CSS properties and such. If you think a feature is missing, consider sending a patch or an RFE to the mailing list.
[`Js_of_ocaml.Clipboard`](./Js_of_ocaml-Clipboard.md) Clipboard API.
[`Js_of_ocaml.Console`](./Js_of_ocaml-Console.md) Console API (debugging console).
[`Js_of_ocaml.Crypto`](./Js_of_ocaml-Crypto.md) Web Crypto API.
[`Js_of_ocaml.Dom`](./Js_of_ocaml-Dom.md) DOM binding
[`Js_of_ocaml.Dom_events`](./Js_of_ocaml-Dom_events.md) Javascript events
[`Js_of_ocaml.Dom_html`](./Js_of_ocaml-Dom_html.md) DOM HTML binding
[`Js_of_ocaml.Dom_svg`](./Js_of_ocaml-Dom_svg.md) DOM SVG binding
[`Js_of_ocaml.EventSource`](./Js_of_ocaml-EventSource.md) EventSource binding
[`Js_of_ocaml.Fetch`](./Js_of_ocaml-Fetch.md) Fetch API.
[`Js_of_ocaml.File`](./Js_of_ocaml-File.md) File API
[`Js_of_ocaml.Form`](./Js_of_ocaml-Form.md) 
[`Js_of_ocaml.Geolocation`](./Js_of_ocaml-Geolocation.md) Geolocation API
[`Js_of_ocaml.IntersectionObserver`](./Js_of_ocaml-IntersectionObserver.md) The Intersection Observer API provides a way to asynchronously observe changes in the intersection of a target element with an ancestor element or with a top-level document's viewport.
[`Js_of_ocaml.Intl`](./Js_of_ocaml-Intl.md) Internationalization API
[`Js_of_ocaml.Json`](./Js_of_ocaml-Json.md) Unsafe IO. (See Deriving\_Json for typesafe IO)
[`Js_of_ocaml.MutationObserver`](./Js_of_ocaml-MutationObserver.md) MutationObserver API
[`Js_of_ocaml.Notification`](./Js_of_ocaml-Notification.md) Notifications API.
[`Js_of_ocaml.Performance`](./Js_of_ocaml-Performance.md) Performance API
[`Js_of_ocaml.PerformanceObserver`](./Js_of_ocaml-PerformanceObserver.md) PerformanceObserver API
[`Js_of_ocaml.Promise`](./Js_of_ocaml-Promise.md) Bindings to the JavaScript Promise API.
[`Js_of_ocaml.Regexp`](./Js_of_ocaml-Regexp.md) Types for regexps.
[`Js_of_ocaml.ResizeObserver`](./Js_of_ocaml-ResizeObserver.md) ResizeObserver API
[`Js_of_ocaml.Url`](./Js_of_ocaml-Url.md) This module provides functions for tampering with Url. It's main goal is to allow one to stay in the Ocaml realm without wandering into the Dom\_html.window\##.location object.
[`Js_of_ocaml.WebGL`](./Js_of_ocaml-WebGL.md) WebGL binding
[`Js_of_ocaml.WebSockets`](./Js_of_ocaml-WebSockets.md) WebSocket binding
[`Js_of_ocaml.Worker`](./Js_of_ocaml-Worker.md) Low-level bindgins to javascript Web Workers.
[`Js_of_ocaml.XmlHttpRequest`](./Js_of_ocaml-XmlHttpRequest.md) XmlHttpRequest object.
The same opam package also provides the `js_of_ocaml.deriving` library — the runtime used by the code that `[@@deriving json]` generates:

[`Deriving_Json`](./Deriving_Json.md) Typesafe IO (based on the deriving library).
[`Deriving_Json_lexer`](./Deriving_Json_lexer.md) 

## `js_of_ocaml-lwt` — Lwt support

Provided by the `js_of_ocaml-lwt` opam package (library `js_of_ocaml-lwt`).

- `Js_of_ocaml_lwt.Lwt_js` — sleeping and yielding
- `Js_of_ocaml_lwt.Lwt_js_events` — DOM events as Lwt threads
- `Js_of_ocaml_lwt.XmlHttpRequest` — XMLHttpRequest with Lwt
- `Js_of_ocaml_lwt.Jsonp` — JSONP requests
- `Js_of_ocaml_lwt.File` — reading files with Lwt
- `Js_of_ocaml_lwt.Promise` — bridge between Lwt threads and JS promises

## `js_of_ocaml-ppx_deriving_json` — JSON derivation syntax

Provided by the `js_of_ocaml-ppx_deriving_json` opam package. This is the PPX that derives JSON serializers (`[@@deriving json]`); the generated code relies on the [`Deriving_Json`](./Deriving_Json.md) runtime from the `js_of_ocaml` package above. See [the manual](./ppx-deriving.md) for usage.


## `js_of_ocaml-tyxml` — TyXML support

Provided by the `js_of_ocaml-tyxml` opam package (library `js_of_ocaml-tyxml`).

- `Js_of_ocaml_tyxml.Tyxml_js` — build and manipulate DOM trees with TyXML
- `Js_of_ocaml_tyxml.Tyxml_cast` — cast between TyXML and Dom nodes
- `Js_of_ocaml_tyxml.Tyxml_cast_sigs` — signatures for the casts

## `js_of_ocaml-toplevel` — toplevel and Dynlink

Provided by the `js_of_ocaml-toplevel` opam package (library `js_of_ocaml-toplevel`).

- `Js_of_ocaml_toplevel.JsooTop` — run an OCaml toplevel in the browser