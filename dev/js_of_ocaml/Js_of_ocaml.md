
# Module `Js_of_ocaml`

```ocaml
module Abort : sig ... end
```
AbortController / AbortSignal.

```ocaml
module CSS : sig ... end
```
This module contains a few types and values to ease the use of CSS properties and such. If you think a feature is missing, consider sending a patch or an RFE to the mailing list.

```ocaml
module Console : sig ... end
```
Console API (debugging console).

```ocaml
module Dom : sig ... end
```
DOM binding

```ocaml
module Dom_events : sig ... end
```
Javascript events

```ocaml
module Dom_html : sig ... end
```
DOM HTML binding

```ocaml
module Dom_svg : sig ... end
```
DOM SVG binding

```ocaml
module Effect_js : sig ... end
```
Javascript-specific effect functions.

```ocaml
module EventSource : sig ... end
```
EventSource binding

```ocaml
module Fetch : sig ... end
```
Fetch API.

```ocaml
module File : sig ... end
```
File API

```ocaml
module Firebug = Console
```
```ocaml
module Form : sig ... end
```
```ocaml
module Geolocation : sig ... end
```
Geolocation API

```ocaml
module IntersectionObserver : sig ... end
```
The Intersection Observer API provides a way to asynchronously observe changes in the intersection of a target element with an ancestor element or with a top-level document's viewport.

```ocaml
module Intl : sig ... end
```
Internationalization API

```ocaml
module Js : sig ... end
```
Javascript binding

```ocaml
module Js_error = Js.Js_error
```
```ocaml
module Json : sig ... end
```
Unsafe IO. (See [`Deriving_Json`](./Deriving_Json.md) for typesafe IO)

```ocaml
module Jstable : sig ... end
```
A minimal table implementation specialized for [`Js.js_string`](./Js_of_ocaml-Js-class-type-js_string.md) keys. This is faster than regular OCaml hashtables.

```ocaml
module MutationObserver : sig ... end
```
MutationObserver API

```ocaml
module Performance : sig ... end
```
Performance API

```ocaml
module PerformanceObserver : sig ... end
```
PerformanceObserver API

```ocaml
module Promise : sig ... end
```
Bindings to the JavaScript `Promise` API.

```ocaml
module ResizeObserver : sig ... end
```
ResizeObserver API

```ocaml
module Regexp : sig ... end
```
Types for regexps.

```ocaml
module Sys_js : sig ... end
```
Javascript specific Sys functions.

```ocaml
module Typed_array : sig ... end
```
Typed Array binding

```ocaml
module Url : sig ... end
```
This module provides functions for tampering with Url. It's main goal is to allow one to stay in the Ocaml realm without wandering into the [`Dom_html.window`](./Js_of_ocaml-Dom_html-class-type-window.md)\##.location object.

```ocaml
module WebGL : sig ... end
```
WebGL binding

```ocaml
module WebSockets : sig ... end
```
WebSocket binding

```ocaml
module Worker : sig ... end
```
Low-level bindgins to javascript Web Workers.

```ocaml
module XmlHttpRequest : sig ... end
```
XmlHttpRequest object.
