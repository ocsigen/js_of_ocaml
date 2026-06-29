
# Class type `ServiceWorker.fetchEvent`

The event passed to the worker's `fetch` handler.

```ocaml
inherit extendableEvent
```
```ocaml
method request : Js_of_ocaml__.Fetch.request Js_of_ocaml__.Js.t
                   Js_of_ocaml__.Js.readonly_prop
```
```ocaml
method clientId : Js_of_ocaml__.Js.js_string Js_of_ocaml__.Js.t
                    Js_of_ocaml__.Js.readonly_prop
```
```ocaml
method resultingClientId : Js_of_ocaml__.Js.js_string Js_of_ocaml__.Js.t
                             Js_of_ocaml__.Js.readonly_prop
```
```ocaml
method replacesClientId : Js_of_ocaml__.Js.js_string Js_of_ocaml__.Js.t
                            Js_of_ocaml__.Js.readonly_prop
```
```ocaml
method preloadResponse : Js_of_ocaml__.Js.Unsafe.any Js_of_ocaml__.Promise.t
                           Js_of_ocaml__.Js.readonly_prop
```
```ocaml
method handled : unit Js_of_ocaml__.Promise.t Js_of_ocaml__.Js.readonly_prop
```
Resolves once the request has been handled (whether or not `respondWith` was called).

```ocaml
method respondWith : Js_of_ocaml__.Fetch.response Js_of_ocaml__.Js.t
                       Js_of_ocaml__.Promise.t ->
  unit Js_of_ocaml__.Js.meth
```
`event##respondWith p` provides the response (as a promise) for the intercepted request.
