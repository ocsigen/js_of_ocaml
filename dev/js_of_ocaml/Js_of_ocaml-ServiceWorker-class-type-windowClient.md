
# Class type `ServiceWorker.windowClient`

A [`client`](./Js_of_ocaml-ServiceWorker-class-type-client.md) that is a top-level browsing context (a window/tab).

```ocaml
inherit client
```
```ocaml
method focused : bool Js_of_ocaml__.Js.t Js_of_ocaml__.Js.readonly_prop
```
```ocaml
method visibilityState : Js_of_ocaml__.Js.js_string Js_of_ocaml__.Js.t
                           Js_of_ocaml__.Js.readonly_prop
```
```ocaml
method ancestorOrigins : Js_of_ocaml__.Js.js_string Js_of_ocaml__.Js.t
                           Js_of_ocaml__.Js.js_array
                           Js_of_ocaml__.Js.t
                           Js_of_ocaml__.Js.readonly_prop
```
```ocaml
method focus : windowClient Js_of_ocaml__.Js.t Js_of_ocaml__.Promise.t
                 Js_of_ocaml__.Js.meth
```
```ocaml
method navigate : Js_of_ocaml__.Js.js_string Js_of_ocaml__.Js.t ->
  windowClient Js_of_ocaml__.Js.t Js_of_ocaml__.Js.opt Js_of_ocaml__.Promise.t
    Js_of_ocaml__.Js.meth
```