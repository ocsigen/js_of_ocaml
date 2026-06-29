
# Class type `ServiceWorker.clients`

The worker's `clients` object, used to enumerate and message controlled clients.

```ocaml
method get : Js_of_ocaml__.Js.js_string Js_of_ocaml__.Js.t ->
  client Js_of_ocaml__.Js.t Js_of_ocaml__.Js.optdef Js_of_ocaml__.Promise.t
    Js_of_ocaml__.Js.meth
```
```ocaml
method matchAll : client Js_of_ocaml__.Js.t Js_of_ocaml__.Js.js_array
                    Js_of_ocaml__.Js.t
                    Js_of_ocaml__.Promise.t
                    Js_of_ocaml__.Js.meth
```
```ocaml
method matchAll_withOptions : clientsQueryOptions Js_of_ocaml__.Js.t ->
  client Js_of_ocaml__.Js.t Js_of_ocaml__.Js.js_array Js_of_ocaml__.Js.t
    Js_of_ocaml__.Promise.t
    Js_of_ocaml__.Js.meth
```
```ocaml
method openWindow : Js_of_ocaml__.Js.js_string Js_of_ocaml__.Js.t ->
  windowClient Js_of_ocaml__.Js.t Js_of_ocaml__.Js.opt Js_of_ocaml__.Promise.t
    Js_of_ocaml__.Js.meth
```
```ocaml
method claim : unit Js_of_ocaml__.Promise.t Js_of_ocaml__.Js.meth
```