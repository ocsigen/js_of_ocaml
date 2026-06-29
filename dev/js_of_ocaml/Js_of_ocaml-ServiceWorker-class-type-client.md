
# Class type `ServiceWorker.client`

A client (a `Window`, worker or shared worker) controlled by the worker.

```ocaml
method id : Js_of_ocaml__.Js.js_string Js_of_ocaml__.Js.t
              Js_of_ocaml__.Js.readonly_prop
```
```ocaml
method url : Js_of_ocaml__.Js.js_string Js_of_ocaml__.Js.t
               Js_of_ocaml__.Js.readonly_prop
```
```ocaml
method _type : Js_of_ocaml__.Js.js_string Js_of_ocaml__.Js.t
                 Js_of_ocaml__.Js.readonly_prop
```
One of `"window"`, `"worker"` or `"sharedworker"`.

```ocaml
method frameType : Js_of_ocaml__.Js.js_string Js_of_ocaml__.Js.t
                     Js_of_ocaml__.Js.readonly_prop
```
One of `"auxiliary"`, `"top-level"`, `"nested"` or `"none"`.

```ocaml
method postMessage : 'a. 'a -> unit Js_of_ocaml__.Js.meth
```
```ocaml
method postMessage_withTransfer : 'a. 'a ->
  Js_of_ocaml__.Js.Unsafe.any Js_of_ocaml__.Js.js_array Js_of_ocaml__.Js.t ->
  unit Js_of_ocaml__.Js.meth
```