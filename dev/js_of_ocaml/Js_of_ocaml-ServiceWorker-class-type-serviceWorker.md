
# Class type `ServiceWorker.serviceWorker`

A service worker, as seen from a controlled page. Inherits `EventTarget`.

```ocaml
inherit Js_of_ocaml__.Dom_html.eventTarget
```
```ocaml
method scriptURL : Js_of_ocaml__.Js.js_string Js_of_ocaml__.Js.t
                     Js_of_ocaml__.Js.readonly_prop
```
```ocaml
method state : Js_of_ocaml__.Js.js_string Js_of_ocaml__.Js.t
                 Js_of_ocaml__.Js.readonly_prop
```
One of `"parsed"`, `"installing"`, `"installed"`, `"activating"`, `"activated"` or `"redundant"`.

```ocaml
method postMessage : 'a. 'a -> unit Js_of_ocaml__.Js.meth
```
```ocaml
method postMessage_withTransfer : 'a. 'a ->
  Js_of_ocaml__.Js.Unsafe.any Js_of_ocaml__.Js.js_array Js_of_ocaml__.Js.t ->
  unit Js_of_ocaml__.Js.meth
```
```ocaml
method onstatechange : ('self Js_of_ocaml__.Js.t,
                         'self Js_of_ocaml__.Dom.event Js_of_ocaml__.Js.t)
                         Js_of_ocaml__.Dom.event_listener
                         Js_of_ocaml__.Js.writeonly_prop
```