
# Class type `ServiceWorker.serviceWorkerGlobalScope`

The global scope of a running service worker (its `self`). Inherits `EventTarget`.

```ocaml
inherit Js_of_ocaml__.Dom_html.eventTarget
```
```ocaml
method clients : clients Js_of_ocaml__.Js.t Js_of_ocaml__.Js.readonly_prop
```
```ocaml
method registration : serviceWorkerRegistration Js_of_ocaml__.Js.t
                        Js_of_ocaml__.Js.readonly_prop
```
```ocaml
method serviceWorker : serviceWorker Js_of_ocaml__.Js.t
                         Js_of_ocaml__.Js.readonly_prop
```
```ocaml
method skipWaiting : unit Js_of_ocaml__.Promise.t Js_of_ocaml__.Js.meth
```
```ocaml
method oninstall : ('self Js_of_ocaml__.Js.t,
                     extendableEvent Js_of_ocaml__.Js.t)
                     Js_of_ocaml__.Dom.event_listener
                     Js_of_ocaml__.Js.writeonly_prop
```
```ocaml
method onactivate : ('self Js_of_ocaml__.Js.t,
                      extendableEvent Js_of_ocaml__.Js.t)
                      Js_of_ocaml__.Dom.event_listener
                      Js_of_ocaml__.Js.writeonly_prop
```
```ocaml
method onfetch : ('self Js_of_ocaml__.Js.t, fetchEvent Js_of_ocaml__.Js.t)
                   Js_of_ocaml__.Dom.event_listener
                   Js_of_ocaml__.Js.writeonly_prop
```
```ocaml
method onmessage : ('self Js_of_ocaml__.Js.t,
                     Js_of_ocaml__.Js.Unsafe.any extendableMessageEvent
                       Js_of_ocaml__.Js.t)
                     Js_of_ocaml__.Dom.event_listener
                     Js_of_ocaml__.Js.writeonly_prop
```
```ocaml
method onmessageerror : ('self Js_of_ocaml__.Js.t,
                          Js_of_ocaml__.Js.Unsafe.any extendableMessageEvent
                            Js_of_ocaml__.Js.t)
                          Js_of_ocaml__.Dom.event_listener
                          Js_of_ocaml__.Js.writeonly_prop
```