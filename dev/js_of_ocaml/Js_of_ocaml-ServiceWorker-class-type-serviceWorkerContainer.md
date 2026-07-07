
# Class type `ServiceWorker.serviceWorkerContainer`

The page-facing entry point, exposed as `navigator.serviceWorker` (see [`container`](./Js_of_ocaml-ServiceWorker.md#val-container)). Inherits `EventTarget`.

```ocaml
inherit Js_of_ocaml__.Dom_html.eventTarget
```
```ocaml
method controller : serviceWorker Js_of_ocaml__.Js.t Js_of_ocaml__.Js.opt
                      Js_of_ocaml__.Js.readonly_prop
```
```ocaml
method ready : serviceWorkerRegistration Js_of_ocaml__.Js.t
                 Js_of_ocaml__.Promise.t
                 Js_of_ocaml__.Js.readonly_prop
```
```ocaml
method register : Js_of_ocaml__.Js.js_string Js_of_ocaml__.Js.t ->
  serviceWorkerRegistration Js_of_ocaml__.Js.t Js_of_ocaml__.Promise.t
    Js_of_ocaml__.Js.meth
```
```ocaml
method register_withOptions : Js_of_ocaml__.Js.js_string Js_of_ocaml__.Js.t ->
  registrationOptions Js_of_ocaml__.Js.t ->
  serviceWorkerRegistration Js_of_ocaml__.Js.t Js_of_ocaml__.Promise.t
    Js_of_ocaml__.Js.meth
```
```ocaml
method getRegistration : serviceWorkerRegistration Js_of_ocaml__.Js.t
                           Js_of_ocaml__.Js.optdef
                           Js_of_ocaml__.Promise.t
                           Js_of_ocaml__.Js.meth
```
```ocaml
method getRegistration_withUrl : Js_of_ocaml__.Js.js_string Js_of_ocaml__.Js.t ->
  serviceWorkerRegistration Js_of_ocaml__.Js.t Js_of_ocaml__.Js.optdef
    Js_of_ocaml__.Promise.t
    Js_of_ocaml__.Js.meth
```
```ocaml
method getRegistrations : serviceWorkerRegistration Js_of_ocaml__.Js.t
                            Js_of_ocaml__.Js.js_array
                            Js_of_ocaml__.Js.t
                            Js_of_ocaml__.Promise.t
                            Js_of_ocaml__.Js.meth
```
```ocaml
method startMessages : unit Js_of_ocaml__.Js.meth
```
```ocaml
method oncontrollerchange : ('self Js_of_ocaml__.Js.t,
                              'self Js_of_ocaml__.Dom.event Js_of_ocaml__.Js.t)
                              Js_of_ocaml__.Dom.event_listener
                              Js_of_ocaml__.Js.writeonly_prop
```
```ocaml
method onmessage : ('self Js_of_ocaml__.Js.t,
                     ('self, Js_of_ocaml__.Js.Unsafe.any)
                       Js_of_ocaml__.Dom_html.messageEvent
                       Js_of_ocaml__.Js.t)
                     Js_of_ocaml__.Dom.event_listener
                     Js_of_ocaml__.Js.writeonly_prop
```
```ocaml
method onmessageerror : ('self Js_of_ocaml__.Js.t,
                          ('self, Js_of_ocaml__.Js.Unsafe.any)
                            Js_of_ocaml__.Dom_html.messageEvent
                            Js_of_ocaml__.Js.t)
                          Js_of_ocaml__.Dom.event_listener
                          Js_of_ocaml__.Js.writeonly_prop
```