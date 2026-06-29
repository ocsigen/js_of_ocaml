
# Class type `ServiceWorker.serviceWorkerRegistration`

A registration of a service worker against a scope. Inherits `EventTarget`.

```ocaml
inherit Js_of_ocaml__.Dom_html.eventTarget
```
```ocaml
method installing : serviceWorker Js_of_ocaml__.Js.t Js_of_ocaml__.Js.opt
                      Js_of_ocaml__.Js.readonly_prop
```
```ocaml
method waiting : serviceWorker Js_of_ocaml__.Js.t Js_of_ocaml__.Js.opt
                   Js_of_ocaml__.Js.readonly_prop
```
```ocaml
method active : serviceWorker Js_of_ocaml__.Js.t Js_of_ocaml__.Js.opt
                  Js_of_ocaml__.Js.readonly_prop
```
```ocaml
method navigationPreload : navigationPreloadManager Js_of_ocaml__.Js.t
                             Js_of_ocaml__.Js.readonly_prop
```
```ocaml
method scope : Js_of_ocaml__.Js.js_string Js_of_ocaml__.Js.t
                 Js_of_ocaml__.Js.readonly_prop
```
```ocaml
method updateViaCache : Js_of_ocaml__.Js.js_string Js_of_ocaml__.Js.t
                          Js_of_ocaml__.Js.readonly_prop
```
```ocaml
method update : serviceWorkerRegistration Js_of_ocaml__.Js.t
                  Js_of_ocaml__.Promise.t
                  Js_of_ocaml__.Js.meth
```
Triggers a soft update of the registration; resolves with the registration object.

```ocaml
method unregister : bool Js_of_ocaml__.Js.t Js_of_ocaml__.Promise.t
                      Js_of_ocaml__.Js.meth
```
Resolves with `true` if the registration was found and unregistered.

```ocaml
method onupdatefound : ('self Js_of_ocaml__.Js.t,
                         'self Js_of_ocaml__.Dom.event Js_of_ocaml__.Js.t)
                         Js_of_ocaml__.Dom.event_listener
                         Js_of_ocaml__.Js.writeonly_prop
```