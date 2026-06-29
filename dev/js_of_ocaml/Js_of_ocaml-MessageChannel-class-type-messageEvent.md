
# Class type `MessageChannel.messageEvent`

A message delivered to a `message` / `messageerror` listener (also used by [`Worker`](./Js_of_ocaml-Worker.md), [`ServiceWorker`](./Js_of_ocaml-ServiceWorker.md) and `postMessage` on a [`messagePort`](./Js_of_ocaml-MessageChannel-class-type-messagePort.md)). The type parameter `'a` is the type of the `data` payload.

```ocaml
inherit Js_of_ocaml__.Dom_html.event
```
```ocaml
method data : 'a Js_of_ocaml__.Js.readonly_prop
```
```ocaml
method origin : Js_of_ocaml__.Js.js_string Js_of_ocaml__.Js.t
                  Js_of_ocaml__.Js.readonly_prop
```
```ocaml
method lastEventId : Js_of_ocaml__.Js.js_string Js_of_ocaml__.Js.t
                       Js_of_ocaml__.Js.readonly_prop
```
```ocaml
method source : Js_of_ocaml__.Js.Unsafe.any Js_of_ocaml__.Js.opt
                  Js_of_ocaml__.Js.readonly_prop
```
The `source` of a message event is a `MessageEventSource`, i.e. one of a `WindowProxy`, a `messagePort` or a `ServiceWorker.serviceWorker`; `null` when there is none.

```ocaml
method ports : messagePort Js_of_ocaml__.Js.t Js_of_ocaml__.Js.js_array
                 Js_of_ocaml__.Js.t
                 Js_of_ocaml__.Js.readonly_prop
```