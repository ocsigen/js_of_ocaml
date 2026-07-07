
# Class type `Dom_html.messageEvent`

The single shared binding for the DOM `MessageEvent` interface, reused by every "message" event source (`Worker`, `WebSocket`, `EventSource`, [`MessageChannel.messagePort`](./Js_of_ocaml-MessageChannel-class-type-messagePort.md), `BroadcastChannel` and `window.postMessage`). The type parameter `'target` is the type of the event target (the worker, socket, port, window, ... the listener is attached to) and `'data` is the type of the `data` payload.

```ocaml
inherit 'target Js_of_ocaml__.Dom.event
```
```ocaml
method data : 'data Js_of_ocaml__.Js.readonly_prop
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
The `source` of a message event is a `MessageEventSource`, i.e. one of a `WindowProxy`, a `MessageChannel.messagePort` or a `ServiceWorker.serviceWorker`; `null` when there is none.

```ocaml
method ports : Js_of_ocaml__.Js.Unsafe.any Js_of_ocaml__.Js.js_array
                 Js_of_ocaml__.Js.t
                 Js_of_ocaml__.Js.readonly_prop
```
The `MessagePort`s associated with the message. Coerce the elements to `MessageChannel.messagePort` to use them.
