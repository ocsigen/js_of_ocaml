
# Class type `ServiceWorker.extendableMessageEvent`

The event delivered to the worker's `message` / `messageerror` handlers. Unlike a plain [`Dom_html.messageEvent`](./Js_of_ocaml-Dom_html-class-type-messageEvent.md) it extends [`extendableEvent`](./Js_of_ocaml-ServiceWorker-class-type-extendableEvent.md), so the handler can call `waitUntil` to keep the worker alive until asynchronous processing of the message finishes. The type parameter `'a` is the type of the `data` payload.

```ocaml
inherit extendableEvent
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
The message source: a `client`, a `serviceWorker` or a `MessageChannel.messagePort`; `null` when there is none.

```ocaml
method ports : Js_of_ocaml__.MessageChannel.messagePort Js_of_ocaml__.Js.t
                 Js_of_ocaml__.Js.js_array
                 Js_of_ocaml__.Js.t
                 Js_of_ocaml__.Js.readonly_prop
```