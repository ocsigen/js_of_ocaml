# Class type `EventSource.messageEvent`

The type parameter `'a` is the type of the event target; the `data` payload of an `EventSource` message is always `js_string t`.

deprecated Use Dom\_html.messageEvent, the single shared binding.
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
