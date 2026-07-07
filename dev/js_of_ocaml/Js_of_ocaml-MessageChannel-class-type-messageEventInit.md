
# Class type `MessageChannel.messageEventInit`

Initializer for a [`Dom_html.messageEvent`](./Js_of_ocaml-Dom_html-class-type-messageEvent.md). All fields are optional; create an empty record with [`empty_message_event_init`](./Js_of_ocaml-MessageChannel.md#val-empty_message_event_init) and populate the ones you need. The `data` payload is injected as [`Js.Unsafe.any`](./Js_of_ocaml-Js-Unsafe.md#type-any); the resulting event is read back at whatever type the caller annotates [`messageEvent_with_init`](./Js_of_ocaml-MessageChannel.md#val-messageEvent_with_init) with.

```ocaml
method data : Js_of_ocaml__.Js.Unsafe.any Js_of_ocaml__.Js.writeonly_prop
```
```ocaml
method origin : Js_of_ocaml__.Js.js_string Js_of_ocaml__.Js.t
                  Js_of_ocaml__.Js.writeonly_prop
```
```ocaml
method lastEventId : Js_of_ocaml__.Js.js_string Js_of_ocaml__.Js.t
                       Js_of_ocaml__.Js.writeonly_prop
```
```ocaml
method source : Js_of_ocaml__.Js.Unsafe.any Js_of_ocaml__.Js.writeonly_prop
```
```ocaml
method ports : messagePort Js_of_ocaml__.Js.t Js_of_ocaml__.Js.js_array
                 Js_of_ocaml__.Js.t
                 Js_of_ocaml__.Js.writeonly_prop
```