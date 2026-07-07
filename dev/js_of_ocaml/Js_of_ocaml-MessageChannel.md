
# Module `Js_of_ocaml.MessageChannel`

Channel messaging: `MessageChannel`, `MessagePort` and `MessageEvent`.

see [https://developer.mozilla.org/en-US/docs/Web/API/MessageChannel](https://developer.mozilla.org/en-US/docs/Web/API/MessageChannel) 
see [https://developer.mozilla.org/en-US/docs/Web/API/MessagePort](https://developer.mozilla.org/en-US/docs/Web/API/MessagePort) 
see [https://developer.mozilla.org/en-US/docs/Web/API/MessageEvent](https://developer.mozilla.org/en-US/docs/Web/API/MessageEvent) 
see [https://html.spec.whatwg.org/multipage/web-messaging.html](https://html.spec.whatwg.org/multipage/web-messaging.html) 
```ocaml
class type  messagePort = object ... end
```
One of the two endpoints of a [`messageChannel`](./Js_of_ocaml-MessageChannel-class-type-messageChannel.md). Messages posted on one port are delivered to the `message` listener of the other.

```ocaml
class type  messageChannel = object ... end
```
```ocaml
val messageChannel : messageChannel Js.t Js.constr
```

### MessageEvent constructor

```ocaml
class type  messageEventInit = object ... end
```
Initializer for a [`Dom_html.messageEvent`](./Js_of_ocaml-Dom_html-class-type-messageEvent.md). All fields are optional; create an empty record with [`empty_message_event_init`](./#val-empty_message_event_init) and populate the ones you need. The `data` payload is injected as [`Js.Unsafe.any`](./Js_of_ocaml-Js-Unsafe.md#type-any); the resulting event is read back at whatever type the caller annotates [`messageEvent_with_init`](./#val-messageEvent_with_init) with.

```ocaml
val empty_message_event_init : unit -> messageEventInit Js.t
```
```ocaml
val messageEvent : 
  (Js.js_string Js.t -> ('b, 'a) Dom_html.messageEvent Js.t) Js.constr
```
```ocaml
val messageEvent_with_init : 
  (Js.js_string Js.t ->
    messageEventInit Js.t ->
    ('b, 'a) Dom_html.messageEvent Js.t)
    Js.constr
```
```ocaml
val is_supported : unit -> bool
```
Whether the `MessageChannel` global is available in the current environment.
