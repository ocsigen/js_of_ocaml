
# Class type `MessageChannel.messagePort`

One of the two endpoints of a [`messageChannel`](./Js_of_ocaml-MessageChannel-class-type-messageChannel.md). Messages posted on one port are delivered to the `message` listener of the other.

```ocaml
inherit Js_of_ocaml__.Dom_html.eventTarget
```
```ocaml
method postMessage : 'a. 'a -> unit Js_of_ocaml__.Js.meth
```
```ocaml
method postMessage_withTransfer : 'a. 'a ->
  Js_of_ocaml__.Js.Unsafe.any Js_of_ocaml__.Js.js_array Js_of_ocaml__.Js.t ->
  unit Js_of_ocaml__.Js.meth
```
`port##postMessage_withTransfer msg transfer` posts `msg`, transferring ownership of the [transferable objects](https://developer.mozilla.org/en-US/docs/Web/API/Web_Workers_API/Transferable_objects) in `transfer` (e.g. `ArrayBuffer`s or other `messagePort`s) to the receiving context.

```ocaml
method start : unit Js_of_ocaml__.Js.meth
```
```ocaml
method close : unit Js_of_ocaml__.Js.meth
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
```ocaml
method onclose : ('self Js_of_ocaml__.Js.t,
                   'self Js_of_ocaml__.Dom.event Js_of_ocaml__.Js.t)
                   Js_of_ocaml__.Dom.event_listener
                   Js_of_ocaml__.Js.writeonly_prop
```