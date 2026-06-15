
# Module `Js_of_ocaml.Abort`

AbortController / AbortSignal.

A general-purpose cancellation primitive used by [`Fetch`](./Js_of_ocaml-Fetch.md), event listener registration, `Streams`, and custom asynchronous code.

see [https://developer.mozilla.org/en-US/docs/Web/API/AbortController](https://developer.mozilla.org/en-US/docs/Web/API/AbortController) 
see [https://dom.spec.whatwg.org/\#interface-abortcontroller](https://dom.spec.whatwg.org/#interface-abortcontroller) 
```ocaml
class type  signal = object ... end
```
```ocaml
class type  controller = object ... end
```
```ocaml
val controller : controller Js.t Js.constr
```