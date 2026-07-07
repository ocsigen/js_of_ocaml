
# Module `Js_of_ocaml.Worker`

Low-level bindgins to javascript Web Workers.

See [the documented Javascript API](https://developer.mozilla.org/en-US/docs/Web/API/Worker) and some more general documentation [about the usage of WebWorker](https://developer.mozilla.org/en-US/docs/Web/API/Web_Workers_API/Using_web_workers).

```ocaml
class type ['a, 'b] worker = object ... end
```
```ocaml
class type  errorEvent = object ... end
```
```ocaml
type 'a messageEvent = (Dom_html.element, 'a) Dom_html.messageEvent
```
The type parameter `'a` is the type of the `data` payload.

deprecated Use Dom\_html.messageEvent, the single shared binding.
```ocaml
val create : string -> ('a, 'b) worker Js.t
```

### Global function to be used by the worker.

```ocaml
val import_scripts : string list -> unit
```
```ocaml
val set_onmessage : ('a -> unit) -> unit
```
```ocaml
val post_message : 'a -> unit
```