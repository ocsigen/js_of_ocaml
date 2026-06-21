
# Module `Jsoo_runtime.Promise`

Low-level wrap/unwrap helpers for the `Js_of_ocaml.Promise` binding. Implemented in `runtime/{js,wasm}/promise.{js,wat}`. Wrapping is conditional on the value being thenable, so non-thenable values are passed through unchanged.

```ocaml
val wrap : 'a -> Js.t
```
```ocaml
val unwrap : Js.t -> 'a
```