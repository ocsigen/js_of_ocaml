
# Module `Jsoo_runtime`

```ocaml
module Js : sig ... end
```
```ocaml
module Sys : sig ... end
```
```ocaml
module Error : sig ... end
```
```ocaml
module For_compatibility_only : sig ... end
```
```ocaml
module Typed_array : sig ... end
```
```ocaml
module Int64 : sig ... end
```
```ocaml
module Effect : sig ... end
```
```ocaml
module Promise : sig ... end
```
Low-level wrap/unwrap helpers for the `Js_of_ocaml.Promise` binding. Implemented in `runtime/{js,wasm}/promise.{js,wat}`. Wrapping is conditional on the value being thenable, so non-thenable values are passed through unchanged.
