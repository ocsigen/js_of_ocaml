
# Module `Jsoo_runtime.Typed_array`

```ocaml
type ('a, 'b) typedArray = Js.t
```
```ocaml
type arrayBuffer = Js.t
```
```ocaml
type uint8Array = Js.t
```
```ocaml
val kind : ('a, 'b) typedArray -> ('a, 'b) Stdlib.Bigarray.kind
```
```ocaml
val from_genarray : 
  ('a, 'b, Stdlib.Bigarray.c_layout) Stdlib.Bigarray.Genarray.t ->
  ('a, 'b) typedArray
```
```ocaml
val to_genarray : 
  ('a, 'b) typedArray ->
  ('a, 'b, Stdlib.Bigarray.c_layout) Stdlib.Bigarray.Genarray.t
```
```ocaml
module Bigstring : sig ... end
```
```ocaml
val string_of_uint8Array : uint8Array -> string
```
```ocaml
val bytes_of_uint8Array : uint8Array -> bytes
```
```ocaml
val uint8Array_of_bytes : bytes -> uint8Array
```