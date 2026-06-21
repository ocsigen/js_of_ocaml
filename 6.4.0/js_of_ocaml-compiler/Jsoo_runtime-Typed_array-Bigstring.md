
# Module `Typed_array.Bigstring`

```ocaml
type t =
  (char, Stdlib.Bigarray.int8_unsigned_elt, Stdlib.Bigarray.c_layout)
    Stdlib.Bigarray.Array1.t
```
```ocaml
val to_arrayBuffer : t -> arrayBuffer
```
```ocaml
val to_uint8Array : t -> uint8Array
```
```ocaml
val of_arrayBuffer : arrayBuffer -> t
```
```ocaml
val of_uint8Array : uint8Array -> t
```