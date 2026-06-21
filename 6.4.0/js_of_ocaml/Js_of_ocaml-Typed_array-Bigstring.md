
# Module `Typed_array.Bigstring`

```ocaml
type t =
  (char, Stdlib.Bigarray.int8_unsigned_elt, Stdlib.Bigarray.c_layout)
    Stdlib.Bigarray.Array1.t
```
```ocaml
val to_arrayBuffer : t -> arrayBuffer Js.t
```
```ocaml
val to_uint8Array : t -> uint8Array Js.t
```
```ocaml
val of_arrayBuffer : arrayBuffer Js.t -> t
```
```ocaml
val of_uint8Array : uint8Array Js.t -> t
```