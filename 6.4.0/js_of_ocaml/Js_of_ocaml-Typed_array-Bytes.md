
# Module `Typed_array.Bytes`

```ocaml
val of_uint8Array : uint8Array Js.t -> bytes
```
This efficiently converts a typed array to `bytes` because it will usually not copy its input.

Modifying its input may also modify its output, and vice versa when modifying its output. This is not a guarantee, however, since certain `bytes` operations may require the runtime to make a copy. One should not use this on input that is sensitive to modification.

```ocaml
val to_uint8Array : bytes -> uint8Array Js.t
```
See the words of caution for [`of_uint8Array`](./#val-of_uint8Array).

```ocaml
val of_arrayBuffer : arrayBuffer Js.t -> bytes
```
See the words of caution for [`of_uint8Array`](./#val-of_uint8Array).
