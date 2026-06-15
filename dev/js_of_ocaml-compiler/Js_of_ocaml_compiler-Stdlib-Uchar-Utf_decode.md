
# Module `Uchar.Utf_decode`

```ocaml
type utf_decode
```
The type for UTF decode results. Values of this type represent the result of a Unicode Transformation Format decoding attempt.

```ocaml
val utf_decode_is_valid : utf_decode -> bool
```
`utf_decode_is_valid d` is `true` if and only if `d` holds a valid decode.

```ocaml
val utf_decode_uchar : utf_decode -> t
```
`utf_decode_uchar d` is the Unicode character decoded by `d` if `utf_decode_is_valid d` is `true` and [`Uchar.rep`](./Js_of_ocaml_compiler-Stdlib-Uchar.md#val-rep) otherwise.

```ocaml
val utf_decode_length : utf_decode -> int
```
`utf_decode_length d` is the number of elements from the source that were consumed by the decode `d`. This is always strictly positive and smaller or equal to `4`. The kind of source elements depends on the actual decoder; for the decoders of the standard library this function always returns a length in bytes.

```ocaml
val utf_decode : int -> t -> utf_decode
```
`utf_decode n u` is a valid UTF decode for `u` that consumed `n` elements from the source for decoding. `n` must be positive and smaller or equal to `4` (this is not checked by the module).

```ocaml
val utf_decode_invalid : int -> utf_decode
```
`utf_decode_invalid n` is an invalid UTF decode that consumed `n` elements from the source to error. `n` must be positive and smaller or equal to `4` (this is not checked by the module). The resulting decode has [`rep`](./Js_of_ocaml_compiler-Stdlib-Uchar.md#val-rep) as the decoded Unicode character.

```ocaml
val utf_8_byte_length : t -> int
```
`utf_8_byte_length u` is the number of bytes needed to encode `u` in UTF-8.

```ocaml
val utf_16_byte_length : t -> int
```
`utf_16_byte_length u` is the number of bytes needed to encode `u` in UTF-16.
