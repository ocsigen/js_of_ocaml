
# Module `Stdlib.Uchar`

```ocaml
type t = Stdlib__Uchar.t
```
```ocaml
val min : t
```
```ocaml
val max : t
```
```ocaml
val bom : t
```
```ocaml
val rep : t
```
```ocaml
val succ : t -> t
```
```ocaml
val pred : t -> t
```
```ocaml
val is_valid : int -> bool
```
```ocaml
val of_int : int -> t
```
```ocaml
val unsafe_of_int : int -> t
```
```ocaml
val to_int : t -> int
```
```ocaml
val is_char : t -> bool
```
```ocaml
val of_char : char -> t
```
```ocaml
val to_char : t -> char
```
```ocaml
val unsafe_to_char : t -> char
```
```ocaml
val equal : t -> t -> bool
```
```ocaml
val compare : t -> t -> int
```
```ocaml
val seeded_hash : int -> t -> int
```
```ocaml
val hash : t -> int
```
```ocaml
val utf_8_decode_length_of_byte : char -> int
```
```ocaml
val max_utf_8_decode_length : int
```
```ocaml
module Utf_decode : sig ... end
```
```ocaml
type utf_decode = Utf_decode.utf_decode
```
The type for UTF decode results. Values of this type represent the result of a Unicode Transformation Format decoding attempt.

```ocaml
val utf_decode_is_valid : utf_decode -> bool
```
`utf_decode_is_valid d` is `true` if and only if `d` holds a valid decode.

```ocaml
val utf_decode_uchar : utf_decode -> t
```
`utf_decode_uchar d` is the Unicode character decoded by `d` if `utf_decode_is_valid d` is `true` and [`Uchar.rep`](./#val-rep) otherwise.

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
`utf_decode_invalid n` is an invalid UTF decode that consumed `n` elements from the source to error. `n` must be positive and smaller or equal to `4` (this is not checked by the module). The resulting decode has [`rep`](./#val-rep) as the decoded Unicode character.

```ocaml
val utf_8_byte_length : t -> int
```
`utf_8_byte_length u` is the number of bytes needed to encode `u` in UTF-8.

```ocaml
val utf_16_byte_length : t -> int
```
`utf_16_byte_length u` is the number of bytes needed to encode `u` in UTF-16.
