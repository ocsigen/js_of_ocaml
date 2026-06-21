
# Module `Source_map.Mappings`

```ocaml
type decoded = map list
```
```ocaml
type t
```
Represent the a list of mapping in its encoded form.

```ocaml
val empty : t
```
The empty mapping.

```ocaml
val is_empty : t -> bool
```
Test whether the mapping is empty.

```ocaml
val of_string_unsafe : string -> t
```
`of_string_unsafe` does not perform any validation of its argument, unlike [`decode_exn`](./#val-decode_exn). It is guaranteed that [`of_string_unsafe`](./#val-of_string_unsafe) and [`to_string`](./#val-to_string) are inverse functions. Time complexity O(1)

```ocaml
val decode_exn : t -> decoded
```
Parse the mappings.

```ocaml
val encode : decoded -> t
```
Encode the mappings.

```ocaml
val encode_with_offset : decoded -> Offset.t * t
```
Encode the mappings shifted by the returned offset so that the encoded mapping is more compact. This is useful to combining multiple mappings into an `Index.t`

```ocaml
val number_of_lines : t -> int
```
```ocaml
val first_line : t -> int
```
```ocaml
val to_string : t -> string
```
Returns the mappings as a string in the Source map v3 format. Time complexity O(1)
