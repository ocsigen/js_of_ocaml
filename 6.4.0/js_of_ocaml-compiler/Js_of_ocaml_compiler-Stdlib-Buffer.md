
# Module `Stdlib.Buffer`

```ocaml
type t = Stdlib__Buffer.t
```
```ocaml
val create : int -> t
```
```ocaml
val contents : t -> string
```
```ocaml
val to_bytes : t -> bytes
```
```ocaml
val sub : t -> int -> int -> string
```
```ocaml
val blit : t -> int -> bytes -> int -> int -> unit
```
```ocaml
val nth : t -> int -> char
```
```ocaml
val length : t -> int
```
```ocaml
val clear : t -> unit
```
```ocaml
val reset : t -> unit
```
```ocaml
val output_buffer : Stdlib.out_channel -> t -> unit
```
```ocaml
val truncate : t -> int -> unit
```
```ocaml
val add_char : t -> char -> unit
```
```ocaml
val add_utf_8_uchar : t -> Stdlib.Uchar.t -> unit
```
```ocaml
val add_utf_16le_uchar : t -> Stdlib.Uchar.t -> unit
```
```ocaml
val add_utf_16be_uchar : t -> Stdlib.Uchar.t -> unit
```
```ocaml
val add_string : t -> string -> unit
```
```ocaml
val add_bytes : t -> bytes -> unit
```
```ocaml
val add_substring : t -> string -> int -> int -> unit
```
```ocaml
val add_subbytes : t -> bytes -> int -> int -> unit
```
```ocaml
val add_substitute : t -> (string -> string) -> string -> unit
```
```ocaml
val add_buffer : t -> t -> unit
```
```ocaml
val add_channel : t -> Stdlib.in_channel -> int -> unit
```
```ocaml
val to_seq : t -> char Stdlib.Seq.t
```
```ocaml
val to_seqi : t -> (int * char) Stdlib.Seq.t
```
```ocaml
val add_seq : t -> char Stdlib.Seq.t -> unit
```
```ocaml
val of_seq : char Stdlib.Seq.t -> t
```
```ocaml
val add_uint8 : t -> int -> unit
```
```ocaml
val add_int8 : t -> int -> unit
```
```ocaml
val add_uint16_ne : t -> int -> unit
```
```ocaml
val add_uint16_be : t -> int -> unit
```
```ocaml
val add_uint16_le : t -> int -> unit
```
```ocaml
val add_int16_ne : t -> int -> unit
```
```ocaml
val add_int16_be : t -> int -> unit
```
```ocaml
val add_int16_le : t -> int -> unit
```
```ocaml
val add_int32_ne : t -> int32 -> unit
```
```ocaml
val add_int32_be : t -> int32 -> unit
```
```ocaml
val add_int32_le : t -> int32 -> unit
```
```ocaml
val add_int64_ne : t -> int64 -> unit
```
```ocaml
val add_int64_be : t -> int64 -> unit
```
```ocaml
val add_int64_le : t -> int64 -> unit
```
```ocaml
val array_conv : char array
```
```ocaml
val add_char_hex : Stdlib.Buffer.t -> Char.t -> unit
```