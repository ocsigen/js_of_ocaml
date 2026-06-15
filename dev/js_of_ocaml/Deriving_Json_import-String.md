
# Module `Deriving_Json_import.String`

```ocaml
type t = string
```
```ocaml
val make : int -> char -> string
```
```ocaml
val init : int -> (int -> char) -> string
```
```ocaml
val empty : string
```
```ocaml
val length : string -> int
```
```ocaml
val get : string -> int -> char
```
```ocaml
val of_bytes : bytes -> string
```
```ocaml
val to_bytes : string -> bytes
```
```ocaml
val blit : string -> int -> bytes -> int -> int -> unit
```
```ocaml
val concat : string -> string list -> string
```
```ocaml
val cat : string -> string -> string
```
```ocaml
val compare : t -> t -> int
```
```ocaml
val starts_with : prefix:string -> string -> bool
```
```ocaml
val ends_with : suffix:string -> string -> bool
```
```ocaml
val contains_from : string -> int -> char -> bool
```
```ocaml
val rcontains_from : string -> int -> char -> bool
```
```ocaml
val contains : string -> char -> bool
```
```ocaml
val sub : string -> int -> int -> string
```
```ocaml
val split_on_char : char -> string -> string list
```
```ocaml
val map : (char -> char) -> string -> string
```
```ocaml
val mapi : (int -> char -> char) -> string -> string
```
```ocaml
val fold_left : ('acc -> char -> 'acc) -> 'acc -> string -> 'acc
```
```ocaml
val fold_right : (char -> 'acc -> 'acc) -> string -> 'acc -> 'acc
```
```ocaml
val for_all : (char -> bool) -> string -> bool
```
```ocaml
val exists : (char -> bool) -> string -> bool
```
```ocaml
val trim : string -> string
```
```ocaml
val escaped : string -> string
```
```ocaml
val uppercase_ascii : string -> string
```
```ocaml
val lowercase_ascii : string -> string
```
```ocaml
val capitalize_ascii : string -> string
```
```ocaml
val uncapitalize_ascii : string -> string
```
```ocaml
val iter : (char -> unit) -> string -> unit
```
```ocaml
val iteri : (int -> char -> unit) -> string -> unit
```
```ocaml
val index_from : string -> int -> char -> int
```
```ocaml
val index_from_opt : string -> int -> char -> int option
```
```ocaml
val rindex_from : string -> int -> char -> int
```
```ocaml
val rindex_from_opt : string -> int -> char -> int option
```
```ocaml
val index : string -> char -> int
```
```ocaml
val index_opt : string -> char -> int option
```
```ocaml
val rindex : string -> char -> int
```
```ocaml
val rindex_opt : string -> char -> int option
```
```ocaml
val to_seq : t -> char Stdlib.Seq.t
```
```ocaml
val to_seqi : t -> (int * char) Stdlib.Seq.t
```
```ocaml
val of_seq : char Stdlib.Seq.t -> t
```
```ocaml
val get_utf_8_uchar : t -> int -> Stdlib.Uchar.utf_decode
```
```ocaml
val is_valid_utf_8 : t -> bool
```
```ocaml
val get_utf_16be_uchar : t -> int -> Stdlib.Uchar.utf_decode
```
```ocaml
val is_valid_utf_16be : t -> bool
```
```ocaml
val get_utf_16le_uchar : t -> int -> Stdlib.Uchar.utf_decode
```
```ocaml
val is_valid_utf_16le : t -> bool
```
```ocaml
val edit_distance : ?limit:int -> t -> t -> int
```
```ocaml
val spellcheck : 
  ?max_dist:(string -> int) ->
  ((string -> unit) -> unit) ->
  string ->
  string list
```
```ocaml
val get_uint8 : string -> int -> int
```
```ocaml
val get_int8 : string -> int -> int
```
```ocaml
val get_uint16_ne : string -> int -> int
```
```ocaml
val get_uint16_be : string -> int -> int
```
```ocaml
val get_uint16_le : string -> int -> int
```
```ocaml
val get_int16_ne : string -> int -> int
```
```ocaml
val get_int16_be : string -> int -> int
```
```ocaml
val get_int16_le : string -> int -> int
```
```ocaml
val get_int32_ne : string -> int -> int32
```
```ocaml
val hash : t -> int
```
```ocaml
val seeded_hash : int -> t -> int
```
```ocaml
val get_int32_be : string -> int -> int32
```
```ocaml
val get_int32_le : string -> int -> int32
```
```ocaml
val get_int64_ne : string -> int -> int64
```
```ocaml
val get_int64_be : string -> int -> int64
```
```ocaml
val get_int64_le : string -> int -> int64
```
```ocaml
val unsafe_get : string -> int -> char
```
```ocaml
val unsafe_blit : string -> int -> bytes -> int -> int -> unit
```
```ocaml
val equal : string -> string -> bool
```