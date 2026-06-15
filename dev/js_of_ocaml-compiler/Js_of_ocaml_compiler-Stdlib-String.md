
# Module `Stdlib.String`

```ocaml
type t = string
```
```ocaml
val make : int -> char -> string
```
```ocaml
val init : int -> f:(int -> char) -> string
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
val blit : 
  src:string ->
  src_pos:int ->
  dst:bytes ->
  dst_pos:int ->
  len:int ->
  unit
```
```ocaml
val concat : sep:string -> string list -> string
```
```ocaml
val cat : string -> string -> string
```
```ocaml
val equal : t -> t -> bool
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
val sub : string -> pos:int -> len:int -> string
```
```ocaml
val split_on_char : sep:char -> string -> string list
```
```ocaml
val map : f:(char -> char) -> string -> string
```
```ocaml
val mapi : f:(int -> char -> char) -> string -> string
```
```ocaml
val fold_left : f:('acc -> char -> 'acc) -> init:'acc -> string -> 'acc
```
```ocaml
val fold_right : f:(char -> 'acc -> 'acc) -> string -> init:'acc -> 'acc
```
```ocaml
val for_all : f:(char -> bool) -> string -> bool
```
```ocaml
val exists : f:(char -> bool) -> string -> bool
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
val iter : f:(char -> unit) -> string -> unit
```
```ocaml
val iteri : f:(int -> char -> unit) -> string -> unit
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
val unsafe_blit : 
  src:string ->
  src_pos:int ->
  dst:bytes ->
  dst_pos:int ->
  len:int ->
  unit
```
```ocaml
module Hashtbl : sig ... end
```
```ocaml
val is_empty : string -> bool
```
```ocaml
val drop_prefix : prefix:string -> string -> string option
```
```ocaml
val is_ascii : string -> bool
```
```ocaml
val has_backslash : string -> bool
```
```ocaml
val lsplit2 : string -> on:char -> (string * string) option
```
```ocaml
val rsplit2 : string -> on:char -> (string * string) option
```
```ocaml
val not_in_x80_to_xBF : int -> bool
```
```ocaml
val not_in_xA0_to_xBF : int -> bool
```
```ocaml
val not_in_x80_to_x9F : int -> bool
```
```ocaml
val not_in_x90_to_xBF : int -> bool
```
```ocaml
val not_in_x80_to_x8F : int -> bool
```
```ocaml
val utf_8_uchar_2 : int -> int -> int
```
```ocaml
val utf_8_uchar_3 : int -> int -> int -> int
```
```ocaml
val utf_8_uchar_4 : int -> int -> int -> int -> int
```
```ocaml
val get_uint8 : string -> int -> int
```
```ocaml
val unsafe_get_uint8 : string -> int -> int
```
```ocaml
val dec_invalid : int -> Uchar.utf_decode
```
```ocaml
val dec_ret : int -> int -> Uchar.utf_decode
```
```ocaml
val get_utf_8_uchar : string -> int -> Uchar.utf_decode
```
```ocaml
val fold_utf_8 : string -> f:('a -> int -> Uchar.t -> 'a) -> 'a -> 'a
```
```ocaml
val fix_utf_8 : string -> string
```
```ocaml
val is_valid_utf_8 : string -> bool
```