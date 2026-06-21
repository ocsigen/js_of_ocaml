
# Module `Stdlib.In_channel`

```ocaml
val stdlib_input_line : Stdlib.in_channel -> string
```
```ocaml
type t = Stdlib.in_channel
```
```ocaml
type open_flag = Stdlib.open_flag = 
  | Open_rdonly
  | Open_wronly
  | Open_append
  | Open_creat
  | Open_trunc
  | Open_excl
  | Open_binary
  | Open_text
  | Open_nonblock
```
```ocaml
val stdin : t
```
```ocaml
val open_bin : string -> t
```
```ocaml
val open_text : string -> t
```
```ocaml
val open_gen : open_flag list -> int -> string -> t
```
```ocaml
val with_open_bin : string -> (t -> 'a) -> 'a
```
```ocaml
val with_open_text : string -> (t -> 'a) -> 'a
```
```ocaml
val with_open_gen : open_flag list -> int -> string -> (t -> 'a) -> 'a
```
```ocaml
val close : t -> unit
```
```ocaml
val close_noerr : t -> unit
```
```ocaml
val input_char : t -> char option
```
```ocaml
val input_byte : t -> int option
```
```ocaml
val input_line : t -> string option
```
```ocaml
val really_input_string : t -> int -> string option
```
```ocaml
val input_all : t -> string
```
```ocaml
val input_lines : t -> string list
```
```ocaml
val input : t -> bytes -> int -> int -> int
```
```ocaml
val input_bigarray : 
  t ->
  ('a, Stdlib.Bigarray.int8_unsigned_elt, Stdlib.Bigarray.c_layout)
    Stdlib.Bigarray.Array1.t ->
  int ->
  int ->
  int
```
```ocaml
val really_input : t -> bytes -> int -> int -> unit option
```
```ocaml
val really_input_bigarray : 
  t ->
  ('a, Stdlib.Bigarray.int8_unsigned_elt, Stdlib.Bigarray.c_layout)
    Stdlib.Bigarray.Array1.t ->
  int ->
  int ->
  unit option
```
```ocaml
val fold_lines : ('acc -> string -> 'acc) -> 'acc -> t -> 'acc
```
```ocaml
val seek : t -> int64 -> unit
```
```ocaml
val pos : t -> int64
```
```ocaml
val length : t -> int64
```
```ocaml
val set_binary_mode : t -> bool -> unit
```
```ocaml
val is_binary_mode : t -> bool
```
```ocaml
val isatty : t -> bool
```
```ocaml
val input_line_exn : Stdlib.in_channel -> string
```