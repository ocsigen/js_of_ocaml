
# Module `Stdlib.Filename`

```ocaml
val current_dir_name : string
```
```ocaml
val parent_dir_name : string
```
```ocaml
val dir_sep : string
```
```ocaml
val concat : string -> string -> string
```
```ocaml
val is_relative : string -> bool
```
```ocaml
val is_implicit : string -> bool
```
```ocaml
val check_suffix : string -> string -> bool
```
```ocaml
val chop_suffix : string -> string -> string
```
```ocaml
val chop_suffix_opt : suffix:string -> string -> string option
```
```ocaml
val extension : string -> string
```
```ocaml
val remove_extension : string -> string
```
```ocaml
val chop_extension : string -> string
```
```ocaml
val basename : string -> string
```
```ocaml
val dirname : string -> string
```
```ocaml
val null : string
```
```ocaml
val temp_file : ?temp_dir:string -> string -> string -> string
```
```ocaml
val open_temp_file : 
  ?mode:Stdlib.open_flag list ->
  ?perms:int ->
  ?temp_dir:string ->
  string ->
  string ->
  string * Stdlib.out_channel
```
```ocaml
val temp_dir : ?temp_dir:string -> ?perms:int -> string -> string -> string
```
```ocaml
val get_temp_dir_name : unit -> string
```
```ocaml
val set_temp_dir_name : string -> unit
```
```ocaml
val quote : string -> string
```
```ocaml
val quote_command : 
  string ->
  ?stdin:string ->
  ?stdout:string ->
  ?stderr:string ->
  string list ->
  string
```
```ocaml
val temp_file_name : temp_dir:string -> string -> string -> string
```
```ocaml
val gen_file : string -> (Stdlib.out_channel -> 'a) -> 'a
```