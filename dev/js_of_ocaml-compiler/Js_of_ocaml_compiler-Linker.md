
# Module `Js_of_ocaml_compiler.Linker`

```ocaml
module Fragment : sig ... end
```
```ocaml
val reset : unit -> unit
```
```ocaml
val load_files : target_env:Target_env.t -> string list -> unit
```
```ocaml
val load_fragments : 
  target_env:Target_env.t ->
  filename:string ->
  Fragment.t list ->
  unit
```
```ocaml
val check_deps : unit -> unit
```
```ocaml
type state
```
```ocaml
type always_required = {
  filename : string;
  program : Javascript.program;
  requires : string list;
}
```
```ocaml
type output = {
  runtime_code : Javascript.program;
  always_required_codes : always_required list;
}
```
```ocaml
val list_all : ?from:string list -> unit -> Stdlib.StringSet.t
```
```ocaml
val list_all_with_aliases : 
  ?from:string list ->
  unit ->
  Stdlib.StringSet.t Stdlib.StringMap.t
```
```ocaml
val init : ?from:string list -> unit -> state
```
```ocaml
val resolve_deps : 
  ?check_missing:bool ->
  state ->
  Stdlib.StringSet.t ->
  state * Stdlib.StringSet.t
```
```ocaml
val link : ?check_missing:bool -> Javascript.program -> state -> output
```
```ocaml
val all : state -> string list
```
```ocaml
val missing : state -> string list
```
```ocaml
val origin : name:string -> string option
```
```ocaml
val deprecated : name:string -> bool
```