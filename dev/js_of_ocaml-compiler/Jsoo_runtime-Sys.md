
# Module `Jsoo_runtime.Sys`

```ocaml
type 'a callback = 'a
```
```ocaml
val create_file : name:string -> content:string -> unit
```
```ocaml
val read_file : name:string -> string
```
```ocaml
val set_channel_output' : 
  Stdlib.out_channel ->
  (js_string:Js.t -> unit) callback ->
  unit
```
```ocaml
val set_channel_input' : Stdlib.in_channel -> (unit -> string) callback -> unit
```
```ocaml
val mount_point : unit -> string list
```
```ocaml
val mount_autoload : 
  string ->
  (string -> string -> string option) callback ->
  unit
```
```ocaml
val unmount : string -> unit
```
```ocaml
type redirection
```
```ocaml
val redirect_channel : 
  Stdlib.out_channel ->
  into:Stdlib.out_channel ->
  redirection
```
```ocaml
val restore_channel : Stdlib.out_channel -> redirection -> unit
```
```ocaml
module Config : sig ... end
```
```ocaml
val version : string
```
```ocaml
val git_version : string
```