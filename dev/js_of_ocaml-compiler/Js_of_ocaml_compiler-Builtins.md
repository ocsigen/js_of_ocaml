
# Module `Js_of_ocaml_compiler.Builtins`

```ocaml
module File : sig ... end
```
```ocaml
val find : string -> File.t option
```
```ocaml
val all : unit -> File.t list
```
```ocaml
val register : 
  name:string ->
  content:string ->
  fragments:string option ->
  File.t
```