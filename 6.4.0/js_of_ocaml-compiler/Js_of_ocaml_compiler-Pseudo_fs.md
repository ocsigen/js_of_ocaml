
# Module `Js_of_ocaml_compiler.Pseudo_fs`

```ocaml
val collect_cmis : 
  cmis:Stdlib.StringSet.t ->
  paths:string list ->
  (string * string) list
```
```ocaml
val f : 
  prim:[ `create_file | `create_file_extern ] ->
  cmis:Stdlib.StringSet.t ->
  files:string list ->
  paths:string list ->
  Code.instr list
```
```ocaml
val list_files : string -> string list -> (string * string) list
```
```ocaml
val embed_file : name:string -> filename:string -> Code.instr
```
```ocaml
val init : unit -> Code.instr
```