
# Module `Link.Wasm_binary`

```ocaml
type importdesc = 
  | Func of int
  | Table
  | Mem
  | Global
  | Tag
```
```ocaml
type import = {
  module_ : string;
  name : string;
  desc : importdesc;
}
```
```ocaml
val check : contents:string -> bool
```
Checks whether `contents` is a Wasm Module

```ocaml
val check_file : file:string -> bool
```
Checks whether `file` contains a Wasm Module

```ocaml
val read_imports : file:string -> import list
```
```ocaml
val append_source_map_section : file:string -> url:string -> unit
```