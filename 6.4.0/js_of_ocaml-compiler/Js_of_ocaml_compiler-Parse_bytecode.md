
# Module `Js_of_ocaml_compiler.Parse_bytecode`

```ocaml
module Debug : sig ... end
```
```ocaml
type one = {
  code : Code.program;
  cmis : Stdlib.StringSet.t;
  debug : Debug.summary;
}
```
```ocaml
module Toc : sig ... end
```
```ocaml
val read_primitives : Toc.t -> Stdlib.in_channel -> string list
```
```ocaml
val from_exe : 
  ?includes:string list ->
  linkall:bool ->
  link_info:bool ->
  include_cmis:bool ->
  ?exported_unit:string list ->
  ?debug:bool ->
  Stdlib.in_channel ->
  one
```
```ocaml
val from_cmo : 
  ?includes:string list ->
  ?include_cmis:bool ->
  ?debug:bool ->
  Ocaml_compiler.Cmo_format.t ->
  Stdlib.in_channel ->
  one
```
```ocaml
val from_cma : 
  ?includes:string list ->
  ?include_cmis:bool ->
  ?debug:bool ->
  Cmo_format.library ->
  Stdlib.in_channel ->
  one
```
```ocaml
val from_channel : 
  Stdlib.in_channel ->
  [ `Cmo of Ocaml_compiler.Cmo_format.t | `Cma of Cmo_format.library | `Exe ]
```
```ocaml
val from_string : 
  prims:string array ->
  debug:Instruct.debug_event list array ->
  string ->
  Code.program
```
```ocaml
val normalize_bytecode : string -> string
```
```ocaml
val predefined_exceptions : unit -> Code.program * Unit_info.t
```
```ocaml
type bytesections = {
  symb : Ocaml_compiler.Symtable.GlobalMap.t;
  crcs : Ocaml_compiler.Import_info.table;
  prim : string list;
  dlpt : string list;
}
```
```ocaml
val read_crcs : Stdlib.in_channel -> Ocaml_compiler.Import_info.t list
```
```ocaml
val link_info : 
  symbols:Ocaml_compiler.Symtable.GlobalMap.t ->
  primitives:Stdlib.StringSet.t ->
  crcs:Ocaml_compiler.Import_info.t list ->
  num_globals:int ->
  Code.program
```