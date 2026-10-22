# Module `Ocaml_compiler.Cmo_format`

```ocaml
type t = Compilation_unit_descr.t
```
```ocaml
val name : t -> Global_name.compunit
```
```ocaml
val requires : t -> Global_name.compunit list
```
```ocaml
val provides : t -> Global_name.compunit list
```
```ocaml
val primitives : t -> string list
```
```ocaml
val force_link : t -> bool
```
```ocaml
val imports : t -> Import_info.t list
```
```ocaml
val hints_pos : t -> int
```
```ocaml
val hints_size : t -> int
```
```ocaml
val block_descs : t -> Stdlib.Obj.t option
```
Introcaml: the block descriptors of the unit (`Block_desc.library`), `None` when the compiler has no introspection support.
