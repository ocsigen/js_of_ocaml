
# Module `Js_of_ocaml_compiler.Magic_number`

```ocaml
type t = private string * int
```
```ocaml
exception Bad_magic_number of string
```
```ocaml
exception Bad_magic_version of t
```
```ocaml
val size : int
```
```ocaml
val compare : t -> t -> int
```
```ocaml
val equal : t -> t -> bool
```
```ocaml
val of_string : string -> t
```
```ocaml
val to_string : t -> string
```
```ocaml
val kind : t -> [ `Cmo | `Cma | `Exe | `Other of string ]
```
```ocaml
val current_exe : t
```
```ocaml
val current_cmo : t
```
```ocaml
val current_cma : t
```
```ocaml
val current : [ `Cmo | `Cma | `Exe ] -> t
```