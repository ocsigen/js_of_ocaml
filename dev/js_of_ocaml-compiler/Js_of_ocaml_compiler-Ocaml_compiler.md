
# Module `Js_of_ocaml_compiler.Ocaml_compiler`

```ocaml
val constant_of_const : Lambda.structured_constant -> Code.constant
```
```ocaml
type module_or_not = 
  | Module
  | Not_module
  | Unknown
```
```ocaml
val is_module_in_summary : Ident.t -> Env.summary -> module_or_not
```
```ocaml
module Symtable : sig ... end
```
```ocaml
module Import_info : sig ... end
```
```ocaml
module Compilation_unit : sig ... end
```
```ocaml
module Compilation_unit_descr : sig ... end
```
```ocaml
module Cmo_format : sig ... end
```