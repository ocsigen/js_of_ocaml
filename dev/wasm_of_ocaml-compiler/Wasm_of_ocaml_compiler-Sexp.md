
# Module `Wasm_of_ocaml_compiler.Sexp`

```ocaml
type t = 
  | Atom of string
  | List of t list
```
```ocaml
val to_string : t -> string
```
```ocaml
val from_string : string -> t
```
```ocaml
module Util : sig ... end
```