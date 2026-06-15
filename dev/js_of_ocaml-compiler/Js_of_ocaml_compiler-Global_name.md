
# Module `Js_of_ocaml_compiler.Global_name`

```ocaml
type compunit = 
  | Compunit of string
```
```ocaml
type predef = 
  | Predef of string
```
```ocaml
type t = 
  | Glob_compunit of compunit
  | Glob_predef of predef
```
```ocaml
val to_string : t -> string
```
```ocaml
val is_predef : t -> bool
```
```ocaml
val compare : t -> t -> int
```
```ocaml
val equal : t -> t -> bool
```
```ocaml
val hash : t -> int
```
```ocaml
val compare_compunit : compunit -> compunit -> int
```
```ocaml
module Compunit_set : Stdlib.Set.S with type elt = compunit
```
```ocaml
module Compunit_hashtbl : 
  Js_of_ocaml_compiler.Stdlib.Hashtbl.S with type key = compunit
```
```ocaml
module Hashtbl : Js_of_ocaml_compiler.Stdlib.Hashtbl.S with type key = t
```
```ocaml
module Set : Stdlib.Set.S with type elt = t
```
```ocaml
module Map : Stdlib.Map.S with type key = t
```