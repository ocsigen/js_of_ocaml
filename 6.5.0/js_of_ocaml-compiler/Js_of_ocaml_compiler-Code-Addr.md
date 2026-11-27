# Module `Code.Addr`

```ocaml
type t = int
```
```ocaml
val to_string : t -> string
```
```ocaml
val zero : t
```
```ocaml
val succ : t -> t
```
```ocaml
val pred : t -> t
```
```ocaml
module Set = Int_set
```
```ocaml
module Map = Int_trie
```
```ocaml
module Hashtbl : Js_of_ocaml_compiler.Stdlib.Hashtbl.S with type key = t
```
