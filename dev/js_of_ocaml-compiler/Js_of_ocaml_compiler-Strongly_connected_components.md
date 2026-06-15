
# Module `Js_of_ocaml_compiler.Strongly_connected_components`

Kosaraju's algorithm for strongly connected components.

```ocaml
module type SET = sig ... end
```
```ocaml
module type MAP = sig ... end
```
```ocaml
module type S = sig ... end
```
```ocaml
module Make (Id : sig ... end) : S with module Id = Id
```