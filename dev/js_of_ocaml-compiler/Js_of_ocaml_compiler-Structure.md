
# Module `Js_of_ocaml_compiler.Structure`

```ocaml
type graph
```
```ocaml
type t
```
```ocaml
val get_edges : graph -> Code.Addr.t -> Code.Addr.Set.t
```
```ocaml
val is_backward : t -> Code.Addr.t -> Code.Addr.t -> bool
```
```ocaml
val is_forward : t -> Code.Addr.t -> Code.Addr.t -> bool
```
```ocaml
val build_graph : 
  Code.block Js_of_ocaml_compiler.Code.Addr.Map.t ->
  Code.Addr.t ->
  t
```
```ocaml
val dominator_tree : t -> graph
```
```ocaml
val is_merge_node : t -> Code.Addr.t -> bool
```
```ocaml
val is_loop_header : t -> Code.Addr.t -> bool
```
```ocaml
val sort_in_post_order : t -> Code.Addr.t list -> Code.Addr.t list
```
```ocaml
val blocks_in_reverse_post_order : t -> Code.Addr.t list
```
```ocaml
val get_nodes : t -> Code.Addr.Set.t
```
```ocaml
val norm : Code.program -> Code.program
```
`norm p` normalizes a program `p` to accommodate `Structure.build_graph` logic. In practice, it ensures that all loops have a predecessor block and allows to exit loops early.
