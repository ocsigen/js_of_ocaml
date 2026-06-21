
# Module type `Strongly_connected_components.S`

```ocaml
module Id : sig ... end
```
```ocaml
type directed_graph = Id.Set.t Id.Map.t
```
```ocaml
type component = 
  | Has_loop of Id.t list
  | No_loop of Id.t
```
```ocaml
val connected_components_sorted_from_roots_to_leaf : 
  directed_graph ->
  component array
```
```ocaml
val component_graph : directed_graph -> (component * int list) array
```