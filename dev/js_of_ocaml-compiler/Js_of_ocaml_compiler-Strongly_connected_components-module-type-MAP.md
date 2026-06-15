
# Module type `Strongly_connected_components.MAP`

```ocaml
type key
```
```ocaml
type 'a t
```
```ocaml
val cardinal : 'a t -> int
```
```ocaml
val bindings : 'a t -> (key * 'a) list
```
```ocaml
val empty : 'a t
```
```ocaml
val find : key -> 'a t -> 'a
```
```ocaml
val add : key -> 'a -> 'a t -> 'a t
```