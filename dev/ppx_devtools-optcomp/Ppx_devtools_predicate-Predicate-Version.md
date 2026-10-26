# Module `Predicate.Version`

```ocaml
type t
```
```ocaml
val of_list : int list -> t
```
```ocaml
val compare : t -> t -> int
```
```ocaml
val current : t
```
```ocaml
type extra_prefix = 
  | Plus
  | Tilde
```
```ocaml
val extra : (extra_prefix * string) option
```
