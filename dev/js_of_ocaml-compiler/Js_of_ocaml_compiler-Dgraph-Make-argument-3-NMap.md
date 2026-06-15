
# Parameter `Make.NMap`

```ocaml
type key = N.t
```
```ocaml
type 'a t
```
```ocaml
val empty : 'a t
```
```ocaml
val find : key -> 'a t -> 'a
```
```ocaml
val find_opt : key -> 'a t -> 'a option
```
```ocaml
val add : key -> 'a -> 'a t -> 'a t
```