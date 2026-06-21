
# Parameter `Make_Imperative.NTbl`

```ocaml
type 'a t
```
```ocaml
type key = N.t
```
```ocaml
type size
```
```ocaml
val get : 'a t -> key -> 'a
```
```ocaml
val set : 'a t -> key -> 'a -> unit
```
```ocaml
val make : size -> 'a -> 'a t
```