
# Module `Stdlib.Fun`

```ocaml
val id : 'a -> 'a
```
```ocaml
val const : 'a -> 'b -> 'a
```
```ocaml
val compose : ('b -> 'c) -> ('a -> 'b) -> 'a -> 'c
```
```ocaml
val flip : ('a -> 'b -> 'c) -> 'b -> 'a -> 'c
```
```ocaml
val negate : ('a -> bool) -> 'a -> bool
```
```ocaml
val protect : finally:(unit -> unit) -> (unit -> 'a) -> 'a
```
```ocaml
exception Finally_raised of exn
```
```ocaml
val memoize : ('a -> 'b) -> 'a -> 'b
```