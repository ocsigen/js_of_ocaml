
# Module `Symtable.GlobalMap`

```ocaml
type t
```
```ocaml
val empty : t
```
```ocaml
val filter : (Global_name.t -> bool) -> t -> t
```
```ocaml
val find : Global_name.t -> t -> int
```
```ocaml
val iter : f:(Global_name.t -> int -> unit) -> t -> unit
```
```ocaml
val fold : (Global_name.t -> int -> 'a -> 'a) -> t -> 'a -> 'a
```
```ocaml
val enter : t Stdlib.ref -> Global_name.t -> int
```