
# Module `Stdlib.Option`

```ocaml
type !'a t = 'a option = 
  | None
  | Some of 'a
```
```ocaml
val none : 'a option
```
```ocaml
val some : 'a -> 'a option
```
```ocaml
val get : 'a option -> 'a
```
```ocaml
val join : 'a option option -> 'a option
```
```ocaml
val fold : none:'a -> some:('b -> 'a) -> 'b option -> 'a
```
```ocaml
val is_none : 'a option -> bool
```
```ocaml
val is_some : 'a option -> bool
```
```ocaml
val equal : ('a -> 'a -> bool) -> 'a option -> 'a option -> bool
```
```ocaml
val compare : ('a -> 'a -> int) -> 'a option -> 'a option -> int
```
```ocaml
val to_result : none:'e -> 'a option -> ('a, 'e) Stdlib.result
```
```ocaml
val to_list : 'a option -> 'a list
```
```ocaml
val to_seq : 'a option -> 'a Stdlib.Seq.t
```
```ocaml
val map : f:('a -> 'b) -> 'a t -> 'b t
```
```ocaml
val bind : f:('a -> 'b t) -> 'a t -> 'b t
```
```ocaml
val iter : f:('a -> unit) -> 'a t -> unit
```
```ocaml
val filter : f:('a -> bool) -> 'a t -> 'a t
```
```ocaml
val value : default:'a -> 'a t -> 'a
```