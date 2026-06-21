
# Module `Stdlib.Seq`

```ocaml
type !'a t = unit -> 'a node
```
```ocaml
and !'a node = 'a Stdlib__Seq.node = 
  | Nil
  | Cons of 'a * 'a t
```
```ocaml
val is_empty : 'a t -> bool
```
```ocaml
val uncons : 'a t -> ('a * 'a t) option
```
```ocaml
val length : 'a t -> int
```
```ocaml
val iter : ('a -> unit) -> 'a t -> unit
```
```ocaml
val fold_left : ('acc -> 'a -> 'acc) -> 'acc -> 'a t -> 'acc
```
```ocaml
val iteri : (int -> 'a -> unit) -> 'a t -> unit
```
```ocaml
val fold_lefti : ('acc -> int -> 'a -> 'acc) -> 'acc -> 'a t -> 'acc
```
```ocaml
val for_all : ('a -> bool) -> 'a t -> bool
```
```ocaml
val exists : ('a -> bool) -> 'a t -> bool
```
```ocaml
val find : ('a -> bool) -> 'a t -> 'a option
```
```ocaml
val find_index : ('a -> bool) -> 'a t -> int option
```
```ocaml
val find_map : ('a -> 'b option) -> 'a t -> 'b option
```
```ocaml
val find_mapi : (int -> 'a -> 'b option) -> 'a t -> 'b option
```
```ocaml
val iter2 : ('a -> 'b -> unit) -> 'a t -> 'b t -> unit
```
```ocaml
val fold_left2 : ('acc -> 'a -> 'b -> 'acc) -> 'acc -> 'a t -> 'b t -> 'acc
```
```ocaml
val for_all2 : ('a -> 'b -> bool) -> 'a t -> 'b t -> bool
```
```ocaml
val exists2 : ('a -> 'b -> bool) -> 'a t -> 'b t -> bool
```
```ocaml
val equal : ('a -> 'b -> bool) -> 'a t -> 'b t -> bool
```
```ocaml
val compare : ('a -> 'b -> int) -> 'a t -> 'b t -> int
```
```ocaml
val empty : 'a t
```
```ocaml
val return : 'a -> 'a t
```
```ocaml
val cons : 'a -> 'a t -> 'a t
```
```ocaml
val singleton : 'a -> 'a t
```
```ocaml
val init : int -> (int -> 'a) -> 'a t
```
```ocaml
val unfold : ('b -> ('a * 'b) option) -> 'b -> 'a t
```
```ocaml
val repeat : 'a -> 'a t
```
```ocaml
val forever : (unit -> 'a) -> 'a t
```
```ocaml
val cycle : 'a t -> 'a t
```
```ocaml
val iterate : ('a -> 'a) -> 'a -> 'a t
```
```ocaml
val map : ('a -> 'b) -> 'a t -> 'b t
```
```ocaml
val filter : ('a -> bool) -> 'a t -> 'a t
```
```ocaml
val filteri : (int -> 'a -> bool) -> 'a t -> 'a t
```
```ocaml
val filter_map : ('a -> 'b option) -> 'a t -> 'b t
```
```ocaml
val scan : ('b -> 'a -> 'b) -> 'b -> 'a t -> 'b t
```
```ocaml
val take : int -> 'a t -> 'a t
```
```ocaml
val drop : int -> 'a t -> 'a t
```
```ocaml
val take_while : ('a -> bool) -> 'a t -> 'a t
```
```ocaml
val drop_while : ('a -> bool) -> 'a t -> 'a t
```
```ocaml
val group : ('a -> 'a -> bool) -> 'a t -> 'a t t
```
```ocaml
val memoize : 'a t -> 'a t
```
```ocaml
exception Forced_twice
```
```ocaml
val once : 'a t -> 'a t
```
```ocaml
val transpose : 'a t t -> 'a t t
```
```ocaml
val append : 'a t -> 'a t -> 'a t
```
```ocaml
val concat : 'a t t -> 'a t
```
```ocaml
val flat_map : ('a -> 'b t) -> 'a t -> 'b t
```
```ocaml
val concat_map : ('a -> 'b t) -> 'a t -> 'b t
```
```ocaml
val zip : 'a t -> 'b t -> ('a * 'b) t
```
```ocaml
val map2 : ('a -> 'b -> 'c) -> 'a t -> 'b t -> 'c t
```
```ocaml
val interleave : 'a t -> 'a t -> 'a t
```
```ocaml
val sorted_merge : ('a -> 'a -> int) -> 'a t -> 'a t -> 'a t
```
```ocaml
val product : 'a t -> 'b t -> ('a * 'b) t
```
```ocaml
val map_product : ('a -> 'b -> 'c) -> 'a t -> 'b t -> 'c t
```
```ocaml
val unzip : ('a * 'b) t -> 'a t * 'b t
```
```ocaml
val split : ('a * 'b) t -> 'a t * 'b t
```
```ocaml
val partition_map : ('a -> ('b, 'c) Stdlib.Either.t) -> 'a t -> 'b t * 'c t
```
```ocaml
val partition : ('a -> bool) -> 'a t -> 'a t * 'a t
```
```ocaml
val of_dispenser : (unit -> 'a option) -> 'a t
```
```ocaml
val to_dispenser : 'a t -> unit -> 'a option
```
```ocaml
val ints : int -> int t
```
```ocaml
val mapi_aux : (int -> 'a -> 'b) -> int -> 'a t -> 'b t
```
```ocaml
val mapi : (int -> 'a -> 'b) -> 'a t -> 'b t
```