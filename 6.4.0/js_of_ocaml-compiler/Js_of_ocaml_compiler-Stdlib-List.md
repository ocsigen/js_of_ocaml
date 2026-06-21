
# Module `Stdlib.List`

```ocaml
type !'a t = 'a list = 
  | []
  | :: of 'a * 'a list
```
```ocaml
val length : 'a list -> int
```
```ocaml
val compare_lengths : 'a list -> 'b list -> int
```
```ocaml
val compare_length_with : 'a list -> len:int -> int
```
```ocaml
val is_empty : 'a list -> bool
```
```ocaml
val cons : 'a -> 'a list -> 'a list
```
```ocaml
val singleton : 'a -> 'a list
```
```ocaml
val hd : 'a list -> 'a
```
```ocaml
val tl : 'a list -> 'a list
```
```ocaml
val nth : 'a list -> int -> 'a
```
```ocaml
val nth_opt : 'a list -> int -> 'a option
```
```ocaml
val rev : 'a list -> 'a list
```
```ocaml
val init : len:int -> f:(int -> 'a) -> 'a list
```
```ocaml
val append : 'a list -> 'a list -> 'a list
```
```ocaml
val rev_append : 'a list -> 'a list -> 'a list
```
```ocaml
val concat : 'a list list -> 'a list
```
```ocaml
val flatten : 'a list list -> 'a list
```
```ocaml
val equal : eq:('a -> 'a -> bool) -> 'a list -> 'a list -> bool
```
```ocaml
val compare : cmp:('a -> 'a -> int) -> 'a list -> 'a list -> int
```
```ocaml
val iter : f:('a -> unit) -> 'a list -> unit
```
```ocaml
val iteri : f:(int -> 'a -> unit) -> 'a list -> unit
```
```ocaml
val mapi : f:(int -> 'a -> 'b) -> 'a list -> 'b list
```
```ocaml
val rev_map : f:('a -> 'b) -> 'a list -> 'b list
```
```ocaml
val filter_map : f:('a -> 'b option) -> 'a list -> 'b list
```
```ocaml
val concat_map : f:('a -> 'b list) -> 'a list -> 'b list
```
```ocaml
val fold_left_map : 
  f:('acc -> 'a -> 'acc * 'b) ->
  init:'acc ->
  'a list ->
  'acc * 'b list
```
```ocaml
val fold_left : f:('acc -> 'a -> 'acc) -> init:'acc -> 'a list -> 'acc
```
```ocaml
val fold_right : f:('a -> 'acc -> 'acc) -> 'a list -> init:'acc -> 'acc
```
```ocaml
val iter2 : f:('a -> 'b -> unit) -> 'a list -> 'b list -> unit
```
```ocaml
val map2 : f:('a -> 'b -> 'c) -> 'a list -> 'b list -> 'c list
```
```ocaml
val rev_map2 : f:('a -> 'b -> 'c) -> 'a list -> 'b list -> 'c list
```
```ocaml
val fold_left2 : 
  f:('acc -> 'a -> 'b -> 'acc) ->
  init:'acc ->
  'a list ->
  'b list ->
  'acc
```
```ocaml
val fold_right2 : 
  f:('a -> 'b -> 'acc -> 'acc) ->
  'a list ->
  'b list ->
  init:'acc ->
  'acc
```
```ocaml
val for_all : f:('a -> bool) -> 'a list -> bool
```
```ocaml
val exists : f:('a -> bool) -> 'a list -> bool
```
```ocaml
val for_all2 : f:('a -> 'b -> bool) -> 'a list -> 'b list -> bool
```
```ocaml
val exists2 : f:('a -> 'b -> bool) -> 'a list -> 'b list -> bool
```
```ocaml
val memq : 'a -> set:'a list -> bool
```
```ocaml
val find : f:('a -> bool) -> 'a list -> 'a
```
```ocaml
val find_opt : f:('a -> bool) -> 'a list -> 'a option
```
```ocaml
val find_index : f:('a -> bool) -> 'a list -> int option
```
```ocaml
val find_map : f:('a -> 'b option) -> 'a list -> 'b option
```
```ocaml
val find_mapi : f:(int -> 'a -> 'b option) -> 'a list -> 'b option
```
```ocaml
val filter : f:('a -> bool) -> 'a list -> 'a list
```
```ocaml
val find_all : f:('a -> bool) -> 'a list -> 'a list
```
```ocaml
val filteri : f:(int -> 'a -> bool) -> 'a list -> 'a list
```
```ocaml
val drop : int -> 'a list -> 'a list
```
```ocaml
val take_while : f:('a -> bool) -> 'a list -> 'a list
```
```ocaml
val drop_while : f:('a -> bool) -> 'a list -> 'a list
```
```ocaml
val partition : f:('a -> bool) -> 'a list -> 'a list * 'a list
```
```ocaml
val partition_map : 
  f:('a -> ('b, 'c) Stdlib.Either.t) ->
  'a list ->
  'b list * 'c list
```
```ocaml
val assq : 'a -> ('a * 'b) list -> 'b
```
```ocaml
val assq_opt : 'a -> ('a * 'b) list -> 'b option
```
```ocaml
val mem_assq : 'a -> map:('a * 'b) list -> bool
```
```ocaml
val remove_assq : 'a -> ('a * 'b) list -> ('a * 'b) list
```
```ocaml
val split : ('a * 'b) list -> 'a list * 'b list
```
```ocaml
val combine : 'a list -> 'b list -> ('a * 'b) list
```
```ocaml
val sort : cmp:('a -> 'a -> int) -> 'a list -> 'a list
```
```ocaml
val stable_sort : cmp:('a -> 'a -> int) -> 'a list -> 'a list
```
```ocaml
val fast_sort : cmp:('a -> 'a -> int) -> 'a list -> 'a list
```
```ocaml
val sort_uniq : cmp:('a -> 'a -> int) -> 'a list -> 'a list
```
```ocaml
val merge : cmp:('a -> 'a -> int) -> 'a list -> 'a list -> 'a list
```
```ocaml
val to_seq : 'a list -> 'a Stdlib.Seq.t
```
```ocaml
val of_seq : 'a Stdlib.Seq.t -> 'a list
```
```ocaml
val mem_assoc : 'a -> 'a list -> bool
```
```ocaml
val assoc : 'a -> ('a * 'b) list -> 'b
```
```ocaml
val assoc_opt : 'a -> ('a * 'b) list -> 'b option
```
```ocaml
val remove_assoc : 'a -> ('a * 'b) list -> ('a * 'b) list
```
```ocaml
val mem : eq:('a -> 'b -> bool) -> 'b -> 'a t -> bool
```
```ocaml
val string_assoc : Stdlib.String.t -> (Stdlib.String.t * 'a) list -> 'a option
```
```ocaml
val rev_append_map : f:('a -> 'b) -> 'a t -> 'b t -> 'b t
```
```ocaml
val max_non_tailcall : int
```
```ocaml
val map : 'a t -> f:('a -> 'b) -> 'b t
```
```ocaml
val take' : 'a t -> int -> 'a t -> 'a t * 'a t
```
```ocaml
val take : int -> 'a t -> 'a list * 'a t
```
```ocaml
val last : 'a t -> 'a option
```
```ocaml
val group : 'a t -> f:('a -> 'a -> bool) -> 'a list t
```
```ocaml
val split_last : 'a t -> ('a list * 'a) option
```
```ocaml
val map_last : f:(bool -> 'a -> 'b) -> 'a t -> 'b t
```
```ocaml
val iter_last : f:(bool -> 'a -> unit) -> 'a t -> unit
```