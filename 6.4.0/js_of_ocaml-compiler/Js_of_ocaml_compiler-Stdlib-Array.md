
# Module `Stdlib.Array`

```ocaml
type !'a t = 'a array
```
```ocaml
val length : 'a array -> int
```
```ocaml
val get : 'a array -> int -> 'a
```
```ocaml
val set : 'a array -> int -> 'a -> unit
```
```ocaml
val make : int -> 'a -> 'a array
```
```ocaml
val create_float : int -> float array
```
```ocaml
val init : int -> f:(int -> 'a) -> 'a array
```
```ocaml
val make_matrix : dimx:int -> dimy:int -> 'a -> 'a array array
```
```ocaml
val init_matrix : 
  dimx:int ->
  dimy:int ->
  f:(int -> int -> 'a) ->
  'a array array
```
```ocaml
val append : 'a array -> 'a array -> 'a array
```
```ocaml
val concat : 'a array list -> 'a array
```
```ocaml
val sub : 'a array -> pos:int -> len:int -> 'a array
```
```ocaml
val copy : 'a array -> 'a array
```
```ocaml
val fill : 'a array -> pos:int -> len:int -> 'a -> unit
```
```ocaml
val blit : 
  src:'a array ->
  src_pos:int ->
  dst:'a array ->
  dst_pos:int ->
  len:int ->
  unit
```
```ocaml
val to_list : 'a array -> 'a list
```
```ocaml
val of_list : 'a list -> 'a array
```
```ocaml
val compare : cmp:('a -> 'a -> int) -> 'a array -> 'a array -> int
```
```ocaml
val iter : f:('a -> unit) -> 'a array -> unit
```
```ocaml
val iteri : f:(int -> 'a -> unit) -> 'a array -> unit
```
```ocaml
val map : f:('a -> 'b) -> 'a array -> 'b array
```
```ocaml
val map_inplace : f:('a -> 'a) -> 'a array -> unit
```
```ocaml
val mapi : f:(int -> 'a -> 'b) -> 'a array -> 'b array
```
```ocaml
val mapi_inplace : f:(int -> 'a -> 'a) -> 'a array -> unit
```
```ocaml
val fold_left : f:('acc -> 'a -> 'acc) -> init:'acc -> 'a array -> 'acc
```
```ocaml
val fold_left_map : 
  f:('acc -> 'a -> 'acc * 'b) ->
  init:'acc ->
  'a array ->
  'acc * 'b array
```
```ocaml
val fold_right : f:('a -> 'acc -> 'acc) -> 'a array -> init:'acc -> 'acc
```
```ocaml
val iter2 : f:('a -> 'b -> unit) -> 'a array -> 'b array -> unit
```
```ocaml
val map2 : f:('a -> 'b -> 'c) -> 'a array -> 'b array -> 'c array
```
```ocaml
val for_all : f:('a -> bool) -> 'a array -> bool
```
```ocaml
val exists : f:('a -> bool) -> 'a array -> bool
```
```ocaml
val for_all2 : f:('a -> 'b -> bool) -> 'a array -> 'b array -> bool
```
```ocaml
val exists2 : f:('a -> 'b -> bool) -> 'a array -> 'b array -> bool
```
```ocaml
val mem : 'a -> set:'a array -> bool
```
```ocaml
val memq : 'a -> set:'a array -> bool
```
```ocaml
val find_opt : f:('a -> bool) -> 'a array -> 'a option
```
```ocaml
val find_index : f:('a -> bool) -> 'a array -> int option
```
```ocaml
val find_map : f:('a -> 'b option) -> 'a array -> 'b option
```
```ocaml
val find_mapi : f:(int -> 'a -> 'b option) -> 'a array -> 'b option
```
```ocaml
val split : ('a * 'b) array -> 'a array * 'b array
```
```ocaml
val combine : 'a array -> 'b array -> ('a * 'b) array
```
```ocaml
val sort : cmp:('a -> 'a -> int) -> 'a array -> unit
```
```ocaml
val stable_sort : cmp:('a -> 'a -> int) -> 'a array -> unit
```
```ocaml
val fast_sort : cmp:('a -> 'a -> int) -> 'a array -> unit
```
```ocaml
val shuffle : rand:(int -> int) -> 'a array -> unit
```
```ocaml
val to_seq : 'a array -> 'a Stdlib.Seq.t
```
```ocaml
val to_seqi : 'a array -> (int * 'a) Stdlib.Seq.t
```
```ocaml
val of_seq : 'a Stdlib.Seq.t -> 'a array
```
```ocaml
val unsafe_get : 'a array -> int -> 'a
```
```ocaml
val unsafe_set : 'a array -> int -> 'a -> unit
```
```ocaml
module Floatarray : sig ... end
```
```ocaml
val fold_right_i : 'a array -> f:(int -> 'a -> 'b -> 'b) -> init:'b -> 'b
```
```ocaml
val equal : ('a -> 'b -> bool) -> 'a array -> 'b array -> bool
```