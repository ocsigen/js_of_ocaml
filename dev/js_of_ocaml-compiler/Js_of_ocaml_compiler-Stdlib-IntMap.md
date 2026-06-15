
# Module `Stdlib.IntMap`

```ocaml
type key = Int.t
```
```ocaml
type !'a t = 'a Stdlib__Map.Make(Int).t
```
```ocaml
val empty : 'a t
```
```ocaml
val add : key -> 'a -> 'a t -> 'a t
```
```ocaml
val add_to_list : key -> 'a -> 'a list t -> 'a list t
```
```ocaml
val update : key -> ('a option -> 'a option) -> 'a t -> 'a t
```
```ocaml
val singleton : key -> 'a -> 'a t
```
```ocaml
val remove : key -> 'a t -> 'a t
```
```ocaml
val merge : 
  (key -> 'a option -> 'b option -> 'c option) ->
  'a t ->
  'b t ->
  'c t
```
```ocaml
val union : (key -> 'a -> 'a -> 'a option) -> 'a t -> 'a t -> 'a t
```
```ocaml
val cardinal : 'a t -> int
```
```ocaml
val bindings : 'a t -> (key * 'a) list
```
```ocaml
val min_binding : 'a t -> key * 'a
```
```ocaml
val min_binding_opt : 'a t -> (key * 'a) option
```
```ocaml
val max_binding : 'a t -> key * 'a
```
```ocaml
val max_binding_opt : 'a t -> (key * 'a) option
```
```ocaml
val choose : 'a t -> key * 'a
```
```ocaml
val choose_opt : 'a t -> (key * 'a) option
```
```ocaml
val find : key -> 'a t -> 'a
```
```ocaml
val find_opt : key -> 'a t -> 'a option
```
```ocaml
val find_first : (key -> bool) -> 'a t -> key * 'a
```
```ocaml
val find_first_opt : (key -> bool) -> 'a t -> (key * 'a) option
```
```ocaml
val find_last : (key -> bool) -> 'a t -> key * 'a
```
```ocaml
val find_last_opt : (key -> bool) -> 'a t -> (key * 'a) option
```
```ocaml
val iter : (key -> 'a -> unit) -> 'a t -> unit
```
```ocaml
val fold : (key -> 'a -> 'acc -> 'acc) -> 'a t -> 'acc -> 'acc
```
```ocaml
val map : ('a -> 'b) -> 'a t -> 'b t
```
```ocaml
val mapi : (key -> 'a -> 'b) -> 'a t -> 'b t
```
```ocaml
val filter : (key -> 'a -> bool) -> 'a t -> 'a t
```
```ocaml
val filter_map : (key -> 'a -> 'b option) -> 'a t -> 'b t
```
```ocaml
val partition : (key -> 'a -> bool) -> 'a t -> 'a t * 'a t
```
```ocaml
val split : key -> 'a t -> 'a t * 'a option * 'a t
```
```ocaml
val is_empty : 'a t -> bool
```
```ocaml
val mem : key -> 'a t -> bool
```
```ocaml
val equal : ('a -> 'a -> bool) -> 'a t -> 'a t -> bool
```
```ocaml
val compare : ('a -> 'a -> int) -> 'a t -> 'a t -> int
```
```ocaml
val for_all : (key -> 'a -> bool) -> 'a t -> bool
```
```ocaml
val exists : (key -> 'a -> bool) -> 'a t -> bool
```
```ocaml
val to_list : 'a t -> (key * 'a) list
```
```ocaml
val of_list : (key * 'a) list -> 'a t
```
```ocaml
val to_seq : 'a t -> (key * 'a) Stdlib.Seq.t
```
```ocaml
val to_rev_seq : 'a t -> (key * 'a) Stdlib.Seq.t
```
```ocaml
val to_seq_from : key -> 'a t -> (key * 'a) Stdlib.Seq.t
```
```ocaml
val add_seq : (key * 'a) Stdlib.Seq.t -> 'a t -> 'a t
```
```ocaml
val of_seq : (key * 'a) Stdlib.Seq.t -> 'a t
```