
# Module `Int.Hashtbl`

```ocaml
type key = int
```
```ocaml
type !'a t
```
```ocaml
val create : int -> 'a t
```
```ocaml
val clear : 'a t -> unit
```
```ocaml
val reset : 'a t -> unit
```
```ocaml
val copy : 'a t -> 'a t
```
```ocaml
val add : 'a t -> key -> 'a -> unit
```
```ocaml
val remove : 'a t -> key -> unit
```
```ocaml
val find : 'a t -> key -> 'a
```
```ocaml
val find_opt : 'a t -> key -> 'a option
```
```ocaml
val find_all : 'a t -> key -> 'a list
```
```ocaml
val replace : 'a t -> key -> 'a -> unit
```
```ocaml
val mem : 'a t -> key -> bool
```
```ocaml
val iter : (key -> 'a -> unit) -> 'a t -> unit
```
```ocaml
val filter_map_inplace : (key -> 'a -> 'a option) -> 'a t -> unit
```
```ocaml
val fold : (key -> 'a -> 'acc -> 'acc) -> 'a t -> 'acc -> 'acc
```
```ocaml
val length : 'a t -> int
```
```ocaml
val stats : 'a t -> Stdlib__Hashtbl.statistics
```
```ocaml
val to_seq : 'a t -> (key * 'a) Stdlib.Seq.t
```
```ocaml
val to_seq_keys : 'a t -> key Stdlib.Seq.t
```
```ocaml
val to_seq_values : 'a t -> 'a Stdlib.Seq.t
```
```ocaml
val add_seq : 'a t -> (key * 'a) Stdlib.Seq.t -> unit
```
```ocaml
val replace_seq : 'a t -> (key * 'a) Stdlib.Seq.t -> unit
```
```ocaml
val of_seq : (key * 'a) Stdlib.Seq.t -> 'a t
```