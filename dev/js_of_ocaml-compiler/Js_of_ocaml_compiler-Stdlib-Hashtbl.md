
# Module `Stdlib.Hashtbl`

```ocaml
type (!'a, !'b) t = ('a, 'b) Stdlib__Hashtbl.t
```
```ocaml
val clear : ('a, 'b) t -> unit
```
```ocaml
val reset : ('a, 'b) t -> unit
```
```ocaml
val copy : ('a, 'b) t -> ('a, 'b) t
```
```ocaml
val add : ('a, 'b) t -> 'a -> 'b -> unit
```
```ocaml
val find : ('a, 'b) t -> 'a -> 'b
```
```ocaml
val find_opt : ('a, 'b) t -> 'a -> 'b option
```
```ocaml
val find_all : ('a, 'b) t -> 'a -> 'b list
```
```ocaml
val mem : ('a, 'b) t -> 'a -> bool
```
```ocaml
val remove : ('a, 'b) t -> 'a -> unit
```
```ocaml
val replace : ('a, 'b) t -> 'a -> 'b -> unit
```
```ocaml
val iter : ('a -> 'b -> unit) -> ('a, 'b) t -> unit
```
```ocaml
val filter_map_inplace : ('a -> 'b -> 'b option) -> ('a, 'b) t -> unit
```
```ocaml
val fold : ('a -> 'b -> 'acc -> 'acc) -> ('a, 'b) t -> 'acc -> 'acc
```
```ocaml
val length : ('a, 'b) t -> int
```
```ocaml
val randomize : unit -> unit
```
```ocaml
val is_randomized : unit -> bool
```
```ocaml
val rebuild : ?random:bool -> ('a, 'b) t -> ('a, 'b) t
```
```ocaml
type statistics = Stdlib__Hashtbl.statistics = {
  num_bindings : int;
  num_buckets : int;
  max_bucket_length : int;
  bucket_histogram : int array;
}
```
```ocaml
val stats : ('a, 'b) t -> statistics
```
```ocaml
val to_seq : ('a, 'b) t -> ('a * 'b) Stdlib.Seq.t
```
```ocaml
val to_seq_keys : ('a, 'b) t -> 'a Stdlib.Seq.t
```
```ocaml
val to_seq_values : ('a, 'b) t -> 'b Stdlib.Seq.t
```
```ocaml
val add_seq : ('a, 'b) t -> ('a * 'b) Stdlib.Seq.t -> unit
```
```ocaml
val replace_seq : ('a, 'b) t -> ('a * 'b) Stdlib.Seq.t -> unit
```
```ocaml
module type HashedType = sig ... end
```
```ocaml
module type S = sig ... end
```
```ocaml
module Make : sig ... end
```
```ocaml
module type SeededHashedType = sig ... end
```
```ocaml
module type SeededS = sig ... end
```
```ocaml
module MakeSeeded : sig ... end
```
```ocaml
val hash : 'a -> int
```
```ocaml
val seeded_hash : int -> 'a -> int
```
```ocaml
val hash_param : int -> int -> 'a -> int
```
```ocaml
val seeded_hash_param : int -> int -> int -> 'a -> int
```
```ocaml
val create : ?random:bool -> int -> ('a, 'b) Stdlib.Hashtbl.t
```
```ocaml
val of_seq : ('a * 'b) Stdlib.Seq.t -> ('a, 'b) Stdlib.Hashtbl.t
```