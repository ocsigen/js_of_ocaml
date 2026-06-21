
# Module `Global_name.Compunit_set`

```ocaml
type elt = compunit
```
```ocaml
type t
```
```ocaml
val empty : t
```
```ocaml
val add : elt -> t -> t
```
```ocaml
val singleton : elt -> t
```
```ocaml
val remove : elt -> t -> t
```
```ocaml
val union : t -> t -> t
```
```ocaml
val inter : t -> t -> t
```
```ocaml
val disjoint : t -> t -> bool
```
```ocaml
val diff : t -> t -> t
```
```ocaml
val cardinal : t -> int
```
```ocaml
val elements : t -> elt list
```
```ocaml
val min_elt : t -> elt
```
```ocaml
val min_elt_opt : t -> elt option
```
```ocaml
val max_elt : t -> elt
```
```ocaml
val max_elt_opt : t -> elt option
```
```ocaml
val choose : t -> elt
```
```ocaml
val choose_opt : t -> elt option
```
```ocaml
val find : elt -> t -> elt
```
```ocaml
val find_opt : elt -> t -> elt option
```
```ocaml
val find_first : (elt -> bool) -> t -> elt
```
```ocaml
val find_first_opt : (elt -> bool) -> t -> elt option
```
```ocaml
val find_last : (elt -> bool) -> t -> elt
```
```ocaml
val find_last_opt : (elt -> bool) -> t -> elt option
```
```ocaml
val iter : (elt -> unit) -> t -> unit
```
```ocaml
val fold : (elt -> 'acc -> 'acc) -> t -> 'acc -> 'acc
```
```ocaml
val map : (elt -> elt) -> t -> t
```
```ocaml
val filter : (elt -> bool) -> t -> t
```
```ocaml
val filter_map : (elt -> elt option) -> t -> t
```
```ocaml
val partition : (elt -> bool) -> t -> t * t
```
```ocaml
val split : elt -> t -> t * bool * t
```
```ocaml
val is_empty : t -> bool
```
```ocaml
val mem : elt -> t -> bool
```
```ocaml
val equal : t -> t -> bool
```
```ocaml
val compare : t -> t -> int
```
```ocaml
val subset : t -> t -> bool
```
```ocaml
val for_all : (elt -> bool) -> t -> bool
```
```ocaml
val exists : (elt -> bool) -> t -> bool
```
```ocaml
val to_list : t -> elt list
```
```ocaml
val of_list : elt list -> t
```
```ocaml
val to_seq_from : elt -> t -> elt Stdlib.Seq.t
```
```ocaml
val to_seq : t -> elt Stdlib.Seq.t
```
```ocaml
val to_rev_seq : t -> elt Stdlib.Seq.t
```
```ocaml
val add_seq : elt Stdlib.Seq.t -> t -> t
```
```ocaml
val of_seq : elt Stdlib.Seq.t -> t
```
```ocaml
val compare_cardinal_with : t -> int -> int
```
`compare_cardinal_with s n` is equivalent to `compare (cardinal s) n` but runs in O(min(n, cardinal s)) time instead of O(cardinal s).

```ocaml
val to_list_bounded : int -> t -> elt list option
```
`to_list_bounded n s` returns `Some l` if `s` has at most `n` elements, where `l` is the sorted list of all elements. Returns `None` if `s` has more than `n` elements. Traverses at most `n+1` elements.
