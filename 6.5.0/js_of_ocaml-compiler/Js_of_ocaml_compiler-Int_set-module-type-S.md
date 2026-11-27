# Module type `Int_set.S`

```ocaml
type elt
```
```ocaml
type t
```
```ocaml
val empty : t
```
```ocaml
val is_empty : t -> bool
```
```ocaml
val mem : elt -> t -> bool
```
```ocaml
val add : elt -> t -> t
```
Returns the set itself when the element is already present.

```ocaml
val singleton : elt -> t
```
```ocaml
val remove : elt -> t -> t
```
Returns the set itself when the element is not present.

```ocaml
val union : t -> t -> t
```
```ocaml
val inter : t -> t -> t
```
```ocaml
val diff : t -> t -> t
```
```ocaml
val equal : t -> t -> bool
```
```ocaml
val compare : t -> t -> int
```
A total order, compatible with `equal`.

```ocaml
val subset : t -> t -> bool
```
```ocaml
val disjoint : t -> t -> bool
```
```ocaml
val iter : (elt -> unit) -> t -> unit
```
```ocaml
val fold : (elt -> 'a -> 'a) -> t -> 'a -> 'a
```
```ocaml
val for_all : (elt -> bool) -> t -> bool
```
```ocaml
val exists : (elt -> bool) -> t -> bool
```
```ocaml
val filter : (elt -> bool) -> t -> t
```
```ocaml
val map : (elt -> elt) -> t -> t
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
val max_elt : t -> elt
```
```ocaml
val choose : t -> elt
```
Returns the smallest element.

```ocaml
val of_list : elt list -> t
```
```ocaml
val to_seq : t -> elt Stdlib.Seq.t
```
```ocaml
val compare_cardinal_with : t -> int -> int
```
`compare_cardinal_with s n` is `compare (cardinal s) n`.

```ocaml
val to_list_bounded : int -> t -> elt list option
```
`to_list_bounded n s` returns `Some (elements s)` if `s` has at most `n` elements, and `None` otherwise.
