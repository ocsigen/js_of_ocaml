# Module `Js_of_ocaml_compiler.Int_trie`

Persistent maps indexed by non-negative integers.

The map is a 32-way trie indexed by the bits of the key. A lookup only follows a few pointers (four levels are enough for a million keys), and the bindings of consecutive keys are stored next to each other. This is much faster than a balanced tree when the keys are dense, as the addresses of the blocks of a program.

The interface is a subset of `Map.S`, with the same semantics. In particular, the bindings are always visited in increasing key order. The only difference is that the keys must be non-negative: `add` and `update` raise `Invalid_argument` on a negative key.

```ocaml
type key = int
```
```ocaml
type +'a t
```
```ocaml
val empty : 'a t
```
```ocaml
val singleton : key -> 'a -> 'a t
```
```ocaml
val add : key -> 'a -> 'a t -> 'a t
```
Returns the map itself when the key is already bound to the same (physically equal) value.

```ocaml
val remove : key -> 'a t -> 'a t
```
```ocaml
val update : key -> ('a option -> 'a option) -> 'a t -> 'a t
```
```ocaml
val find : key -> 'a t -> 'a
```
```ocaml
val find_opt : key -> 'a t -> 'a option
```
```ocaml
val mem : key -> 'a t -> bool
```
```ocaml
val cardinal : 'a t -> int
```
```ocaml
val is_empty : 'a t -> bool
```
```ocaml
val choose : 'a t -> key * 'a
```
Returns the binding with the smallest key.

```ocaml
val min_binding : 'a t -> key * 'a
```
```ocaml
val max_binding : 'a t -> key * 'a
```
```ocaml
val iter : (key -> 'a -> unit) -> 'a t -> unit
```
```ocaml
val fold : (key -> 'a -> 'b -> 'b) -> 'a t -> 'b -> 'b
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
val equal : ('a -> 'a -> bool) -> 'a t -> 'a t -> bool
```
```ocaml
val bindings : 'a t -> (key * 'a) list
```
```ocaml
val to_seq : 'a t -> (key * 'a) Stdlib.Seq.t
```
```ocaml
val to_rev_seq : 'a t -> (key * 'a) Stdlib.Seq.t
```
```ocaml
val of_seq : (key * 'a) Stdlib.Seq.t -> 'a t
```
Builds a map in one go, which is much cheaper than repeatedly calling `add`. Later bindings take precedence.
