
# Module `Stdlib.BitSet`

```ocaml
type t
```
```ocaml
val create : unit -> t
```
```ocaml
val create' : int -> t
```
```ocaml
val mem : t -> int -> bool
```
```ocaml
val set : t -> int -> unit
```
```ocaml
val unset : t -> int -> unit
```
```ocaml
val copy : t -> t
```
```ocaml
val iter : f:(int -> unit) -> t -> unit
```
```ocaml
val size : t -> int
```
```ocaml
val next_free : t -> int -> int
```
```ocaml
val next_mem : t -> int -> int
```
```ocaml
val clear : t -> unit
```