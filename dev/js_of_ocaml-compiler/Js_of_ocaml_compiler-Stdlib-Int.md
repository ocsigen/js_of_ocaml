
# Module `Stdlib.Int`

```ocaml
type t = int
```
```ocaml
val zero : int
```
```ocaml
val one : int
```
```ocaml
val minus_one : int
```
```ocaml
val neg : int -> int
```
```ocaml
val add : int -> int -> int
```
```ocaml
val sub : int -> int -> int
```
```ocaml
val mul : int -> int -> int
```
```ocaml
val div : int -> int -> int
```
```ocaml
val rem : int -> int -> int
```
```ocaml
val succ : int -> int
```
```ocaml
val pred : int -> int
```
```ocaml
val abs : int -> int
```
```ocaml
val max_int : int
```
```ocaml
val min_int : int
```
```ocaml
val logand : int -> int -> int
```
```ocaml
val logor : int -> int -> int
```
```ocaml
val logxor : int -> int -> int
```
```ocaml
val lognot : int -> int
```
```ocaml
val shift_left : int -> int -> int
```
```ocaml
val shift_right : int -> int -> int
```
```ocaml
val shift_right_logical : int -> int -> int
```
```ocaml
val equal : int -> int -> bool
```
```ocaml
val compare : int -> int -> int
```
```ocaml
val min : int -> int -> int
```
```ocaml
val max : int -> int -> int
```
```ocaml
val to_float : int -> float
```
```ocaml
val of_float : float -> int
```
```ocaml
val to_string : int -> string
```
```ocaml
val seeded_hash : int -> int -> int
```
```ocaml
val hash : t -> t
```
```ocaml
module Hashtbl : sig ... end
```