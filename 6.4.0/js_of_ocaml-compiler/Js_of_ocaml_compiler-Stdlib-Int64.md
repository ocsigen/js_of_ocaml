
# Module `Stdlib.Int64`

```ocaml
val zero : int64
```
```ocaml
val one : int64
```
```ocaml
val minus_one : int64
```
```ocaml
val neg : int64 -> int64
```
```ocaml
val add : int64 -> int64 -> int64
```
```ocaml
val sub : int64 -> int64 -> int64
```
```ocaml
val mul : int64 -> int64 -> int64
```
```ocaml
val div : int64 -> int64 -> int64
```
```ocaml
val unsigned_div : int64 -> int64 -> int64
```
```ocaml
val rem : int64 -> int64 -> int64
```
```ocaml
val unsigned_rem : int64 -> int64 -> int64
```
```ocaml
val succ : int64 -> int64
```
```ocaml
val pred : int64 -> int64
```
```ocaml
val abs : int64 -> int64
```
```ocaml
val max_int : int64
```
```ocaml
val min_int : int64
```
```ocaml
val logand : int64 -> int64 -> int64
```
```ocaml
val logor : int64 -> int64 -> int64
```
```ocaml
val logxor : int64 -> int64 -> int64
```
```ocaml
val lognot : int64 -> int64
```
```ocaml
val shift_left : int64 -> int -> int64
```
```ocaml
val shift_right : int64 -> int -> int64
```
```ocaml
val shift_right_logical : int64 -> int -> int64
```
```ocaml
val of_int : int -> int64
```
```ocaml
val to_int : int64 -> int
```
```ocaml
val unsigned_to_int : int64 -> int option
```
```ocaml
val of_float : float -> int64
```
```ocaml
val to_float : int64 -> float
```
```ocaml
val of_int32 : int32 -> int64
```
```ocaml
val to_int32 : int64 -> int32
```
```ocaml
val of_nativeint : nativeint -> int64
```
```ocaml
val to_nativeint : int64 -> nativeint
```
```ocaml
val of_string : string -> int64
```
```ocaml
val of_string_opt : string -> int64 option
```
```ocaml
val to_string : int64 -> string
```
```ocaml
val bits_of_float : float -> int64
```
```ocaml
val float_of_bits : int64 -> float
```
```ocaml
type t = int64
```
```ocaml
val compare : t -> t -> int
```
```ocaml
val unsigned_compare : t -> t -> int
```
```ocaml
val equal : t -> t -> bool
```
```ocaml
val min : t -> t -> t
```
```ocaml
val max : t -> t -> t
```
```ocaml
val seeded_hash : int -> t -> int
```
```ocaml
val hash : t -> int
```
```ocaml
val (<) : int64 -> int64 -> bool
```
```ocaml
val (<=) : int64 -> int64 -> bool
```
```ocaml
val (<>) : int64 -> int64 -> bool
```
```ocaml
val (=) : int64 -> int64 -> bool
```
```ocaml
val (>) : int64 -> int64 -> bool
```
```ocaml
val (>=) : int64 -> int64 -> bool
```