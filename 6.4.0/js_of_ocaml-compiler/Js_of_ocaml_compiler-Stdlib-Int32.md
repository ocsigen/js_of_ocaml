
# Module `Stdlib.Int32`

```ocaml
val zero : int32
```
```ocaml
val one : int32
```
```ocaml
val minus_one : int32
```
```ocaml
val neg : int32 -> int32
```
```ocaml
val add : int32 -> int32 -> int32
```
```ocaml
val sub : int32 -> int32 -> int32
```
```ocaml
val mul : int32 -> int32 -> int32
```
```ocaml
val div : int32 -> int32 -> int32
```
```ocaml
val unsigned_div : int32 -> int32 -> int32
```
```ocaml
val rem : int32 -> int32 -> int32
```
```ocaml
val unsigned_rem : int32 -> int32 -> int32
```
```ocaml
val succ : int32 -> int32
```
```ocaml
val pred : int32 -> int32
```
```ocaml
val abs : int32 -> int32
```
```ocaml
val max_int : int32
```
```ocaml
val min_int : int32
```
```ocaml
val logand : int32 -> int32 -> int32
```
```ocaml
val logor : int32 -> int32 -> int32
```
```ocaml
val logxor : int32 -> int32 -> int32
```
```ocaml
val lognot : int32 -> int32
```
```ocaml
val shift_left : int32 -> int -> int32
```
```ocaml
val shift_right : int32 -> int -> int32
```
```ocaml
val shift_right_logical : int32 -> int -> int32
```
```ocaml
val of_int : int -> int32
```
```ocaml
val to_int : int32 -> int
```
```ocaml
val unsigned_to_int : int32 -> int option
```
```ocaml
val of_float : float -> int32
```
```ocaml
val to_float : int32 -> float
```
```ocaml
val of_string : string -> int32
```
```ocaml
val of_string_opt : string -> int32 option
```
```ocaml
val to_string : int32 -> string
```
```ocaml
val bits_of_float : float -> int32
```
```ocaml
val float_of_bits : int32 -> float
```
```ocaml
type t = int32
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
val (<) : int32 -> int32 -> bool
```
```ocaml
val (<=) : int32 -> int32 -> bool
```
```ocaml
val (<>) : int32 -> int32 -> bool
```
```ocaml
val (=) : int32 -> int32 -> bool
```
```ocaml
val (>) : int32 -> int32 -> bool
```
```ocaml
val (>=) : int32 -> int32 -> bool
```
```ocaml
val warn_overflow : 
  string ->
  to_dec:('a -> string) ->
  to_hex:('a -> string) ->
  'a ->
  int32 ->
  unit
```
```ocaml
val convert_warning_on_overflow : 
  string ->
  to_int32:('a -> int32) ->
  of_int32:(int32 -> 'b) ->
  equal:('b -> 'a -> bool) ->
  to_dec:('a -> string) ->
  to_hex:('a -> string) ->
  'a ->
  int32
```
```ocaml
val of_nativeint_warning_on_overflow : Stdlib.Nativeint.t -> int32
```