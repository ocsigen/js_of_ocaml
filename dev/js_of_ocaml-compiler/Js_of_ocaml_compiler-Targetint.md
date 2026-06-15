
# Module `Js_of_ocaml_compiler.Targetint`

```ocaml
type t
```
```ocaml
val equal : t -> t -> bool
```
```ocaml
val compare : t -> t -> int
```
```ocaml
val is_zero : t -> bool
```
```ocaml
val to_string : t -> string
```
```ocaml
val to_int_exn : t -> int
```
```ocaml
val to_float : t -> float
```
```ocaml
val to_int32 : t -> int32
```
```ocaml
val of_string_exn : string -> t
```
```ocaml
val of_int_exn : int -> t
```
```ocaml
val of_int32_exn : int32 -> t
```
```ocaml
val of_int32_truncate : int32 -> t
```
```ocaml
val of_int32_warning_on_overflow : int32 -> t
```
```ocaml
val of_nativeint_warning_on_overflow : nativeint -> t
```
```ocaml
val of_int_warning_on_overflow : int -> t
```
```ocaml
val of_float_opt : float -> t option
```
```ocaml
val succ : t -> t
```
```ocaml
val add : t -> t -> t
```
```ocaml
val sub : t -> t -> t
```
```ocaml
val mul : t -> t -> t
```
```ocaml
val div : t -> t -> t
```
```ocaml
val rem : t -> t -> t
```
```ocaml
val logand : t -> t -> t
```
```ocaml
val logor : t -> t -> t
```
```ocaml
val logxor : t -> t -> t
```
```ocaml
val shift_left : t -> int -> t
```
```ocaml
val shift_right : t -> int -> t
```
```ocaml
val shift_right_logical : t -> int -> t
```
```ocaml
val neg : t -> t
```
```ocaml
val abs : t -> t
```
```ocaml
val min_int : unit -> t
```
```ocaml
val max_int : unit -> t
```
```ocaml
val zero : t
```
```ocaml
val one : t
```
```ocaml
val num_bits : unit -> int
```
```ocaml
val set_num_bits : int -> unit
```
```ocaml
val (>=) : t -> t -> bool
```
```ocaml
val (<=) : t -> t -> bool
```
```ocaml
val (<) : t -> t -> bool
```
```ocaml
val (>) : t -> t -> bool
```
```ocaml
val (=) : t -> t -> bool
```
```ocaml
val (<>) : t -> t -> bool
```
```ocaml
val unsigned_lt : t -> t -> bool
```