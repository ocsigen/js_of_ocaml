
# Module `Stdlib.Float`

```ocaml
val zero : float
```
```ocaml
val one : float
```
```ocaml
val minus_one : float
```
```ocaml
val neg : float -> float
```
```ocaml
val add : float -> float -> float
```
```ocaml
val sub : float -> float -> float
```
```ocaml
val mul : float -> float -> float
```
```ocaml
val div : float -> float -> float
```
```ocaml
val fma : float -> float -> float -> float
```
```ocaml
val rem : float -> float -> float
```
```ocaml
val succ : float -> float
```
```ocaml
val pred : float -> float
```
```ocaml
val abs : float -> float
```
```ocaml
val infinity : float
```
```ocaml
val neg_infinity : float
```
```ocaml
val nan : float
```
```ocaml
val signaling_nan : float
```
```ocaml
val quiet_nan : float
```
```ocaml
val pi : float
```
```ocaml
val max_float : float
```
```ocaml
val min_float : float
```
```ocaml
val epsilon : float
```
```ocaml
val is_finite : float -> bool
```
```ocaml
val is_infinite : float -> bool
```
```ocaml
val is_nan : float -> bool
```
```ocaml
val is_integer : float -> bool
```
```ocaml
val of_int : int -> float
```
```ocaml
val to_int : float -> int
```
```ocaml
val of_string : string -> float
```
```ocaml
val of_string_opt : string -> float option
```
```ocaml
val to_string : float -> string
```
```ocaml
type fpclass = Stdlib.fpclass = 
  | FP_normal
  | FP_subnormal
  | FP_zero
  | FP_infinite
  | FP_nan
```
```ocaml
val classify_float : float -> fpclass
```
```ocaml
val pow : float -> float -> float
```
```ocaml
val sqrt : float -> float
```
```ocaml
val cbrt : float -> float
```
```ocaml
val exp : float -> float
```
```ocaml
val exp2 : float -> float
```
```ocaml
val log : float -> float
```
```ocaml
val log10 : float -> float
```
```ocaml
val log2 : float -> float
```
```ocaml
val expm1 : float -> float
```
```ocaml
val log1p : float -> float
```
```ocaml
val cos : float -> float
```
```ocaml
val sin : float -> float
```
```ocaml
val tan : float -> float
```
```ocaml
val acos : float -> float
```
```ocaml
val asin : float -> float
```
```ocaml
val atan : float -> float
```
```ocaml
val atan2 : float -> float -> float
```
```ocaml
val hypot : float -> float -> float
```
```ocaml
val cosh : float -> float
```
```ocaml
val sinh : float -> float
```
```ocaml
val tanh : float -> float
```
```ocaml
val acosh : float -> float
```
```ocaml
val asinh : float -> float
```
```ocaml
val atanh : float -> float
```
```ocaml
val erf : float -> float
```
```ocaml
val erfc : float -> float
```
```ocaml
val trunc : float -> float
```
```ocaml
val round : float -> float
```
```ocaml
val ceil : float -> float
```
```ocaml
val floor : float -> float
```
```ocaml
val next_after : float -> float -> float
```
```ocaml
val copy_sign : float -> float -> float
```
```ocaml
val sign_bit : float -> bool
```
```ocaml
val frexp : float -> float * int
```
```ocaml
val ldexp : float -> int -> float
```
```ocaml
val modf : float -> float * float
```
```ocaml
type t = float
```
```ocaml
val compare : t -> t -> int
```
```ocaml
val min : t -> t -> t
```
```ocaml
val max : float -> float -> float
```
```ocaml
val min_max : float -> float -> float * float
```
```ocaml
val min_num : t -> t -> t
```
```ocaml
val max_num : t -> t -> t
```
```ocaml
val min_max_num : float -> float -> float * float
```
```ocaml
val seeded_hash : int -> t -> int
```
```ocaml
val hash : t -> int
```
```ocaml
module Array : sig ... end
```
```ocaml
module ArrayLabels : sig ... end
```
```ocaml
val equal : float -> float -> [> `Use_ieee_equal_or_bitwise_equal ]
```
```ocaml
val ieee_equal : float -> float -> bool
```
```ocaml
val bitwise_equal : float -> float -> bool
```
```ocaml
val (<) : t -> t -> bool
```
```ocaml
val (<=) : t -> t -> bool
```
```ocaml
val (<>) : t -> t -> bool
```
```ocaml
val (=) : t -> t -> bool
```
```ocaml
val (>) : t -> t -> bool
```
```ocaml
val (>=) : t -> t -> bool
```