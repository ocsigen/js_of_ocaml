type t

val equal : t -> t -> bool

val compare : t -> t -> int

val is_zero : t -> bool

(* to *)

val to_string : t -> string

val to_int_exn : t -> int

val to_float : t -> float

val to_int32 : t -> int32

(* of *)

val of_string_exn : string -> t

val of_int_exn : int -> t

val of_int32_exn : int32 -> t

val of_int32_truncate : int32 -> t

val of_int32_warning_on_overflow : int32 -> t

val of_nativeint_warning_on_overflow : nativeint -> t

val of_int_warning_on_overflow : int -> t

val of_float_opt : float -> t option

(* arithmetic *)

val succ : t -> t

val add : t -> t -> t

val sub : t -> t -> t

val mul : t -> t -> t

val div : t -> t -> t

val rem : t -> t -> t

val logand : t -> t -> t

val logor : t -> t -> t

val logxor : t -> t -> t

val shift_left : t -> int -> t

val shift_right : t -> int -> t

val shift_right_logical : t -> int -> t

val neg : t -> t

val abs : t -> t

(* constant *)

val min_int : unit -> t

val max_int : unit -> t

val zero : t

val one : t

(* num bits *)

val num_bits : unit -> int

val set_num_bits : int -> unit

(* comparison *)

val ( >= ) : t -> t -> bool

val ( <= ) : t -> t -> bool

val ( < ) : t -> t -> bool

val ( > ) : t -> t -> bool

val ( = ) : t -> t -> bool

val ( <> ) : t -> t -> bool

val unsigned_lt : t -> t -> bool
