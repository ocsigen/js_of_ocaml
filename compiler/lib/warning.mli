type t =
  [ (* Parsing bytecode *)
    `Integer_overflow
  | `Missing_debug_event
  | `Missing_cmi
  | `Effect_handlers_without_effect_backend
  | (* runtime *)
    `Missing_primitive
  | `Missing_define
  | `Missing_deps
  | `Free_variables_in_primitive
  | `Deprecated_joo_global_object
  | `Overriding_primitive
  | `Overriding_primitive_purity
  | `Deprecated_primitive
  | `Unused_js_variable
  ]

val all : t list

val name : t -> string

val parse : string -> t option

val enable : t -> unit

val disable : t -> unit

val enabled : t -> bool

val quiet : bool ref

val werror : bool ref

val warn : t -> ('a, Format.formatter, unit, unit) format4 -> 'a

val process_warnings : unit -> unit
