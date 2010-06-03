
val is_pure : string -> bool

val mark_used : string -> unit
val list_used : unit -> unit
val get_used : unit -> string list

type kind = [ `Const | `Mutable | `Mutator ]

val kind : string -> kind
val register : string -> kind -> unit

val alias : string -> string -> unit
val resolve : string -> string
