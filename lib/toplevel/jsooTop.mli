open Format
val use : formatter -> string -> bool
val execute : bool -> ?pp_code:formatter -> formatter -> string -> unit
val initialize : unit -> unit

val get_camlp4_syntaxes : unit -> string list
val register_camlp4_syntax : string ->
  (((string * (unit -> unit)) -> unit) -> unit)-> unit
