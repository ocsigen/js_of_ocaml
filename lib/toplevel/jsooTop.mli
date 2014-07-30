open Format
val use : formatter -> string -> bool
val execute : bool -> ?pp_code:formatter -> formatter -> string -> unit
val initialize : unit -> unit
