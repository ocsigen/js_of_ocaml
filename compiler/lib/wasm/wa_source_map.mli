type t

val load : ?tmp_buf:Buffer.t -> string -> t

val parse : ?tmp_buf:Buffer.t -> string -> t

val write : string -> t -> unit

val is_empty : t -> bool

type resize_data =
  { mutable i : int
  ; mutable pos : int array
  ; mutable delta : int array
  }

val resize : resize_data -> t -> t

val concatenate : (int * t) list -> t

val iter_sources : t -> (int option -> int option -> string -> unit) -> unit

val insert_source_contents :
     rewrite_path:(string -> string)
  -> t
  -> (int option -> int option -> string -> string option)
  -> t

val blackbox_filename : string

val blackbox_contents : string
