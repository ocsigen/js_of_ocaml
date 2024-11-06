type t

val is_empty : Source_map.t -> bool

type resize_data =
  { mutable i : int
  ; mutable pos : int array
  ; mutable delta : int array
  }

val resize : resize_data -> Source_map.t -> Source_map.t

val concatenate : (int * Source_map.t) list -> Source_map.t

val iter_sources : Source_map.t -> (int option -> int option -> string -> unit) -> unit

val insert_source_contents :
     rewrite_path:(string -> string)
  -> Source_map.t
  -> (int option -> int option -> string -> string option)
  -> Source_map.t

val blackbox_filename : string

val blackbox_contents : string
