
module IntSet : Set.S with type elt = int
module IntMap : Map.S with type key = int

val opt_filter : ('a -> bool) -> 'a option -> 'a option
val opt_map : ('a -> 'b) -> 'a option -> 'b option
val opt_iter : ('a -> unit) -> 'a option -> unit
