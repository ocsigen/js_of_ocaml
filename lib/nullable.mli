
type +'a t

val null : 'a t
val some : 'a -> 'a t
val maybe : 'a t -> 'a option
