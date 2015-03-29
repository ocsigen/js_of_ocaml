exception Camlp4 of (Location.t * exn)
val loc : exn -> Location.t option

