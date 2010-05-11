
type t

val inject : 'a -> t
val extract : t -> 'a

val get : t -> string -> t
val set : t -> string -> t -> unit

val call : t -> t -> t array -> t
val fun_call : t -> t array -> t
val meth_call : t -> string -> t array -> t

(* Object and array literals *)
val obj : (string * t) array -> t
val array : t array -> t

val variable : string -> t

(*
XXX Could we build a camlp4 parser on top of this?

##m(e1,...,en) : <m : 'a1 * ... * 'an -> 'b> t -> 'b
==> explicitly provides the arity
==> typed
*)
