
type t

(*FIX: use 'val' rather than 'external' when inlining is implemented... *)

external inject : 'a -> t = "%identity"
external extract : t -> 'a = "%identity"

val null : t

external get : t -> string -> t = "caml_js_get"
external set : t -> string -> t -> unit = "caml_js_set"

external call : t -> t -> t array -> t = "caml_js_call"
external fun_call : t -> t array -> t = "caml_js_fun_call"
external meth_call : t -> string -> t array -> t = "caml_js_meth_call"
external new_obj : t -> t array -> t = "caml_js_new"

(* Object and array literals *)
external obj : (string * t) array -> t = "caml_js_obj"
external array_lit : t array -> t = "caml_js_array"

external variable : string -> t = "caml_js_var"

(*
XXX Could we build a camlp4 parser on top of this?

##m(e1,...,en) : <m : 'a1 * ... * 'an -> 'b> t -> 'b
==> explicitly provides the arity
==> typed
*)

(****)

module Obj : sig
type +'a t
type +'a readonly_prop
type 'a prop = private 'a readonly_prop
type +'a meth
(*type 'a nullable_prop = 'a Nullable.t prop*)

external unsafe_get : 'a t -> string -> 'b = "caml_js_get"
external unsafe_set : 'a t -> string -> 'b -> unit = "caml_js_set"
external unsafe_meth_call : 'a t -> string -> 'b array -> 'c = "caml_js_meth_call"
external unsafe_inject : 'a -> unit = "%identity"
end

(****)

type string
type 'a array
type bool

external string : string -> t = "%identity"
external array : 'a array -> t = "%identity"
val _true : bool
val _false : bool
