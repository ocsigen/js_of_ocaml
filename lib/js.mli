
type +'a t

(* Javascript booleans *)
val _true : bool t
val _false : bool t

val float : float -> float t
val to_float : float t -> float

val string : string -> string t
val to_string : string t -> string

(* Null and undefined *)

type +'a opt
type +'a optdef

val null : 'a opt
val some : 'a -> 'a opt
val undefined : 'a optdef
val def : 'a -> 'a optdef

module type OPT = sig
  type 'a t
  val ret : 'a -> 'a t
  val map : 'a t -> ('a -> 'b) -> 'b t
  val bind : 'a t -> ('a -> 'b t) -> 'b t
  val case : 'a t -> 'b -> ('a -> 'b) -> 'b
  val get : 'a t -> (unit -> 'a) -> 'a
  val iter : 'a t -> ('a -> unit) -> unit
end

module Opt : OPT with type 'a t = 'a opt
module Optdef : OPT with type 'a t = 'a optdef

(* Method and object properties specification *)

type +'a meth
type +'a gen_prop
type 'a readonly_prop = <read : 'a> gen_prop
type 'a writeonly_prop = <write : 'a> gen_prop
type 'a prop = <read: 'a; write : 'a> gen_prop
type 'a optdef_prop = <read: 'a optdef; write : 'a> gen_prop

(* Constructors *)

type +'a constr

(* Unsafe operations.  Use with care! *)

module Unsafe : sig
(*FIX: use 'val' rather than 'external' when inlining is implemented... *)
  external variable : string -> 'a = "caml_js_var"

  type any
  external inject : 'a -> any = "%identity"
  external extract : any -> 'a = "%identity"
  external coerce : < .. > t -> < ..> t = "%identity"

  external get : 'a -> string -> 'b = "caml_js_get"
  external set : 'a -> string -> 'b -> unit = "caml_js_set"
  external meth_call : 'a -> string -> any array -> 'c = "caml_js_meth_call"

(*FIX also, object/array literals; array/hash access
  external call : t -> t -> t array -> t = "caml_js_call"
  external fun_call : t -> t array -> t = "caml_js_fun_call"
  external new_obj : t -> t array -> t = "caml_js_new"
*)

end
