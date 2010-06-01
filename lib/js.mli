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
type 'a readonly_prop = <get : 'a> gen_prop
type 'a writeonly_prop = <set : 'a> gen_prop
type 'a prop = <get: 'a; set : 'a> gen_prop
type 'a optdef_prop = <get: 'a optdef; set : 'a> gen_prop

(* Constructors *)

type +'a constr

(* Callbacks *)

type +'a callback

(* Javascript objects *)

type +'a t

val _true : bool t
val _false : bool t
val bool : bool -> bool t
val to_bool : bool t -> bool

type js_match_result_handle
type js_string_array

class type js_string = object
  method toString : js_string t meth
  method valueOf : js_string t meth
  method charAt : int -> js_string t meth
  method charCodeAt : int -> float meth        (* This may return NaN... *)
  method concat : js_string t -> js_string t meth
  method concat_2 : js_string t -> js_string t -> js_string t meth
  method concat_3 :
    js_string t -> js_string t -> js_string t -> js_string t meth
  method concat_4 :
    js_string t -> js_string t -> js_string t -> js_string t ->
    js_string t meth
  method indexOf : js_string t -> int meth
  method indexOf_from : js_string t -> int -> int meth
  method lastIndexOf : js_string t -> int meth
  method lastIndexOf_from : js_string t -> int -> int meth
  method localeCompare : js_string t -> float meth
  method _match : js_regexp t -> js_match_result_handle t opt meth
  method replace : js_regexp t -> js_string t -> js_string t
  (* FIX: version of replace taking a function *)
  method replace_string : js_string t -> js_string t -> js_string t
  method search : js_regexp t -> js_match_result_handle t opt meth
  method slice : int -> int -> js_string t meth
  method slice_end : int -> js_string t meth
  method split : js_string t -> js_string_array t meth
  method split_limited : js_string t -> int -> js_string_array t meth
  method substring : int -> int -> js_string t meth
  method substring_to_end : int -> js_string t meth
  method toLowerCase : js_string meth
  method toLocaleLowerCase : js_string meth
  method toUpperCase : js_string meth
  method toLocaleUpperCase : js_string meth
end

and js_regexp = object
  method exec : js_string t -> js_match_result_handle t opt meth
  method test : js_string t -> bool t meth
  method toString : js_string t meth
  method source : js_string t readonly_prop
  method global : bool t readonly_prop
  method ignoreCase : bool t readonly_prop
  method multiline : bool t readonly_prop
  method lastIndex : int prop
end
(* FIX: creating regexps *)

class type ['a] js_array = object
  method toString : js_string t meth
  method toLocaleString : js_string t meth
  method concat : 'a js_array t -> 'a js_array t meth
  method join : js_string t -> js_string t meth
  method pop : 'a optdef meth
  method push : 'a -> int meth
  method push_2 : 'a -> 'a -> int meth
  method push_3 : 'a -> 'a -> 'a -> int meth
  method push_4 : 'a -> 'a -> 'a -> 'a -> int meth
  method reverse : 'a js_array t meth
  method shift : 'a optdef meth
  method slice : int -> int -> 'a js_array t meth
  method slice_end : int -> 'a js_array t meth
  method sort : ('a -> 'a -> float) callback -> 'a js_array t meth
  method sort_asStrings : 'a js_array t meth
  method splice : int -> int -> 'a js_array t meth
  method splice_1 : int -> int -> 'a -> 'a js_array t meth
  method splice_2 : int -> int -> 'a -> 'a -> 'a js_array t meth
  method splice_3 : int -> int -> 'a -> 'a -> 'a -> 'a js_array t meth
  method splice_4 : int -> int -> 'a -> 'a -> 'a -> 'a -> 'a js_array t meth
  method unshift : 'a -> int meth
  method unshift_2 : 'a -> 'a -> int meth
  method unshift_3 : 'a -> 'a -> 'a -> int meth
  method unshift_4 : 'a -> 'a -> 'a -> 'a -> int meth
  method length : int prop
end

(*FIX: array access :
val array_set : 'a #js_array t -> int -> 'a optdef
val array_get : 'a #js_array t -> int -> 'a -> unit
*)

class type js_match_result = object
  inherit [js_string t] js_array
  method index : int
  method input : js_string t
end

val str_array : js_string_array t -> js_string t js_array t
val match_result : js_match_result_handle t -> js_match_result t

val float : float -> float t
val to_float : float t -> float

val string : string -> js_string t
val to_string : js_string t -> string

(* Unsafe operations.  Use with care! *)

module Unsafe : sig
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
