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

type +'a t
type +'a meth
type +'a gen_prop
type 'a readonly_prop = <get : 'a> gen_prop
type 'a writeonly_prop = <set : 'a> gen_prop
type 'a prop = <get : 'a; set : 'a> gen_prop
type 'a optdef_prop = <get : 'a optdef; set : 'a> gen_prop
type float_prop = <get : float t; set : float> gen_prop

(* Constructors *)

type +'a constr

(* Callbacks *)

type (+'a, +'b) meth_callback
type 'a callback = (unit, 'a) meth_callback

external wrap_callback : ('a -> 'b) -> ('c, 'a -> 'b) meth_callback =
    "caml_js_wrap_callback"
external wrap_meth_callback :
  ('c -> 'a -> 'b) -> ('c, 'a -> 'b) meth_callback =
  "caml_js_wrap_meth_callback"

(* Javascript objects *)

val _true : bool t
val _false : bool t

type match_result_handle (* Used to resolved the mutual dependency *)
type string_array        (* between strings and arrays *)

class type js_string = object
  method toString : js_string t meth
  method valueOf : js_string t meth
  method charAt : int -> js_string t meth
  method charCodeAt : int -> float t meth        (* This may return NaN... *)
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
  method localeCompare : js_string t -> float t meth
  method _match : regExp t -> match_result_handle t opt meth
  method replace : regExp t -> js_string t -> js_string t
  (* FIX: version of replace taking a function... *)
  method replace_string : js_string t -> js_string t -> js_string t
  method search : regExp t -> match_result_handle t opt meth
  method slice : int -> int -> js_string t meth
  method slice_end : int -> js_string t meth
  method split : js_string t -> string_array t meth
  method split_limited : js_string t -> int -> string_array t meth
  method substring : int -> int -> js_string t meth
  method substring_to_end : int -> js_string t meth
  method toLowerCase : js_string t meth
  method toLocaleLowerCase : js_string t meth
  method toUpperCase : js_string t meth
  method toLocaleUpperCase : js_string t meth
end

and regExp = object
  method exec : js_string t -> match_result_handle t opt meth
  method test : js_string t -> bool t meth
  method toString : js_string t meth
  method source : js_string t readonly_prop
  method global : bool t readonly_prop
  method ignoreCase : bool t readonly_prop
  method multiline : bool t readonly_prop
  method lastIndex : int prop
end

val regExp : (js_string t -> regExp t) constr
val regExp_withFlags : (js_string t -> js_string t -> regExp t) constr
val regExp_copy : (regExp t -> regExp t) constr

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

val array_empty : 'a js_array t constr
val array_length : (int -> 'a js_array t) constr

val array_get : 'a #js_array t -> int -> 'a optdef
val array_set : 'a #js_array t -> int -> 'a -> unit

class type match_result = object
  inherit [js_string t] js_array
  method index : int
  method input : js_string t
end

val str_array : string_array t -> js_string t js_array t
val match_result : match_result_handle t -> match_result t

class type number = object
  method toString : js_string t meth
  method toString_radix : int -> js_string t meth
  method toLocaleString : js_string t meth
  method toFixed : int -> js_string t meth
  method toExponential : js_string t meth
  method toExponential_digits : int -> js_string t meth
  method toPrecision : int -> js_string meth t
end

external number_of_float : float -> number t = "caml_js_from_float"
external float_of_number : number t -> float = "caml_js_to_float"

class type date = object
  method toString : js_string t meth
  method toDateString : js_string t meth
  method toTimeString : js_string t meth
  method toLocaleString : js_string t meth
  method toLocaleDateString : js_string t meth
  method toLocaleTimeString : js_string t meth
  method valueOf : float t meth
  method getTime : float t meth
  method getFullYear : int meth
  method getUTCFullYear : int meth
  method getMonth : int meth
  method getUTCMonth : int meth
  method getDate : int meth
  method getUTCDate : int meth
  method getDay : int meth
  method getUTCDay : int meth
  method getHours : int meth
  method getUTCHours : int meth
  method getMinutes : int meth
  method getUTCMinutes : int meth
  method getSeconds : int meth
  method getUTCSeconds : int meth
  method getMilliseconds : int meth
  method getUTCMilliseconds : int meth
  method getTimezoneOffset : int meth
  method setTime : float -> float t meth
  method setFullYear : int -> float t meth
  method setUTCFullYear : int -> float t meth
  method setMonth : int -> float t meth
  method setUTCMonth : int -> float t meth
  method setDate : int -> float t meth
  method setUTCDate : int -> float t meth
  method setDay : int -> float t meth
  method setUTCDay : int -> float t meth
  method setHours : int -> float t meth
  method setUTCHours : int -> float t meth
  method setMinutes : int -> float t meth
  method setUTCMinutes : int -> float t meth
  method setSeconds : int -> float t meth
  method setUTCSeconds : int -> float t meth
  method setMilliseconds : int -> float t meth
  method setUTCMilliseconds : int -> float t meth
  method toUTCString : js_string t meth
  method toISOString : js_string t meth
  method toJSON : 'a -> js_string t meth
end

val date_now : date t constr
val date_fromTimeValue : (float -> date t) constr
val date_month : (int -> int -> date t) constr
val date_day : (int -> int -> int -> date t) constr
val date_hour : (int -> int -> int -> int -> date t) constr
val date_min : (int -> int -> int -> int -> int -> date t) constr
val date_sec : (int -> int -> int -> int -> int -> int -> date t) constr
val date_ms : (int -> int -> int -> int -> int -> int -> int -> date t) constr

class type date_constr = object
  method parse : js_string t -> float t meth
  method _UTC_month : int -> int -> float t meth
  method _UTC_day : int -> int -> float t meth
  method _UTC_hour : int -> int -> int -> int -> float t meth
  method _UTC_min : int -> int -> int -> int -> int -> float t meth
  method _UTC_sec : int -> int -> int -> int -> int -> int -> float t meth
  method _UTC_ms :
    int -> int -> int -> int -> int -> int -> int -> float t meth
  method now : float t meth
end

val date : date_constr t

(* Conversion functions *)

external bool : bool -> bool t = "caml_js_from_bool"
external to_bool : bool t -> bool = "caml_js_to_bool"
external string : string -> js_string t = "caml_js_from_string"
external to_string : js_string t -> string = "caml_js_to_string"
external float : float -> float t = "caml_js_from_float"
external to_float : float t -> float = "caml_js_to_float"
external array : 'a array -> 'a js_array t = "caml_js_from_array"
external to_array : 'a js_array t -> 'a array = "caml_js_to_array"

(* Unsafe operations.  Use with care! *)

module Unsafe : sig
  external variable : string -> 'a = "caml_js_var"

  type any
  external inject : 'a -> any = "%identity"
  external coerce : < .. > t -> < ..> t = "%identity"

  external get : 'a -> 'b -> 'c = "caml_js_get"
  external set : 'a -> 'b -> 'c -> unit = "caml_js_set"
  external call : 'a -> 'b -> any array -> 'c = "caml_js_call"
  external fun_call : 'a -> any array -> 'b = "caml_js_fun_call"
  external meth_call : 'a -> string -> any array -> 'b = "caml_js_meth_call"
  external new_obj : 'a -> any array -> 'b = "caml_js_new"

(*FIX also, object/array literals *)
end
