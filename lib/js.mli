(* Js_of_ocaml library
 * http://www.ocsigen.org/js_of_ocaml/
 * Copyright (C) 2010 Jérôme Vouillon
 * Laboratoire PPS - CNRS Université Paris Diderot
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU Lesser General Public License as published by
 * the Free Software Foundation, with linking exception;
 * either version 2.1 of the License, or (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
 *)

(** Javascript binding

    This module provides types and functions to interoperate with Javascript
    values, and gives access to Javascript standard objects. *)

(** {2 Dealing with [null] and [undefined] values.} *)

(** Type of possibly null values. *)
type +'a opt

(** Type of possibly undefined values. *)
type +'a optdef

(** The [null] value. *)
val null : 'a opt

(** Consider a value into a possibly null value. *)
val some : 'a -> 'a opt

(** The [undefined] value *)
val undefined : 'a optdef

(** Consider a value into a possibly undefined value. *)
val def : 'a -> 'a optdef

(** Signatures of a set of standard functions for manipulating optional values. *)
module type OPT = sig
  type 'a t

  (** No value. *)
  val empty : 'a t

  (** Consider a value as an optional value. *)
  val return : 'a -> 'a t

  (** Apply a function to an optional value if it is available. Returns the
      result of the application. *)
  val map : 'a t -> ('a -> 'b) -> 'b t

  (** Apply a function returning an optional value to an optional value *)
  val bind : 'a t -> ('a -> 'b t) -> 'b t

  (** Returns [true] if a value is available, [false] otherwise. *)
  val test : 'a t -> bool

  (** Apply a function to an optional value if it is available. *)
  val iter : 'a t -> ('a -> unit) -> unit

  (** Pattern matching on optional values. *)
  val case : 'a t -> (unit -> 'b) -> ('a -> 'b) -> 'b

  (** Get the value. If no value available, an alternative function is called
      to get a default value. *)
  val get : 'a t -> (unit -> 'a) -> 'a

  (** Convert option type. *)
  val option : 'a option -> 'a t

  (** Convert to option type. *)
  val to_option : 'a t -> 'a option
end

(** Standard functions for manipulating possibly null values. *)
module Opt : OPT with type 'a t = 'a opt

(** Standard functions for manipulating possibly undefined values. *)
module Optdef : OPT with type 'a t = 'a optdef

(** {2 Types for specifying method and properties of Javascript objects} *)

(** Type of Javascript objects. The type parameter is used to specify more
    precisely an object. *)
type +'a t

(** Type used to specify method types: a Javascript object [<m : t1 -> t2 ->
    ... -> tn -> t Js.meth> Js.t] has a Javascript method [m] expecting {i n}
    arguments of types [t1] to [tn] and returns a value of type [t]. *)
type +'a meth

(** Type used to specify the properties of Javascript objects. In practice you
    should rarely need this type directly, but should rather use the type
    abbreviations below instead. *)
type +'a gen_prop

(** Type of read-only properties: a Javascript object [<p : t Js.readonly_prop>
    Js.t] has a read-only property [p] of type [t]. *)
type 'a readonly_prop = < get: 'a > gen_prop

(** Type of write-only properties: a Javascript object [<p : t
    Js.writeonly_prop> Js.t] has a write-only property [p] of type [t]. *)
type 'a writeonly_prop = < set: 'a -> unit > gen_prop

(** Type of read/write properties: a Javascript object [<p : t
    Js.writeonly_prop> Js.t] has a read/write property [p] of type [t]. *)
type 'a prop = < get: 'a ; set: 'a -> unit > gen_prop

(** Type of read/write properties that may be undefined: you can set them to a
    value of some type [t], but if you read them, you will get a value of type
    [t optdef] (that may be [undefined]). *)
type 'a optdef_prop = < get: 'a optdef ; set: 'a -> unit > gen_prop

(** {2 Object constructors} *)

(** A value of type [(t1 -> ... -> tn -> t Js.t) Js.constr] is a Javascript
    constructor expecting {i n} arguments of types [t1] to [tn] and returning a
    Javascript object of type [t Js.t]. Use the syntax extension [jsnew c (e1,
    ..., en)] to build an object using constructor [c] and arguments [e1] to
    [en]. *)
type +'a constr

(** {2 Callbacks to OCaml} *)

(** Type of callback functions. A function of type [(u, t1 -> ... -> tn -> t)
    meth_callback] can be called from Javascript with [this] bound to a value
    of type [u] and up to {i n} arguments of types [t1] to [tn]. The system
    takes care of currification, so less than {i n} arguments can be provided.
    As a special case, a callback of type [(t, unit -> t) meth_callback] can be
    called from Javascript with no argument. It will behave as if it was called
    with a single argument of type [unit]. *)
type (-'a, +'b) meth_callback

(** Type of callback functions intended to be called without a meaningful
    [this] implicit parameter. *)
type 'a callback = (unit, 'a) meth_callback

(** Wrap an OCaml function so that it can be invoked from Javascript. *)
external wrap_callback :
  ('a -> 'b) -> ('c, 'a -> 'b) meth_callback
  = "caml_js_wrap_callback"

(** Wrap an OCaml function so that it can be invoked from Javascript. The first
    parameter of the function will be bound to the value of the [this] implicit
    parameter. *)
external wrap_meth_callback :
  ('b -> 'a) -> ('b, 'a) meth_callback
  = "caml_js_wrap_meth_callback"

(** {2 Javascript standard objects} *)

(** Javascript [true] boolean. *)
val _true : bool t

(** Javascript [false] boolean. *)
val _false : bool t

(** A handle to a match result. Use function [Js.match_result] to get the
    corresponding [MatchResult] object. (This type is used to resolved the
    mutual dependency between string and array type definitions.) *)
type match_result_handle

(** Opaque type for string arrays. You can get the actual [Array] object using
    function [Js.str_array]. (This type is used to resolved the mutual
    dependency between string and array type definitions.) *)
type string_array

(** Specification of Javascript string objects. *)
class type js_string =
  object
    method toString : js_string t meth

    method valueOf : js_string t meth

    method charAt : int -> js_string t meth

    method charCodeAt : int -> float meth

    (* This may return NaN... *)
    method concat : js_string t -> js_string t meth

    method concat_2 : js_string t -> js_string t -> js_string t meth

    method concat_3 :
      js_string t -> js_string t -> js_string t -> js_string t meth

    method concat_4 :
         js_string t
      -> js_string t
      -> js_string t
      -> js_string t
      -> js_string t meth

    method indexOf : js_string t -> int meth

    method indexOf_from : js_string t -> int -> int meth

    method lastIndexOf : js_string t -> int meth

    method lastIndexOf_from : js_string t -> int -> int meth

    method localeCompare : js_string t -> float meth

    method _match : regExp t -> match_result_handle t opt meth

    method replace : regExp t -> js_string t -> js_string t meth

    (* FIX: version of replace taking a function... *)
    method replace_string : js_string t -> js_string t -> js_string t meth

    method search : regExp t -> int meth

    method slice : int -> int -> js_string t meth

    method slice_end : int -> js_string t meth

    method split : js_string t -> string_array t meth

    method split_limited : js_string t -> int -> string_array t meth

    method split_regExp : regExp t -> string_array t meth

    method split_regExpLimited : regExp t -> int -> string_array t meth

    method substring : int -> int -> js_string t meth

    method substring_toEnd : int -> js_string t meth

    method toLowerCase : js_string t meth

    method toLocaleLowerCase : js_string t meth

    method toUpperCase : js_string t meth

    method toLocaleUpperCase : js_string t meth

    method trim : js_string t meth

    method length : int readonly_prop
  end

(** Specification of Javascript regular expression objects. *)
and regExp =
  object
    method exec : js_string t -> match_result_handle t opt meth

    method test : js_string t -> bool t meth

    method toString : js_string t meth

    method source : js_string t readonly_prop

    method global : bool t readonly_prop

    method ignoreCase : bool t readonly_prop

    method multiline : bool t readonly_prop

    method lastIndex : int prop
  end

(** Specification of the string constructor, considered as an object. *)
class type string_constr =
  object
    method fromCharCode : int -> js_string t meth
  end

(** The string constructor, as an object. *)
val string_constr : string_constr t

(** Constructor of [RegExp] objects. The expression [jsnew regExp (s)] builds
    the regular expression specified by string [s]. *)
val regExp : (js_string t -> regExp t) constr

(** Constructor of [RegExp] objects. The expression [jsnew regExp (s, f)]
    builds the regular expression specified by string [s] using flags [f]. *)
val regExp_withFlags : (js_string t -> js_string t -> regExp t) constr

(** Constructor of [RegExp] objects. The expression [jsnew regExp (r)] builds a
    copy of regular expression [r]. *)
val regExp_copy : (regExp t -> regExp t) constr

(** Specification of Javascript regular arrays. Use [Js.array_get] and
    [Js.array_set] to access and set array elements. *)
class type ['a] js_array =
  object
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

    method some :
      ('a -> int -> 'a js_array t -> bool t) callback -> bool t meth

    method every :
      ('a -> int -> 'a js_array t -> bool t) callback -> bool t meth

    method forEach : ('a -> int -> 'a js_array t -> unit) callback -> unit meth

    method map :
      ('a -> int -> 'a js_array t -> 'b) callback -> 'b js_array t meth

    method filter :
      ('a -> int -> 'a js_array t -> bool t) callback -> 'a js_array t meth

    method reduce_init :
      ('b -> 'a -> int -> 'a js_array t -> 'b) callback -> 'b -> 'b meth

    method reduce :
      ('a -> 'a -> int -> 'a js_array t -> 'a) callback -> 'a meth

    method reduceRight_init :
      ('b -> 'a -> int -> 'a js_array t -> 'b) callback -> 'b -> 'b meth

    method reduceRight :
      ('a -> 'a -> int -> 'a js_array t -> 'a) callback -> 'a meth

    method length : int prop
  end

(** Returns jsarray containing keys of the object as Object.keys does. *)
val object_keys : 'a t -> js_string t js_array t

(** Constructor of [Array] objects. The expression [jsnew array_empty ()]
    returns an empty array. *)
val array_empty : 'a js_array t constr

(** Constructor of [Array] objects. The expression [jsnew array_length (l)]
    returns an array of length [l]. *)
val array_length : (int -> 'a js_array t) constr

(** Array access: [array_get a i] returns the element at index [i] of array
    [a]. Returns [undefined] if there is no element at this index. *)
val array_get : 'a #js_array t -> int -> 'a optdef

(** Array update: [array_set a i v] puts [v] at index [i] in array [a]. *)
val array_set : 'a #js_array t -> int -> 'a -> unit

(** Array map: [array_map f a] is [a##map(wrap_callback (fun elt idx arr -> f
    elt))]. *)
val array_map : ('a -> 'b) -> 'a #js_array t -> 'b #js_array t

(** Array mapi: [array_mapi f a] is [a##map(wrap_callback (fun elt idx arr -> f
    idx elt))]. *)
val array_mapi : (int -> 'a -> 'b) -> 'a #js_array t -> 'b #js_array t

(** Specification of match result objects *)
class type match_result =
  object
    inherit [js_string t] js_array

    method index : int readonly_prop

    method input : js_string t readonly_prop
  end

(** Convert an opaque [string_array t] object into an array of string. (Used to
    resolved the mutual dependency between string and array type definitions.) *)
val str_array : string_array t -> js_string t js_array t

(** Convert a match result handle into a [MatchResult] object. (Used to
    resolved the mutual dependency between string and array type definitions.) *)
val match_result : match_result_handle t -> match_result t

(** Specification of Javascript number objects. *)
class type number =
  object
    method toString : js_string t meth

    method toString_radix : int -> js_string t meth

    method toLocaleString : js_string t meth

    method toFixed : int -> js_string t meth

    method toExponential : js_string t meth

    method toExponential_digits : int -> js_string t meth

    method toPrecision : int -> js_string t meth
  end

(** Conversion of OCaml floats to Javascript number objects. *)
external number_of_float : float -> number t = "caml_js_from_float"

(** Conversion of Javascript number objects to OCaml floats. *)
external float_of_number : number t -> float = "caml_js_to_float"

(** Specification of Javascript date objects. *)
class type date =
  object
    method toString : js_string t meth

    method toDateString : js_string t meth

    method toTimeString : js_string t meth

    method toLocaleString : js_string t meth

    method toLocaleDateString : js_string t meth

    method toLocaleTimeString : js_string t meth

    method valueOf : float meth

    method getTime : float meth

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

    method setTime : float -> float meth

    method setFullYear : int -> float meth

    method setUTCFullYear : int -> float meth

    method setMonth : int -> float meth

    method setUTCMonth : int -> float meth

    method setDate : int -> float meth

    method setUTCDate : int -> float meth

    method setDay : int -> float meth

    method setUTCDay : int -> float meth

    method setHours : int -> float meth

    method setUTCHours : int -> float meth

    method setMinutes : int -> float meth

    method setUTCMinutes : int -> float meth

    method setSeconds : int -> float meth

    method setUTCSeconds : int -> float meth

    method setMilliseconds : int -> float meth

    method setUTCMilliseconds : int -> float meth

    method toUTCString : js_string t meth

    method toISOString : js_string t meth

    method toJSON : 'a -> js_string t meth
  end

(** Constructor of [Date] objects: [jsnew date_now ()] returns a [Date] object
    initialized with the current date. *)
val date_now : date t constr

(** Constructor of [Date] objects: [jsnew date_fromTimeValue (t)] returns a
    [Date] object initialized with the time value [t]. *)
val date_fromTimeValue : (float -> date t) constr

(** Constructor of [Date] objects: [jsnew date_fromTimeValue (y, m)] returns a
    [Date] object corresponding to year [y] and month [m]. *)
val date_month : (int -> int -> date t) constr

(** Constructor of [Date] objects: [jsnew date_fromTimeValue (y, m, d)] returns
    a [Date] object corresponding to year [y], month [m] and day [d]. *)
val date_day : (int -> int -> int -> date t) constr

(** Constructor of [Date] objects: [jsnew date_fromTimeValue (y, m, d, h)]
    returns a [Date] object corresponding to year [y] to hour [h]. *)
val date_hour : (int -> int -> int -> int -> date t) constr

(** Constructor of [Date] objects: [jsnew date_fromTimeValue (y, m, d, h, m')]
    returns a [Date] object corresponding to year [y] to minute [m']. *)
val date_min : (int -> int -> int -> int -> int -> date t) constr

(** Constructor of [Date] objects: [jsnew date_fromTimeValue (y, m, d, h, m',
    s)] returns a [Date] object corresponding to year [y] to second [s]. *)
val date_sec : (int -> int -> int -> int -> int -> int -> date t) constr

(** Constructor of [Date] objects: [jsnew date_fromTimeValue (y, m, d, h, m',
    s, ms)] returns a [Date] object corresponding to year [y] to millisecond
    [ms]. *)
val date_ms : (int -> int -> int -> int -> int -> int -> int -> date t) constr

(** Specification of the date constructor, considered as an object. *)
class type date_constr =
  object
    method parse : js_string t -> float meth

    method _UTC_month : int -> int -> float meth

    method _UTC_day : int -> int -> float meth

    method _UTC_hour : int -> int -> int -> int -> float meth

    method _UTC_min : int -> int -> int -> int -> int -> float meth

    method _UTC_sec : int -> int -> int -> int -> int -> int -> float meth

    method _UTC_ms :
      int -> int -> int -> int -> int -> int -> int -> float meth

    method now : float meth
  end

(** The date constructor, as an object. *)
val date : date_constr t

(** Specification of Javascript math object. *)
class type math =
  object
    method _E : float readonly_prop

    method _LN2 : float readonly_prop

    method _LN10 : float readonly_prop

    method _LOG2E : float readonly_prop

    method _LOG10E : float readonly_prop

    method _PI : float readonly_prop

    method _SQRT1_2_ : float readonly_prop

    method _SQRT2 : float readonly_prop

    method abs : float -> float meth

    method acos : float -> float meth

    method asin : float -> float meth

    method atan : float -> float meth

    method atan2 : float -> float -> float meth

    method ceil : float -> float meth

    method cos : float -> float meth

    method exp : float -> float meth

    method floor : float -> float meth

    method log : float -> float meth

    method max : float -> float -> float meth

    method max_3 : float -> float -> float -> float meth

    method max_4 : float -> float -> float -> float -> float meth

    method min : float -> float -> float meth

    method min_3 : float -> float -> float -> float meth

    method min_4 : float -> float -> float -> float -> float meth

    method pow : float -> float -> float meth

    method random : float meth

    method round : float -> float meth

    method sin : float -> float meth

    method sqrt : float -> float meth

    method tan : float -> float meth
  end

(** The Math object *)
val math : math t

(** Specification of Javascript error object. *)
class type error =
  object
    method name : js_string t prop

    method message : js_string t prop

    method stack : js_string t optdef prop

    method toString : js_string t meth
  end

(** Constructor of [Error] objects: [jsnew error_constr (msg)] returns an
    [Error] object with the message [msg]. *)
val error_constr : (js_string t -> error t) constr

val string_of_error : error t -> string
val raise_js_error : error t -> 'a

(** The [Error] exception wrap javascript exceptions when catched by ocaml
    code. In case the javascript exception is not an instance of javascript
    [Error], it will be serialized and wrapped into a [Failure] exception. *)
exception Error of error t

(** Specification of Javascript JSON object. *)
class type json =
  object
    method parse : js_string t -> 'a meth

    method stringify : 'a -> js_string t meth
  end

(** JSON object *)
val _JSON : json t

(** {2 Standard Javascript functions} *)

(** Decode a URI: replace by the corresponding byte all escape sequences but
    the ones corresponding to a URI reserved character and convert the string
    from UTF-8 to UTF-16. *)
val decodeURI : js_string t -> js_string t

(** Decode a URIComponent: replace all escape sequences by the corresponding
    byte and convert the string from UTF-8 to UTF-16. *)
val decodeURIComponent : js_string t -> js_string t

(** Encode a URI: convert the string to UTF-8 and replace all unsafe bytes by
    the corresponding escape sequence. *)
val encodeURI : js_string t -> js_string t

(** Same as [encodeURI], but also encode URI reserved characters. *)
val encodeURIComponent : js_string t -> js_string t

(** Escape a string: unsafe UTF-16 code points are replaced by 2-digit and
    4-digit escape sequences. *)
val escape : js_string t -> js_string t

(** Unescape a string: 2-digit and 4-digit escape sequences are replaced by the
    corresponding UTF-16 code point. *)
val unescape : js_string t -> js_string t

val isNaN : 'a -> bool
val parseInt : js_string t -> int
val parseFloat : js_string t -> float

(** {2 Conversion functions between Javascript and OCaml types} *)

(** Conversion of booleans from OCaml to Javascript. *)
external bool : bool -> bool t = "caml_js_from_bool"

(** Conversion of booleans from Javascript to OCaml. *)
external to_bool : bool t -> bool = "caml_js_to_bool"

(** Conversion of strings from OCaml to Javascript. (The OCaml string is
    considered to be encoded in UTF-8 and is converted to UTF-16.) *)
external string : string -> js_string t = "caml_js_from_string"

(** Conversion of strings from Javascript to OCaml. *)
external to_string : js_string t -> string = "caml_js_to_string"

(** Conversion of arrays from OCaml to Javascript. *)
external array : 'a array -> 'a js_array t = "caml_js_from_array"

(** Conversion of arrays from Javascript to OCaml. *)
external to_array : 'a js_array t -> 'a array = "caml_js_to_array"

(** Conversion of strings of bytes from OCaml to Javascript. (Each byte will be
    converted in an UTF-16 code point.) *)
external bytestring : string -> js_string t = "caml_jsbytes_of_string"

(** Conversion of strings of bytes from Javascript to OCaml. (The Javascript
    string should only contain UTF-16 code points below 255.) *)
external to_bytestring : js_string t -> string = "caml_js_to_byte_string"

(** {2 Convenience coercion functions} *)

(** Apply a possibly failing coercion function. [coerce v c f] attempts to
    apply coercion [c] to value [v]. If the coercion returns [null], function
    [f] is called. *)
val coerce : 'a -> ('a -> 'b Opt.t) -> ('a -> 'b) -> 'b

(** Apply a possibly failing coercion function. [coerce_opt v c f] attempts to
    apply coercion [c] to value [v]. If [v] is [null] or the coercion returns
    [null], function [f] is called. Typical usage is the following:
    {[Js.coerce_opt (Dom_html.document##getElementById id)
    Dom_html.CoerceTo.div (fun _ -> assert false)]} *)
val coerce_opt : 'a Opt.t -> ('a -> 'b Opt.t) -> ('a -> 'b) -> 'b

(** {2 Type checking operators.} *)

(** Returns the type of a Javascript object. *)
external typeof : _ t -> js_string t = "caml_js_typeof"

(** Tests whether a Javascript object is an instance of a given class. *)
external instanceof : _ t -> _ constr -> bool = "caml_js_instanceof"

(** {2 Debugging operations.} *)

(** Invokes any available debugging functionality. If no debugging
    functionality is available, it has no effect. In practice, it will insert a
    "debugger;" statement in the generated javascript. *)
external debugger : unit -> unit = "debugger"

(** {2 Export functionality.} Export values to [module.exports] if it exists or
    to the global object otherwise. *)

(** [export name value] export [name] *)
val export : string -> 'a -> unit

(** [export_all obj] export every key of [obj] object. {[ export_all object%js
    method add x y = x +. y method abs x = abs_float x val zero = 0. end ]} *)
val export_all : 'a t -> unit

(** {2 Unsafe operations.} *)

(** Unsafe Javascript operations *)
module Unsafe : sig
  type top

  (** Top type. Used for putting values of different types in a same array. *)
  type any = top t

  type any_js_array = any

  (** Coercion to top type. *)
  external inject : 'a -> any = "%identity"

  (** Unsafe coercion between to Javascript objects. *)
  external coerce : _ t -> _ t = "%identity"

  (** Get the value of an object property. The expression [get o s] returns the
      value of property [s] of object [o]. *)
  external get : 'a -> 'b -> 'c = "caml_js_get"

  (** Set an object property. The expression [set o s v] set the property [s]
      of object [o] to value [v]. *)
  external set : 'a -> 'b -> 'c -> unit = "caml_js_set"

  (** Delete an object property. The expression [delete o s] deletes property
      [s] of object [o]. *)
  external delete : 'a -> 'b -> unit = "caml_js_delete"

  (** Performs a Javascript function call. The expression [call f o a] calls
      the Javascript function [f] with the arguments given by the array [a],
      and binding [this] to [o]. *)
  external call : 'a -> 'b -> any array -> 'c = "caml_js_call"

  (** Performs a Javascript function call. The expression [fun_call f a] calls
      the Javascript function [f] with the arguments given by the array [a]. *)
  external fun_call : 'a -> any array -> 'b = "caml_js_fun_call"

  (** Performs a Javascript method call. The expression [meth_call o m a] calls
      the Javascript method [m] of object [o] with the arguments given by the
      array [a]. *)
  external meth_call : 'a -> string -> any array -> 'b = "caml_js_meth_call"

  (** Create a Javascript object. The expression [new_obj c a] creates a
      Javascript object with constructor [c] using the arguments given by the
      array [a]. *)
  external new_obj : 'a -> any array -> 'b = "caml_js_new"

  (** Same Create a Javascript object. The expression [new_obj_arr c a] creates
      a Javascript object with constructor [c] using the arguments given by the
      Javascript array [a]. *)
  external new_obj_arr : 'a -> any_js_array -> 'b = "caml_ojs_new_arr"

  (** Creates a Javascript literal object. The expression [obj a] creates a
      Javascript object whose fields are given by the array [a] *)
  external obj : (string * any) array -> 'a = "caml_js_object"

  (** Asserts that an expression is pure, and can therefore be optimized away
      by the compiler if unused. *)
  external pure_expr : (unit -> 'a) -> 'a = "caml_js_pure_expr"

  (** Evaluate Javascript code *)
  external eval_string : string -> 'a = "caml_js_eval_string"

  (** [js_expr e] will parse the JavaScript expression [e] if [e] is available
      at compile time or will fallback to a runtime evaluation. See
      [eval_string] *)
  external js_expr : string -> 'a = "caml_js_expr"

  (** [pure_js_expr str] behaves like [pure_expr (fun () -> js_expr str)]. *)
  external pure_js_expr : string -> 'a = "caml_pure_js_expr"

  (** Javascript global object *)
  val global : < .. > t

  (** Wrap an OCaml function so that it can be invoked from Javascript.
      Contrary to [Js.wrap_callback], partial application and over-application
      are not supported: missing arguments will be set to [undefined] and extra
      arguments are lost. *)
  external callback : ('a -> 'b) -> ('c, 'a -> 'b) meth_callback = "%identity"

  (** Wrap an OCaml function so that it can be invoked from Javascript. The
      first parameter of the function will be bound to the [arguments]
      JavaScript *)
  external callback_with_arguments :
    (any_js_array -> 'b) -> ('c, any_js_array -> 'b) meth_callback
    = "caml_js_wrap_callback_arguments"

  external callback_with_arity :
    int -> ('a -> 'b) -> ('c, 'a -> 'b) meth_callback
    = "caml_js_wrap_callback_strict"

  (** Wrap an OCaml function so that it can be invoked from Javascript. The
      first parameter of the function will be bound to the value of the [this]
      implicit parameter. Contrary to [Js.wrap_meth_callback], partial
      application and over-application is not supported: missing arguments will
      be set to [undefined] and extra arguments are lost. *)
  external meth_callback :
    ('b -> 'a) -> ('b, 'a) meth_callback
    = "caml_js_wrap_meth_callback_unsafe"

  (** Wrap an OCaml function so that it can be invoked from Javascript. The
      first parameter of the function will be bound to the value of the [this]
      implicit parameter. The second parameter of the function with be bound to
      the value of the [arguments]. *)
  external meth_callback_with_arguments :
    ('b -> any_js_array -> 'a) -> ('b, any_js_array -> 'a) meth_callback
    = "caml_js_wrap_meth_callback_arguments"

  external meth_callback_with_arity :
    int -> ('b -> 'a) -> ('b, 'a) meth_callback
    = "caml_js_wrap_meth_callback_strict"

  (** {3 Deprecated functions.} *)

  (** Access a Javascript variable. [variable "foo"] will return the current
      value of variable [foo]. *)
  external variable : string -> 'a = "caml_js_var"
end

(** {2 Deprecated functions and types.} *)

(** Conversion of OCaml floats to Javascript numbers. *)
external float : float -> float = "%identity"

(** Conversion of Javascript numbers to OCaml floats. *)
external to_float : float -> float = "%identity"

(** Type of float properties. *)
type float_prop = float prop
