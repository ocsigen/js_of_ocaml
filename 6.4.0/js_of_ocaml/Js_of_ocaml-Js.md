
# Module `Js_of_ocaml.Js`

Javascript binding

This module provides types and functions to interoperate with Javascript values, and gives access to Javascript standard objects.


### Dealing with `null` and `undefined` values.

```ocaml
type +'a opt
```
Type of possibly null values.

```ocaml
type +'a optdef
```
Type of possibly undefined values.

```ocaml
val null : 'a opt
```
The `null` value.

```ocaml
val some : 'a -> 'a opt
```
Consider a value into a possibly null value.

```ocaml
val undefined : 'a optdef
```
The `undefined` value

```ocaml
val def : 'a -> 'a optdef
```
Consider a value into a possibly undefined value.

```ocaml
module type OPT = sig ... end
```
Signatures of a set of standard functions for manipulating optional values.

```ocaml
module Opt : OPT with type 'a t = 'a opt
```
Standard functions for manipulating possibly null values.

```ocaml
module Optdef : OPT with type 'a t = 'a optdef
```
Standard functions for manipulating possibly undefined values.


### Types for specifying method and properties of Javascript objects

```ocaml
type +'a t
```
Type of Javascript objects. The type parameter is used to specify more precisely an object.

```ocaml
type +'a meth
```
Type used to specify method types: a Javascript object `<m : t1 -> t2 -> ... -> tn -> t Js.meth> Js.t` has a Javascript method `m` expecting *n* arguments of types `t1` to `tn` and returns a value of type `t`.

```ocaml
type +'a gen_prop
```
Type used to specify the properties of Javascript objects. In practice you should rarely need this type directly, but should rather use the type abbreviations below instead.

```ocaml
type 'a readonly_prop = < get : 'a > gen_prop
```
Type of read-only properties: a Javascript object `<p : t Js.readonly_prop> Js.t` has a read-only property `p` of type `t`.

```ocaml
type 'a writeonly_prop = < set : 'a -> unit > gen_prop
```
Type of write-only properties: a Javascript object `<p : t Js.writeonly_prop> Js.t` has a write-only property `p` of type `t`.

```ocaml
type 'a prop = < get : 'a ; set : 'a -> unit > gen_prop
```
Type of read/write properties: a Javascript object `<p : t Js.prop> Js.t` has a read/write property `p` of type `t`.

```ocaml
type 'a optdef_prop = < get : 'a optdef ; set : 'a -> unit > gen_prop
```
Type of read/write properties that may be undefined: you can set them to a value of some type `t`, but if you read them, you will get a value of type `t optdef` (that may be `undefined`).


### Object constructors

```ocaml
type +'a constr
```
A value of type `(t1 -> ... -> tn -> t Js.t) Js.constr` is a Javascript constructor expecting *n* arguments of types `t1` to `tn` and returning a Javascript object of type `t Js.t`. Use the syntax extension `new%js c e1 ... en` to build an object using constructor `c` and arguments `e1` to `en`.


### Callbacks to OCaml

```ocaml
type (-'a, +'b) meth_callback
```
Type of callback functions. A function of type `(u, t1 -> ... -> tn -> t) meth_callback` can be called from Javascript with `this` bound to a value of type `u` and up to *n* arguments of types `t1` to `tn`. The system takes care of currification, so less than *n* arguments can be provided. As a special case, a callback of type `(t, unit -> t) meth_callback` can be called from Javascript with no argument. It will behave as if it was called with a single argument of type `unit`.

```ocaml
type 'a callback = (unit, 'a) meth_callback
```
Type of callback functions intended to be called without a meaningful `this` implicit parameter.

```ocaml
val wrap_callback : ('a -> 'b) -> ('c, 'a -> 'b) meth_callback
```
Wrap an OCaml function so that it can be invoked from Javascript.

```ocaml
val wrap_meth_callback : ('b -> 'a) -> ('b, 'a) meth_callback
```
Wrap an OCaml function so that it can be invoked from Javascript. The first parameter of the function will be bound to the value of the `this` implicit parameter.


### Javascript comparisons

```ocaml
val equals : _ t -> _ t -> bool
```
Javascript `==` equality operator.

```ocaml
val strict_equals : _ t -> _ t -> bool
```
Javascript `===` equality operator.


### Javascript standard objects

```ocaml
val _true : bool t
```
Javascript `true` boolean.

```ocaml
val _false : bool t
```
Javascript `false` boolean.

```ocaml
type match_result_handle
```
A handle to a match result. Use function `Js.match_result` to get the corresponding `MatchResult` object. (This type is used to resolved the mutual dependency between string and array type definitions.)

```ocaml
type string_array
```
Opaque type for string arrays. You can get the actual `Array` object using function `Js.str_array`. (This type is used to resolved the mutual dependency between string and array type definitions.)

```ocaml
type normalization
```
Opaque type for Unicode normalization forms.

```ocaml
val nfd : normalization t
```
Canonical Decomposition

```ocaml
val nfc : normalization t
```
Canonical Decomposition, followed by Canonical Composition

```ocaml
val nfkd : normalization t
```
Compatibility Decomposition

```ocaml
val nfkc : normalization t
```
Compatibility Decomposition, followed by Canonical Composition

Specification of Javascript number objects.

```ocaml
class type  number = object ... end
```
```ocaml
class type  js_string = object ... end
```
Specification of Javascript string objects.

```ocaml
class type  regExp = object ... end
```
Specification of Javascript regular expression objects.

```ocaml
type number_t = number t
```
```ocaml
class type  string_constr = object ... end
```
Specification of the string constructor, considered as an object.

```ocaml
val string_constr : string_constr t
```
The string constructor, as an object.

```ocaml
val regExp : (js_string t -> regExp t) constr
```
Constructor of `RegExp` objects. The expression `new%js regExp s` builds the regular expression specified by string `s`.

```ocaml
val regExp_withFlags : (js_string t -> js_string t -> regExp t) constr
```
Constructor of `RegExp` objects. The expression `new%js regExp s f` builds the regular expression specified by string `s` using flags `f`.

```ocaml
val regExp_copy : (regExp t -> regExp t) constr
```
Constructor of `RegExp` objects. The expression `new%js regExp r` builds a copy of regular expression `r`.

```ocaml
class type 'a js_array = object ... end
```
Specification of Javascript regular arrays. Use `Js.array_get` and `Js.array_set` to access and set array elements.

```ocaml
val object_keys : 'a t -> js_string t js_array t
```
Returns jsarray containing keys of the object as Object.keys does.

```ocaml
val array_empty : 'a js_array t constr
```
Constructor of `Array` objects. The expression `new%js array_empty` returns an empty array.

```ocaml
val array_length : (int -> 'a js_array t) constr
```
Constructor of `Array` objects. The expression `new%js array_length l` returns an array of length `l`.

```ocaml
val array_get : 'a js_array t -> int -> 'a optdef
```
Array access: `array_get a i` returns the element at index `i` of array `a`. Returns `undefined` if there is no element at this index.

```ocaml
val array_set : 'a js_array t -> int -> 'a -> unit
```
Array update: `array_set a i v` puts `v` at index `i` in array `a`.

```ocaml
val array_map : ('a -> 'b) -> 'a js_array t -> 'b js_array t
```
Array map: `array_map f a` is `a##map(wrap_callback (fun elt idx arr -> f elt))`.

```ocaml
val array_mapi : (int -> 'a -> 'b) -> 'a js_array t -> 'b js_array t
```
Array mapi: `array_mapi f a` is `a##map(wrap_callback (fun elt idx arr -> f idx elt))`.

```ocaml
class type  match_result = object ... end
```
Specification of match result objects

```ocaml
val str_array : string_array t -> js_string t js_array t
```
Convert an opaque `string_array t` object into an array of string. (Used to resolved the mutual dependency between string and array type definitions.)

```ocaml
val match_result : match_result_handle t -> match_result t
```
Convert a match result handle into a `MatchResult` object. (Used to resolved the mutual dependency between string and array type definitions.)

```ocaml
class type  date = object ... end
```
Specification of Javascript date objects.

```ocaml
val date_now : date t constr
```
Constructor of `Date` objects: `new%js date_now` returns a `Date` object initialized with the current date.

```ocaml
val date_fromTimeValue : (number_t -> date t) constr
```
Constructor of `Date` objects: `new%js date_fromTimeValue t` returns a `Date` object initialized with the time value `t`.

```ocaml
val date_month : (int -> int -> date t) constr
```
Constructor of `Date` objects: `new%js date_month y m` returns a `Date` object corresponding to year `y` and month `m`.

```ocaml
val date_day : (int -> int -> int -> date t) constr
```
Constructor of `Date` objects: `new%js date_day y m d` returns a `Date` object corresponding to year `y`, month `m` and day `d`.

```ocaml
val date_hour : (int -> int -> int -> int -> date t) constr
```
Constructor of `Date` objects: `new%js date_hour y m d h` returns a `Date` object corresponding to year `y` to hour `h`.

```ocaml
val date_min : (int -> int -> int -> int -> int -> date t) constr
```
Constructor of `Date` objects: `new%js date_min y m d h m'` returns a `Date` object corresponding to year `y` to minute `m'`.

```ocaml
val date_sec : (int -> int -> int -> int -> int -> int -> date t) constr
```
Constructor of `Date` objects: `new%js date_sec y m d h m' s` returns a `Date` object corresponding to year `y` to second `s`.

```ocaml
val date_ms : (int -> int -> int -> int -> int -> int -> int -> date t) constr
```
Constructor of `Date` objects: `new%js date_ms y m d h m' s ms` returns a `Date` object corresponding to year `y` to millisecond `ms`.

```ocaml
class type  date_constr = object ... end
```
Specification of the date constructor, considered as an object.

```ocaml
val date : date_constr t
```
The date constructor, as an object.

```ocaml
class type  math = object ... end
```
Specification of Javascript math object.

```ocaml
val math : math t
```
The Math object

```ocaml
class type  error = object ... end
```
Specification of Javascript error object.

```ocaml
val error_constr : (js_string t -> error t) constr
```
Constructor of `Error` objects: `new%js error_constr msg` returns an `Error` object with the message `msg`.

```ocaml
module Js_error : sig ... end
```
```ocaml
class type  json = object ... end
```
Specification of Javascript JSON object.

```ocaml
val _JSON : json t
```
JSON object


### Standard Javascript functions

```ocaml
val decodeURI : js_string t -> js_string t
```
Decode a URI: replace by the corresponding byte all escape sequences but the ones corresponding to a URI reserved character and convert the string from UTF-8 to UTF-16.

```ocaml
val decodeURIComponent : js_string t -> js_string t
```
Decode a URIComponent: replace all escape sequences by the corresponding byte and convert the string from UTF-8 to UTF-16.

```ocaml
val encodeURI : js_string t -> js_string t
```
Encode a URI: convert the string to UTF-8 and replace all unsafe bytes by the corresponding escape sequence.

```ocaml
val encodeURIComponent : js_string t -> js_string t
```
Same as `encodeURI`, but also encode URI reserved characters.

```ocaml
val escape : js_string t -> js_string t
```
Escape a string: unsafe UTF-16 code points are replaced by 2-digit and 4-digit escape sequences.

```ocaml
val unescape : js_string t -> js_string t
```
Unescape a string: 2-digit and 4-digit escape sequences are replaced by the corresponding UTF-16 code point.

```ocaml
val isNaN : 'a -> bool
```
```ocaml
val parseInt : js_string t -> int
```
Parse a string as an integer. Raises `Failure "parseInt"` if the string does not start with a number.

```ocaml
val parseFloat : js_string t -> number_t
```
Parse a string as a floating-point number. Raises `Failure "parseFloat"` if the string does not start with a number.


### Conversion functions between Javascript and OCaml types

```ocaml
val bool : bool -> bool t
```
Conversion of booleans from OCaml to Javascript.

```ocaml
val to_bool : bool t -> bool
```
Conversion of booleans from Javascript to OCaml.

```ocaml
val string : string -> js_string t
```
Conversion of strings from OCaml to Javascript. (The OCaml string is considered to be encoded in UTF-8 and is converted to UTF-16.)

```ocaml
val to_string : js_string t -> string
```
Conversion of strings from Javascript to OCaml.

```ocaml
val array : 'a array -> 'a js_array t
```
Conversion of arrays from OCaml to Javascript.

```ocaml
val to_array : 'a js_array t -> 'a array
```
Conversion of arrays from Javascript to OCaml.

```ocaml
val bytestring : string -> js_string t
```
Conversion of strings of bytes from OCaml to Javascript. (Each byte will be converted in an UTF-16 code point.)

```ocaml
val to_bytestring : js_string t -> string
```
Conversion of strings of bytes from Javascript to OCaml. (The Javascript string should only contain UTF-16 code points below 255\.)

```ocaml
val float : float -> number_t
```
Conversion of OCaml floats to Javascript numbers.

```ocaml
val to_float : number_t -> float
```
Conversion of Javascript numbers to OCaml floats.

```ocaml
val number_of_float : float -> number t
```
Conversion of OCaml floats to Javascript number objects.

```ocaml
val float_of_number : number t -> float
```
Conversion of Javascript number objects to OCaml floats.

```ocaml
val int32 : int32 -> number_t
```
Conversion of OCaml 32-bit integers to Javascript numbers.

```ocaml
val to_int32 : number_t -> int32
```
Conversion of Javascript numbers to OCaml 32-bits. The given floating-point number is truncated to an integer.

```ocaml
val nativeint : nativeint -> number_t
```
Conversion of OCaml native integers to Javascript numbers.

```ocaml
val to_nativeint : number_t -> nativeint
```
Conversion of Javascript numbers to OCaml native integers. The given floating-point number is truncated to an integer.


### Convenience coercion functions

```ocaml
val coerce : 'a -> ('a -> 'b Opt.t) -> ('a -> 'b) -> 'b
```
Apply a possibly failing coercion function. `coerce v c f` attempts to apply coercion `c` to value `v`. If the coercion returns `null`, function `f` is called.

```ocaml
val coerce_opt : 'a Opt.t -> ('a -> 'b Opt.t) -> ('a -> 'b) -> 'b
```
Apply a possibly failing coercion function. `coerce_opt v c f` attempts to apply coercion `c` to value `v`. If `v` is `null` or the coercion returns `null`, function `f` is called. Typical usage is the following:

```ocaml
Js.coerce_opt (Dom_html.document##getElementById id)
Dom_html.CoerceTo.div (fun _ -> assert false)
```

### Type checking operators.

```ocaml
val typeof : _ t -> js_string t
```
Returns the type of a Javascript object.

```ocaml
val instanceof : _ t -> _ constr -> bool
```
Tests whether a Javascript object is an instance of a given class.


### Debugging operations.

```ocaml
val debugger : unit -> unit
```
Invokes any available debugging functionality. If no debugging functionality is available, it has no effect. In practice, it will insert a "debugger;" statement in the generated javascript.


### Export functionality.

Export values to `module.exports` if it exists or to the global object otherwise.

```ocaml
val export : string -> 'a -> unit
```
`export name value` export `name`

```ocaml
val export_all : 'a t -> unit
```
`export_all obj` export every key of `obj` object.

```ocaml
export_all
  object%js
    method add x y = x +. y
    method abs x = abs_float x
    val zero = 0.
  end
```

### Unsafe operations.

```ocaml
module Unsafe : sig ... end
```
Unsafe Javascript operations


### Deprecated functions and types.

```ocaml
val string_of_error : error t -> string
```
deprecated \[since 4.0\] Use \[Js\_error.to\_string\] instead.
```ocaml
val raise_js_error : error t -> 'a
```
deprecated \[since 4.0\] Use \[Js\_error.raise\_\] instead.
```ocaml
val exn_with_js_backtrace : exn -> force:bool -> exn
```
Attach a JavaScript error to an OCaml exception. if `force = false` and a JavaScript error is already attached, it will do nothing. This function is useful to store and retrieve information about JavaScript stack traces.

Attaching JavaScript errors will happen automatically when compiling with `--enable with-js-error`.

deprecated \[since 4.0\] Use \[Js\_error.raise\_\] instead.
```ocaml
val js_error_of_exn : exn -> error t opt
```
Extract a JavaScript error attached to an OCaml exception, if any. This is useful to inspect an eventual stack trace, especially when sourcemap is enabled.

deprecated \[since 4.0\] Use \[Js\_error.of\_exn\] instead.
```ocaml
exception Error of error t
```
The `Error` exception wrap javascript exceptions when caught by OCaml code. In case the javascript exception is not an instance of javascript `Error`, it will be serialized and wrapped into a `Failure` exception.

deprecated \[since 4.0\] Use \[Js\_error.Exn\] instead.
```ocaml
type float_prop = number_t prop
```
deprecated \[since 2.0\].
Type of float properties.
