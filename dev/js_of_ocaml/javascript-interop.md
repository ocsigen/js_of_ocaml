
# JavaScript interoperability

This page explains how to interact with JavaScript from OCaml using the Js\_of\_ocaml library.


## Introduction

OCaml and JavaScript represent values differently, for example:

- OCaml strings are sequence of bytes; JavaScript strings are UTF-16
- OCaml booleans are integers (0/1); JavaScript has distinct `true`/`false`
- OCaml arrays have a tag element; JavaScript arrays don't
- OCaml objects don't map to JavaScript objects
Js\_of\_ocaml (the lib) provides a typed interface to bridge these differences safely.


### Alternatives

Other libraries for JavaScript interop:

- [Brr](https://erratique.ch/software/brr) — Alternative browser API bindings with a different design
- [gen\_js\_api](https://github.com/LexiFi/gen_js_api) — Generates bindings from TypeScript definitions

## Core concept: `'a Js.t`

All JavaScript values have type `'a Js.t` where the phantom type parameter `'a` encodes the shape of the JavaScript value:

For example, `Js.js_string Js.t` represents a JavaScript string, and `< length : int Js.readonly_prop > Js.t` represents an object with a `length` property.

To work with these values (access properties, call methods), you need the [PPX syntax extension](./ppx.md) which provides operators like `##.` and `##`.


## Conversions

OCaml and JavaScript represent basic types differently (see [runtime representation](./runtime-representation.md)). For example, OCaml strings are byte sequences while JavaScript strings are UTF-16. When passing values between OCaml and JavaScript code, you must convert them explicitly.

The conversion functions follow a consistent naming pattern:

- **`Js.xxx`** converts OCaml to JavaScript (e.g., `Js.string`, `Js.bool`, `Js.array`)
- **`Js.to_xxx`** converts JavaScript to OCaml (e.g., `Js.to_string`, `Js.to_bool`, `Js.to_array`)
**When do you need to convert?**

- Passing OCaml values to JavaScript functions or properties
- Reading JavaScript values back into OCaml
- Working with DOM APIs (which use JavaScript types)
**Exception**: OCaml integers can be used directly—no conversion needed.


### Summary

| --- | --- | --- | --- |
| OCaml type | JS type | OCaml \-\> JS | JS \-\> OCaml |
| `string` | String | [Js.string](./Js_of_ocaml-Js.md#val-string) | [Js.to\_string](./Js_of_ocaml-Js.md#val-to_string) |
| `string` (bytes) | String | [Js.bytestring](./Js_of_ocaml-Js.md#val-bytestring) | [Js.to\_bytestring](./Js_of_ocaml-Js.md#val-to_bytestring) |
| `bool` | Boolean | [Js.bool](./Js_of_ocaml-Js.md#val-bool) | [Js.to\_bool](./Js_of_ocaml-Js.md#val-to_bool) |
| `int` | Number | (direct) | (direct) |
| `float` | Number | [Js.float](./Js_of_ocaml-Js.md#val-float) | [Js.to\_float](./Js_of_ocaml-Js.md#val-to_float) |
| `'a array` | Array | [Js.array](./Js_of_ocaml-Js.md#val-array) | [Js.to\_array](./Js_of_ocaml-Js.md#val-to_array) |

### Strings

OCaml strings are byte arrays; JavaScript strings are UTF-16.

```ocaml
(* OCaml string -> JS string *)
let js_str : Js.js_string Js.t = Js.string "Hello"

(* JS string -> OCaml string *)
let ocaml_str : string = Js.to_string js_str
```
For binary data (bytes 0-255), use [Js.bytestring](./Js_of_ocaml-Js.md#val-bytestring) and [Js.to\_bytestring](./Js_of_ocaml-Js.md#val-to_bytestring):

```ocaml
let js_bytes = Js.bytestring "\x00\x01\x02"
let ocaml_bytes = Js.to_bytestring js_bytes
```

### Booleans

OCaml booleans are encoded as 0 and 1, not JavaScript's `true` and `false`.

```ocaml
let js_true : bool Js.t = Js._true        (* or Js.bool true *)
let js_false : bool Js.t = Js._false      (* or Js.bool false *)
let ocaml_bool : bool = Js.to_bool js_true
```

### Numbers

OCaml integers can be used directly. Floats need conversion.

```ocaml
(* Floats need conversion *)
let js_num : Js.number Js.t = Js.float 3.14
let ocaml_float : float = Js.to_float js_num
```

### Arrays

```ocaml
(* OCaml array -> JS array *)
let js_arr : int Js.js_array Js.t = Js.array [| 1; 2; 3 |]

(* JS array -> OCaml array *)
let ocaml_arr : int array = Js.to_array js_arr
```

## Describing JS objects

To call methods or access properties on a JavaScript object, you need to tell OCaml what shape the object has. This is done using OCaml class types, where each `method` declaration describes either a JavaScript property or method.

There are two ways to write these types:

**Inline (anonymous) types** — useful for one-off or simple cases:

```
< field1 : type1; field2 : type2; ... > Js.t
```
**Named class types** — better for reusable or complex interfaces:

```ocaml
class type myObject = object
  method field1 : type1
  method field2 : type2
end

(* Use as: myObject Js.t *)
```
For instance, a JavaScript object with a `data` property and an `appendData` method would have type:

```
< data : Js.js_string Js.t Js.prop;
  appendData : Js.js_string Js.t -> unit Js.meth > Js.t
```

### Property types

| --- | --- |
| Type | Description |
| `'a Js.readonly_prop` | Read-only property |
| `'a Js.writeonly_prop` | Write-only property |
| `'a Js.prop` | Read/write property |
| `t1 -> ... -> tn -> t Js.meth` | Method taking n arguments |
| `'a Js.optdef_prop` | Optional property (may be `undefined`) |
The [PPX syntax](./ppx.md) rely on these info to provide type safe access to properties and method.


### Example

Given a JavaScript object:

```javascript
{
  name: "example",           // read-only string
  count: 42,                 // read-write number
  greet: function(x) { ... } // method
}
```
Its type could be `myObj Js.t`:

```ocaml
class type myObj = object
  method name : Js.js_string Js.t Js.readonly_prop
  method count : int Js.prop
  method greet : Js.js_string Js.t -> unit Js.meth
end
```

### Method and property name mangling

When accessing a field using the `##.`/`##` syntax, the field name is transformed by:

1. Removing a leading underscore (if present)
2. Removing all characters starting from the last underscore
This enables:

- **Capitalized names**: `_Foo` refers to JavaScript's `Foo`
- **Reserved ocaml keywords**: `_type` refers to JavaScript's `type`
- **Method overloading**: `foo_int` and `foo_string` both refer to `foo`
**Warning**: This mangling is a common source of bugs. If you write `obj##.some_property`, it accesses the JavaScript property `some` (not `some_property`). To access `some_property`, use `obj##._some_property_` or `obj##.some_property_`.


#### Examples

```ocaml
class type canvas = object
  (* All three refer to the same JS method: drawImage *)
  method drawImage :
      imageElement Js.t -> int -> int -> unit Js.meth
  method drawImage_withSize :
      imageElement Js.t -> int -> int -> int -> int -> unit Js.meth
  method drawImage_fromCanvas :
      canvasElement Js.t -> int -> int -> unit Js.meth
end
```

#### Full naming rules

```ocaml
class type example = object
  (* All of these refer to JS field [meth] *)
  method meth : ..
  method meth_int : ..
  method _meth_ : ..
  method _meth_aa : ..

  (* All of these refer to JS field [meth_a] *)
  method meth_a_int : ..
  method _meth_a_ : ..
  method _meth_a_b : ..

  (* Refer to [Meth] (capitalized) *)
  method _Meth : ..

  (* Refer to [_meth] (leading underscore in JS) *)
  method __meth : ..

  (* Refer to [_] *)
  method __ : ..
end
```

### Binding constants from a class

For JavaScript constants like `SomeLib.SomeClass.VALUE_A`:

```ocaml
(* Type definition *)
class type someClass = object
  method _VALUE_A_ : int Js.readonly_prop
  method _VALUE_B_ : int Js.readonly_prop
end

(* Get the class object *)
let someClass : someClass Js.t =
  (Js.Unsafe.js_expr "SomeLib")##._SomeClass

(* Access constants *)
let value_a = someClass##._VALUE_A_
```

## Accessing JavaScript values

Once you have described a JavaScript object's type, use the [PPX syntax](./ppx.md) to access its properties and methods:

- `obj##.prop` — read a property
- `obj##.prop := v` — write a property
- `obj##meth args` — call a method

### Global variables

Global JavaScript variables are properties of the global object. Use [Js.Unsafe.global](./Js_of_ocaml-Js-Unsafe.md#val-global) to access them (`window` in browsers, `globalThis` in Node.js):

```ocaml
(* Access document *)
let doc : Dom_html.document Js.t = Js.Unsafe.global##.document

(* Read and write a custom global *)
let get_config () : config Js.t = Js.Unsafe.global##.myAppConfig
let set_config (x : config Js.t) = Js.Unsafe.global##.myAppConfig := x
```
You can also use [Js.Unsafe.js\_expr](./Js_of_ocaml-Js-Unsafe.md#val-js_expr) for any JavaScript expression:

```ocaml
let v = (Js.Unsafe.js_expr "window")##.document
```
Be careful: both [Js.Unsafe.global](./Js_of_ocaml-Js-Unsafe.md#val-global) and [Js.Unsafe.js\_expr](./Js_of_ocaml-Js-Unsafe.md#val-js_expr) are untyped. Verify the library documentation before writing type annotations.


### Untyped property access

When a property is missing from the OCaml interface, use [Js.Unsafe.coerce](./Js_of_ocaml-Js-Unsafe.md#val-coerce) for untyped access:

```ocaml
(* Read a property *)
let value = (Js.Unsafe.coerce obj)##.someProp

(* Write a property *)
(Js.Unsafe.coerce obj)##.someProp := v
```

## Handling null and undefined

JavaScript has two "missing value" types: `null` and `undefined`. Js\_of\_ocaml represents these with distinct types.


### Js.Opt for nullable values (`null`)

Use [Js.Opt](./Js_of_ocaml-Js-Opt.md) for values that may be `null` (e.g., DOM methods that return `null` when an element is not found):

```ocaml
(* Check if null *)
let is_present = Js.Opt.test value

(* Convert to OCaml option *)
let opt : element Js.t option = Js.Opt.to_option value

(* Handle both cases *)
let result = Js.Opt.case value
  (fun () -> (* null *) "not found")
  (fun v -> (* has value *) process v)

(* Get value or raise exception *)
let v = Js.Opt.get value (fun () -> failwith "was null")

(* Create nullable values *)
let some_val : element Js.t Js.opt = Js.some element
let null_val : element Js.t Js.opt = Js.null
```

### Js.Optdef for optional values (`undefined`)

Use [Js.Optdef](./Js_of_ocaml-Js-Optdef.md) for values that may be `undefined` (e.g., optional object properties, array access beyond bounds):

```ocaml
(* Check if defined *)
let is_defined = Js.Optdef.test value

(* Convert to OCaml option *)
let opt : config Js.t option = Js.Optdef.to_option value

(* Handle both cases *)
let result = Js.Optdef.case value
  (fun () -> (* undefined *) default_config)
  (fun v -> (* defined *) v)

(* Create optdef values *)
let def_val : int Js.optdef = Js.def 42
let undef_val : int Js.optdef = Js.undefined
```

## Calling JavaScript functions

OCaml and Javascript do not follow the same calling convention. In OCaml, functions can be partially applied, returning a function closure. In Javascript, when only some of the parameters are passed, the others are set to the undefined value. As a consequence, it is not possible to call a Javascript function from OCaml as if it was an OCaml function, and conversely.

There are three ways to call JavaScript functions, depending on how `this` should be bound.

At the moment, there is no syntactic sugar for calling Javascript functions.


### Standalone functions with `fun_call`

Use [Js.Unsafe.fun\_call](./Js_of_ocaml-Js-Unsafe.md#val-fun_call) for functions where `this` doesn't matter:

```ocaml
(* Equivalent to: decodeURI(s) *)
let decodeURI (s : Js.js_string Js.t) : Js.js_string Js.t =
  Js.Unsafe.fun_call
    (Js.Unsafe.js_expr "decodeURI")
    [| Js.Unsafe.inject s |]

(* Equivalent to: parseInt(s, 10) *)
let parseInt (s : Js.js_string Js.t) : int =
  Js.Unsafe.fun_call
    (Js.Unsafe.js_expr "parseInt")
    [| Js.Unsafe.inject s; Js.Unsafe.inject 10 |]
```

### Methods with `meth_call`

Use [Js.Unsafe.meth\_call](./Js_of_ocaml-Js-Unsafe.md#val-meth_call) to call a method on an object (`this` is bound to the object):

```ocaml
(* Equivalent to: arr.slice(1, 3) *)
let slice arr start stop =
  Js.Unsafe.meth_call arr "slice"
    [| Js.Unsafe.inject start; Js.Unsafe.inject stop |]
```

### Functions with explicit `this` binding

Use [Js.Unsafe.call](./Js_of_ocaml-Js-Unsafe.md#val-call) when you need to explicitly set `this`:

```ocaml
(* Equivalent to: func.call(thisArg, arg1, arg2) *)
let result =
  Js.Unsafe.call func thisArg [| Js.Unsafe.inject arg1; Js.Unsafe.inject arg2 |]
```

### Calling runtime primitives

For JavaScript functions declared in runtime files (with `//Provides:`), you can use OCaml `external` declarations:

```ocaml
external my_primitive : int -> int -> int = "my_js_function"
```
This calls the JavaScript function `my_js_function` directly, without the overhead of `Js.Unsafe` wrappers. See [writing JavaScript primitives](./linker.md#writing_primitives) for how to define such functions.


## Passing OCaml functions to JavaScript

When JavaScript code needs to call back into OCaml (e.g., event handlers, async callbacks), you must wrap OCaml functions appropriately.


### Basic callbacks with `Js.wrap_callback`

```ocaml
(* setTimeout example *)
let set_timeout f ms =
  Js.Unsafe.global##setTimeout
    (Js.wrap_callback f)
    ms

let () = set_timeout (fun () -> print_endline "Hello!") 1000
```
[Js.wrap\_callback](./Js_of_ocaml-Js.md#val-wrap_callback) handles partial application: if JavaScript calls the function with fewer arguments than expected, the result is a partially applied function.


### Callbacks with `this` binding

Use [Js.wrap\_meth\_callback](./Js_of_ocaml-Js.md#val-wrap_meth_callback) when the callback needs access to `this`:

```ocaml
(* The first parameter receives the 'this' value *)
let callback = Js.wrap_meth_callback (fun this event ->
  let target : Dom_html.element Js.t = this in
  (* ... handle event ... *)
  Js._true)
```

### DOM event handlers

For DOM events, use [Dom\_html.handler](./Js_of_ocaml-Dom_html.md#val-handler) which wraps the function and handles the return value (`Js._false` prevents the default action):

```ocaml
let handler = Dom_html.handler (fun event ->
  Dom_html.window##alert (Js.string "Clicked!");
  Js._true)

button##.onclick := handler
```
For handlers that need access to `this`, use [Dom.full\_handler](./Js_of_ocaml-Dom.md#val-full_handler):

```ocaml
let handler = Dom.full_handler (fun this event ->
  let element : Dom_html.element Js.t = this in
  (* ... *)
  Js._true)
```

### Strict arity callbacks

For performance-critical code, or when you don't need partial application, use [Js.Unsafe.callback](./Js_of_ocaml-Js-Unsafe.md#val-callback):

```ocaml
(* Strict callback - missing args become undefined, extra args are lost *)
let strict_cb = Js.Unsafe.callback (fun x y -> x + y)

(* Explicit arity - ensures exactly 2 arguments *)
let arity_cb = Js.Unsafe.callback_with_arity 2 (fun x y -> x + y)
```

### Callbacks with variable arguments

Use [Js.Unsafe.callback\_with\_arguments](./Js_of_ocaml-Js-Unsafe.md#val-callback_with_arguments) when the callback receives a variable number of arguments:

```ocaml
let varargs_cb = Js.Unsafe.callback_with_arguments (fun args ->
  let len = args##.length in
  Printf.printf "Called with %d arguments\n" len)
```

## Feature detection

JavaScript APIs vary across browsers. Use [Js.Optdef](./Js_of_ocaml-Js-Optdef.md) to check for optional features:

```ocaml
let supports_fetch () =
  Js.Optdef.test Js.Unsafe.global##.fetch

let supports_local_storage () =
  Js.Optdef.test Js.Unsafe.global##.localStorage

(* Safely access optional members *)
let get_optional_method obj =
  Js.Optdef.to_option (Js.Unsafe.coerce obj)##.someMethod
```

## Union types

JavaScript APIs often use union types (e.g., `string | Node`). Since OCaml requires a single type, use an opaque type with runtime checking.


### Using `instanceof` for object types

```ocaml
(* Opaque type representing the union: Element | Text *)
type element_or_text

class type container = object
  method child : element_or_text Js.t Js.prop
end

(* Cast using instanceof *)
let as_element (x : element_or_text Js.t) : Dom.element Js.t Js.opt =
  if Js.instanceof x (Js.Unsafe.global##._Element)
  then Js.some (Js.Unsafe.coerce x)
  else Js.null
```

### Using `typeof` for primitive types

```ocaml
type string_or_number

let as_string (x : string_or_number Js.t) : Js.js_string Js.t Js.opt =
  if Js.typeof x = Js.string "string"
  then Js.some (Js.Unsafe.coerce x)
  else Js.null
```

### Converting to OCaml variants

```ocaml
type child =
  | Element of Dom.element Js.t
  | Text of Dom.text Js.t

let classify_child (x : element_or_text Js.t) : child =
  if Js.instanceof x (Js.Unsafe.global##._Element)
  then Element (Js.Unsafe.coerce x)
  else Text (Js.Unsafe.coerce x)

(* Now use pattern matching *)
let handle container =
  match classify_child container##.child with
  | Element e -> (* work with element *)
  | Text t -> (* work with text node *)
```

## JSON serialization

The [Json](./Js_of_ocaml-Json.md) module provides serialization between OCaml values and JSON strings. The deserialization is unsafe in the same way the OCaml Marshal.from\_string function is.

```ocaml
(* OCaml value -> JSON string *)
let json : Js.js_string Js.t = Json.output value

(* JSON string -> OCaml value (unsafe, like Marshal) *)
let value : 'a = Json.unsafe_input json
```
For type-safe JSON handling, use [ppx\_deriving\_json](./ppx-deriving.md).


## Accessing runtime values

JavaScript values declared with `//Provides:` in runtime files can be accessed from OCaml. There are two approaches depending on whether you're accessing a function or a non-function value.

See [writing JavaScript primitives](./linker.md#writing_primitives) for more about the `//Provides:` syntax.


### Functions: use `external`

For JavaScript **functions**, use OCaml's `external` declaration:

```javascript
//Provides: my_add
function my_add(x, y) { return x + y; }
```
```ocaml
external my_add : int -> int -> int = "my_add"

let result = my_add 1 2  (* calls the JS function directly *)
```
This is efficient and integrates naturally with OCaml code.


### Non-function values: use `runtime_value`

For JavaScript **objects, constants, or other non-function values**, use [Js.Unsafe.runtime\_value](./Js_of_ocaml-Js-Unsafe.md#val-runtime_value):

```javascript
//Provides: myConfig
var myConfig = { debug: true, version: 42 };
```
```ocaml
let config : < debug : bool Js.t Js.prop; version : int Js.prop > Js.t =
  Js.Unsafe.runtime_value "myConfig"
```
**Important**: The argument must be a string literal, not a variable.


## See also

- [PPX syntax](./ppx.md) \- Syntax reference: `##.`, `##`, `new%js`, `object%js`
- [Exporting to JavaScript](./rev-bindings.md) \- Make OCaml code callable from JavaScript
- [Js](./Js_of_ocaml-Js.md) \- Core JavaScript bindings
- [Dom\_html](./Js_of_ocaml-Dom_html.md) \- HTML DOM bindings