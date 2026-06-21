
# Module `Js.Unsafe`

Unsafe Javascript operations

```ocaml
type top
```
```ocaml
type any = top t
```
Top type. Used for putting values of different types in a same array.

```ocaml
type any_js_array = any
```
```ocaml
val inject : 'a -> any
```
Coercion to top type.

```ocaml
val coerce : _ t -> _ t
```
Unsafe coercion between to Javascript objects.

```ocaml
val get : 'a -> 'b -> 'c
```
Get the value of an object property. The expression `get o s` returns the value of property `s` of object `o`.

```ocaml
val set : 'a -> 'b -> 'c -> unit
```
Set an object property. The expression `set o s v` set the property `s` of object `o` to value `v`.

```ocaml
val delete : 'a -> 'b -> unit
```
Delete an object property. The expression `delete o s` deletes property `s` of object `o`.

```ocaml
val call : 'a -> 'b -> any array -> 'c
```
Performs a Javascript function call. The expression `call f o a` calls the Javascript function `f` with the arguments given by the array `a`, and binding `this` to `o`.

```ocaml
val fun_call : 'a -> any array -> 'b
```
Performs a Javascript function call. The expression `fun_call f a` calls the Javascript function `f` with the arguments given by the array `a`.

```ocaml
val meth_call : 'a -> string -> any array -> 'b
```
Performs a Javascript method call. The expression `meth_call o m a` calls the Javascript method `m` of object `o` with the arguments given by the array `a`.

```ocaml
val new_obj : 'a -> any array -> 'b
```
Create a Javascript object. The expression `new_obj c a` creates a Javascript object with constructor `c` using the arguments given by the array `a`. Example: `Js.new_obj (Js.Unsafe.variable "ArrayBuffer") [||]`

```ocaml
val new_obj_arr : 'a -> any_js_array -> 'b
```
Same Create a Javascript object. The expression `new_obj_arr c a` creates a Javascript object with constructor `c` using the arguments given by the Javascript array `a`.

```ocaml
val obj : (string * any) array -> 'a
```
Creates a Javascript literal object. The expression `obj a` creates a Javascript object whose fields are given by the array `a`

```ocaml
val pure_expr : (unit -> 'a) -> 'a
```
Asserts that an expression is pure, and can therefore be optimized away by the compiler if unused.

```ocaml
val eval_string : string -> 'a
```
Evaluate Javascript code

```ocaml
val js_expr : string -> 'a
```
`js_expr e` will parse the JavaScript expression `e` if `e` is available at compile time or will fallback to a runtime evaluation. See `eval_string`

```ocaml
val pure_js_expr : string -> 'a
```
`pure_js_expr str` behaves like `pure_expr (fun () -> js_expr str)`.

```ocaml
val runtime_value : string -> 'a
```
`runtime_value "FOO"` returns the JavaScript value FOO provided by the JavaScript runtime (with '//Provides: FOO'). The string argument must be a string literal.

```ocaml
val global : < .. > t
```
Javascript global object

```ocaml
val callback : ('a -> 'b) -> ('c, 'a -> 'b) meth_callback
```
Wrap an OCaml function so that it can be invoked from Javascript. Contrary to `Js.wrap_callback`, partial application and over-application are not supported: missing arguments will be set to `undefined` and extra arguments are lost.

```ocaml
val callback_with_arguments : 
  (any_js_array -> 'b) ->
  ('c, any_js_array -> 'b) meth_callback
```
Wrap an OCaml function so that it can be invoked from Javascript. The first parameter of the function will be bound to the `arguments` JavaScript

```ocaml
val callback_with_arity : int -> ('a -> 'b) -> ('c, 'a -> 'b) meth_callback
```
```ocaml
val meth_callback : ('b -> 'a) -> ('b, 'a) meth_callback
```
Wrap an OCaml function so that it can be invoked from Javascript. The first parameter of the function will be bound to the value of the `this` implicit parameter. Contrary to `Js.wrap_meth_callback`, partial application and over-application is not supported: missing arguments will be set to `undefined` and extra arguments are lost.

```ocaml
val meth_callback_with_arguments : 
  ('b -> any_js_array -> 'a) ->
  ('b, any_js_array -> 'a) meth_callback
```
Wrap an OCaml function so that it can be invoked from Javascript. The first parameter of the function will be bound to the value of the `this` implicit parameter. The second parameter of the function with be bound to the value of the `arguments`.

```ocaml
val meth_callback_with_arity : int -> ('b -> 'a) -> ('b, 'a) meth_callback
```
```ocaml
val equals : _ -> _ -> bool
```
Javascript `==` equality operator.

```ocaml
val strict_equals : _ -> _ -> bool
```
Javascript `===` equality operator.


#### Deprecated functions.

```ocaml
val variable : string -> 'a
```
Access a Javascript variable. `variable "foo"` will return the current value of variable `foo`.

deprecated \[since 2.6\] use Js.Unsafe.pure\_js\_expr instead