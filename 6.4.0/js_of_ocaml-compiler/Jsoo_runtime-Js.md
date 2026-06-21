
# Module `Jsoo_runtime.Js`

```ocaml
type t
```
```ocaml
type 'a js_array = t
```
```ocaml
type ('a, 'b) meth_callback = t
```
```ocaml
val string : string -> t
```
```ocaml
val to_string : t -> string
```
```ocaml
val bytestring : string -> t
```
```ocaml
val to_bytestring : t -> string
```
```ocaml
val bool : bool -> t
```
```ocaml
val to_bool : t -> bool
```
```ocaml
val array : 'a array -> t
```
```ocaml
val to_array : t -> 'a array
```
```ocaml
val number_of_float : float -> t
```
```ocaml
val float_of_number : t -> float
```
```ocaml
val number_of_int32 : int32 -> t
```
```ocaml
val int32_of_number : t -> int32
```
```ocaml
val number_of_nativeint : nativeint -> t
```
```ocaml
val nativeint_of_number : t -> nativeint
```
```ocaml
val typeof : t -> t
```
```ocaml
val instanceof : t -> t -> bool
```
```ocaml
val debugger : unit -> unit
```
```ocaml
val get : t -> t -> t
```
```ocaml
val set : t -> t -> t -> unit
```
```ocaml
val delete : t -> t -> unit
```
```ocaml
val call : t -> t -> t array -> t
```
```ocaml
val fun_call : t -> t array -> t
```
```ocaml
val meth_call : t -> string -> t array -> t
```
```ocaml
val new_obj : t -> t array -> t
```
```ocaml
val new_obj_arr : t -> t js_array -> t
```
```ocaml
val obj : (string * t) array -> t
```
```ocaml
val equals : t -> t -> bool
```
```ocaml
val strict_equals : t -> t -> bool
```
```ocaml
val pure_expr : (unit -> 'a) -> 'a
```
```ocaml
val eval_string : string -> 'a
```
```ocaml
val js_expr : string -> 'a
```
```ocaml
val pure_js_expr : string -> 'a
```
```ocaml
val callback : ('b -> 'a) -> ('b, 'a) meth_callback
```
```ocaml
val callback_with_arguments : 
  (t js_array -> 'b) ->
  ('c, t js_array -> 'b) meth_callback
```
```ocaml
val callback_with_arity : int -> ('a -> 'b) -> ('c, 'a -> 'b) meth_callback
```
```ocaml
val meth_callback : ('b -> 'a) -> ('b, 'a) meth_callback
```
```ocaml
val meth_callback_with_arity : int -> ('b -> 'a) -> ('b, 'a) meth_callback
```
```ocaml
val meth_callback_with_arguments : 
  ('b -> t js_array -> 'a) ->
  ('b, t js_array -> 'a) meth_callback
```
```ocaml
val wrap_callback : ('a -> 'b) -> ('c, 'a -> 'b) meth_callback
```
```ocaml
val wrap_meth_callback : ('a -> 'b) -> ('a, 'b) meth_callback
```
```ocaml
val runtime_value : string -> 'a
```
`runtime_value "FOO"` returns the JavaScript value FOO provided by the JavaScript runtime (with '//Provides: FOO'). The string argument must be a string literal.

```ocaml
val custom_identifier : Stdlib.Obj.t -> string
```