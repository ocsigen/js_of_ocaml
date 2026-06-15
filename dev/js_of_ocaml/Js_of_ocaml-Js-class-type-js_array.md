
# Class type `Js.js_array`

Specification of Javascript regular arrays. Use `Js.array_get` and `Js.array_set` to access and set array elements.

```ocaml
method toString : js_string t meth
```
```ocaml
method toLocaleString : js_string t meth
```
```ocaml
method concat : 'a js_array t -> 'a js_array t meth
```
```ocaml
method join : js_string t -> js_string t meth
```
```ocaml
method pop : 'a optdef meth
```
```ocaml
method push : 'a -> int meth
```
```ocaml
method push_2 : 'a -> 'a -> int meth
```
```ocaml
method push_3 : 'a -> 'a -> 'a -> int meth
```
```ocaml
method push_4 : 'a -> 'a -> 'a -> 'a -> int meth
```
```ocaml
method reverse : 'a js_array t meth
```
```ocaml
method shift : 'a optdef meth
```
```ocaml
method slice : int -> int -> 'a js_array t meth
```
```ocaml
method slice_end : int -> 'a js_array t meth
```
```ocaml
method sort : ('a -> 'a -> number_t) callback -> 'a js_array t meth
```
```ocaml
method sort_asStrings : 'a js_array t meth
```
```ocaml
method splice : int -> int -> 'a js_array t meth
```
```ocaml
method splice_1 : int -> int -> 'a -> 'a js_array t meth
```
```ocaml
method splice_2 : int -> int -> 'a -> 'a -> 'a js_array t meth
```
```ocaml
method splice_3 : int -> int -> 'a -> 'a -> 'a -> 'a js_array t meth
```
```ocaml
method splice_4 : int -> int -> 'a -> 'a -> 'a -> 'a -> 'a js_array t meth
```
```ocaml
method unshift : 'a -> int meth
```
```ocaml
method unshift_2 : 'a -> 'a -> int meth
```
```ocaml
method unshift_3 : 'a -> 'a -> 'a -> int meth
```
```ocaml
method unshift_4 : 'a -> 'a -> 'a -> 'a -> int meth
```
```ocaml
method some : ('a -> int -> 'a js_array t -> bool t) callback -> bool t meth
```
```ocaml
method every : ('a -> int -> 'a js_array t -> bool t) callback -> bool t meth
```
```ocaml
method forEach : ('a -> int -> 'a js_array t -> unit) callback -> unit meth
```
```ocaml
method map : ('a -> int -> 'a js_array t -> 'b) callback -> 'b js_array t meth
```
```ocaml
method filter : ('a -> int -> 'a js_array t -> bool t) callback ->
  'a js_array t meth
```
```ocaml
method reduce_init : ('b -> 'a -> int -> 'a js_array t -> 'b) callback ->
  'b ->
  'b meth
```
```ocaml
method reduce : ('a -> 'a -> int -> 'a js_array t -> 'a) callback -> 'a meth
```
```ocaml
method reduceRight_init : ('b -> 'a -> int -> 'a js_array t -> 'b) callback ->
  'b ->
  'b meth
```
```ocaml
method reduceRight : ('a -> 'a -> int -> 'a js_array t -> 'a) callback ->
  'a meth
```
```ocaml
method length : int prop
```