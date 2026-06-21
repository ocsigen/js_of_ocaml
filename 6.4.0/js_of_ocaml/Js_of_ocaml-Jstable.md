
# Module `Js_of_ocaml.Jstable`

A minimal table implementation specialized for [`Js.js_string`](./Js_of_ocaml-Js-class-type-js_string.md) keys. This is faster than regular OCaml hashtables.

This implementation does not emulate the backtracking behavior of `Stdlib.Hashtbl`.

```ocaml
type 'a t
```
```ocaml
val create : unit -> 'a t
```
```ocaml
val add : 'a t -> Js.js_string Js.t -> 'a -> unit
```
```ocaml
val remove : 'a t -> Js.js_string Js.t -> unit
```
```ocaml
val find : 'a t -> Js.js_string Js.t -> 'a Js.optdef
```
```ocaml
val keys : 'a t -> Js.js_string Js.t list
```