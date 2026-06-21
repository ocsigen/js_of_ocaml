
# Module `Dom_html.DomStringMap`

Typed access to [`domStringMap`](./Js_of_ocaml-Dom_html.md#type-domStringMap) (the type of `Element.dataset`).

```ocaml
val get : domStringMap Js.t -> Js.js_string Js.t -> Js.js_string Js.t Js.optdef
```
`get m k` is the value stored under key `k`, or `undefined` if absent.

```ocaml
val set : domStringMap Js.t -> Js.js_string Js.t -> Js.js_string Js.t -> unit
```
`set m k v` sets `k` to `v`.

```ocaml
val remove : domStringMap Js.t -> Js.js_string Js.t -> unit
```
`remove m k` deletes the `k` entry.
