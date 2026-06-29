
# Class type `Cache.cacheStorage`

The set of named [`Cache`](./Js_of_ocaml-Cache.md)s for the current origin, exposed as the global `caches` (see [`caches`](./Js_of_ocaml-Cache.md#val-caches)).

```ocaml
method match_ : Js_of_ocaml__.Fetch.request Js_of_ocaml__.Js.t ->
  Js_of_ocaml__.Fetch.response Js_of_ocaml__.Js.t Js_of_ocaml__.Js.optdef
    Js_of_ocaml__.Promise.t
    Js_of_ocaml__.Js.meth
```
```ocaml
method match_url : Js_of_ocaml__.Js.js_string Js_of_ocaml__.Js.t ->
  Js_of_ocaml__.Fetch.response Js_of_ocaml__.Js.t Js_of_ocaml__.Js.optdef
    Js_of_ocaml__.Promise.t
    Js_of_ocaml__.Js.meth
```
```ocaml
method match_withOptions : Js_of_ocaml__.Fetch.request Js_of_ocaml__.Js.t ->
  multiQueryOptions Js_of_ocaml__.Js.t ->
  Js_of_ocaml__.Fetch.response Js_of_ocaml__.Js.t Js_of_ocaml__.Js.optdef
    Js_of_ocaml__.Promise.t
    Js_of_ocaml__.Js.meth
```
```ocaml
method has : Js_of_ocaml__.Js.js_string Js_of_ocaml__.Js.t ->
  bool Js_of_ocaml__.Js.t Js_of_ocaml__.Promise.t Js_of_ocaml__.Js.meth
```
```ocaml
method open_ : Js_of_ocaml__.Js.js_string Js_of_ocaml__.Js.t ->
  cache Js_of_ocaml__.Js.t Js_of_ocaml__.Promise.t Js_of_ocaml__.Js.meth
```
`caches##open_ name` resolves with the `cache` named `name`, creating it if it does not yet exist.

```ocaml
method delete : Js_of_ocaml__.Js.js_string Js_of_ocaml__.Js.t ->
  bool Js_of_ocaml__.Js.t Js_of_ocaml__.Promise.t Js_of_ocaml__.Js.meth
```
```ocaml
method keys : Js_of_ocaml__.Js.js_string Js_of_ocaml__.Js.t
                Js_of_ocaml__.Js.js_array
                Js_of_ocaml__.Js.t
                Js_of_ocaml__.Promise.t
                Js_of_ocaml__.Js.meth
```