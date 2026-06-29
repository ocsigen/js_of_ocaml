
# Class type `Cache.cache`

A single named [`Cache`](./Js_of_ocaml-Cache.md) instance, obtained from [`cacheStorage`](./Js_of_ocaml-Cache-class-type-cacheStorage.md).

The methods come in two flavours: the `_url` variants take the request as a URL string, while the others take a [Request](./Js_of_ocaml-Fetch.md#request).

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
  queryOptions Js_of_ocaml__.Js.t ->
  Js_of_ocaml__.Fetch.response Js_of_ocaml__.Js.t Js_of_ocaml__.Js.optdef
    Js_of_ocaml__.Promise.t
    Js_of_ocaml__.Js.meth
```
```ocaml
method matchAll : Js_of_ocaml__.Fetch.response Js_of_ocaml__.Js.t
                    Js_of_ocaml__.Js.js_array
                    Js_of_ocaml__.Js.t
                    Js_of_ocaml__.Promise.t
                    Js_of_ocaml__.Js.meth
```
```ocaml
method matchAll_request : Js_of_ocaml__.Fetch.request Js_of_ocaml__.Js.t ->
  Js_of_ocaml__.Fetch.response Js_of_ocaml__.Js.t Js_of_ocaml__.Js.js_array
    Js_of_ocaml__.Js.t
    Js_of_ocaml__.Promise.t
    Js_of_ocaml__.Js.meth
```
```ocaml
method matchAll_withOptions : Js_of_ocaml__.Fetch.request Js_of_ocaml__.Js.t ->
  queryOptions Js_of_ocaml__.Js.t ->
  Js_of_ocaml__.Fetch.response Js_of_ocaml__.Js.t Js_of_ocaml__.Js.js_array
    Js_of_ocaml__.Js.t
    Js_of_ocaml__.Promise.t
    Js_of_ocaml__.Js.meth
```
```ocaml
method add : Js_of_ocaml__.Fetch.request Js_of_ocaml__.Js.t ->
  unit Js_of_ocaml__.Promise.t Js_of_ocaml__.Js.meth
```
`cache##add req` fetches `req` and stores the resulting response. The promise rejects if the response does not have an ok status.

```ocaml
method add_url : Js_of_ocaml__.Js.js_string Js_of_ocaml__.Js.t ->
  unit Js_of_ocaml__.Promise.t Js_of_ocaml__.Js.meth
```
```ocaml
method addAll : Js_of_ocaml__.Fetch.request Js_of_ocaml__.Js.t
                  Js_of_ocaml__.Js.js_array
                  Js_of_ocaml__.Js.t ->
  unit Js_of_ocaml__.Promise.t Js_of_ocaml__.Js.meth
```
```ocaml
method addAll_url : Js_of_ocaml__.Js.js_string Js_of_ocaml__.Js.t
                      Js_of_ocaml__.Js.js_array
                      Js_of_ocaml__.Js.t ->
  unit Js_of_ocaml__.Promise.t Js_of_ocaml__.Js.meth
```
```ocaml
method put : Js_of_ocaml__.Fetch.request Js_of_ocaml__.Js.t ->
  Js_of_ocaml__.Fetch.response Js_of_ocaml__.Js.t ->
  unit Js_of_ocaml__.Promise.t Js_of_ocaml__.Js.meth
```
```ocaml
method put_url : Js_of_ocaml__.Js.js_string Js_of_ocaml__.Js.t ->
  Js_of_ocaml__.Fetch.response Js_of_ocaml__.Js.t ->
  unit Js_of_ocaml__.Promise.t Js_of_ocaml__.Js.meth
```
```ocaml
method delete : Js_of_ocaml__.Fetch.request Js_of_ocaml__.Js.t ->
  bool Js_of_ocaml__.Js.t Js_of_ocaml__.Promise.t Js_of_ocaml__.Js.meth
```
`cache##delete req` resolves with `true` if a matching entry was found and removed, `false` otherwise.

```ocaml
method delete_url : Js_of_ocaml__.Js.js_string Js_of_ocaml__.Js.t ->
  bool Js_of_ocaml__.Js.t Js_of_ocaml__.Promise.t Js_of_ocaml__.Js.meth
```
```ocaml
method delete_withOptions : Js_of_ocaml__.Fetch.request Js_of_ocaml__.Js.t ->
  queryOptions Js_of_ocaml__.Js.t ->
  bool Js_of_ocaml__.Js.t Js_of_ocaml__.Promise.t Js_of_ocaml__.Js.meth
```
```ocaml
method keys : Js_of_ocaml__.Fetch.request Js_of_ocaml__.Js.t
                Js_of_ocaml__.Js.js_array
                Js_of_ocaml__.Js.t
                Js_of_ocaml__.Promise.t
                Js_of_ocaml__.Js.meth
```
```ocaml
method keys_request : Js_of_ocaml__.Fetch.request Js_of_ocaml__.Js.t ->
  Js_of_ocaml__.Fetch.request Js_of_ocaml__.Js.t Js_of_ocaml__.Js.js_array
    Js_of_ocaml__.Js.t
    Js_of_ocaml__.Promise.t
    Js_of_ocaml__.Js.meth
```