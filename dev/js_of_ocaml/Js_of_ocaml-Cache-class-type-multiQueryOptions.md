
# Class type `Cache.multiQueryOptions`

Like [`queryOptions`](./Js_of_ocaml-Cache-class-type-queryOptions.md) but with a `cacheName` to restrict [`cacheStorage`](./Js_of_ocaml-Cache-class-type-cacheStorage.md)`##match_withOptions` to a single named cache. Create an empty record with [`empty_multi_query_options`](./Js_of_ocaml-Cache.md#val-empty_multi_query_options).

```ocaml
inherit queryOptions
```
```ocaml
method cacheName : Js_of_ocaml__.Js.js_string Js_of_ocaml__.Js.t
                     Js_of_ocaml__.Js.writeonly_prop
```