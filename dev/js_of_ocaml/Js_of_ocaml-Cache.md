
# Module `Js_of_ocaml.Cache`

Cache API: persistent storage of [`Fetch`](./Js_of_ocaml-Fetch.md) `Request`/`Response` pairs.

Available from [`Dom_html.window`](./Js_of_ocaml-Dom_html-class-type-window.md) and from [`ServiceWorker`](./Js_of_ocaml-ServiceWorker.md) contexts; the Cache API requires a secure context. The methods are Promise-typed — see [`Promise`](./Js_of_ocaml-Promise.md).

see [https://developer.mozilla.org/en-US/docs/Web/API/Cache](https://developer.mozilla.org/en-US/docs/Web/API/Cache) 
see [https://developer.mozilla.org/en-US/docs/Web/API/CacheStorage](https://developer.mozilla.org/en-US/docs/Web/API/CacheStorage) 
see [https://w3c.github.io/ServiceWorker/\#cache-interface](https://w3c.github.io/ServiceWorker/#cache-interface) 
```ocaml
class type  queryOptions = object ... end
```
Options controlling how a request is matched against cached entries. Create an empty record with [`empty_query_options`](./#val-empty_query_options).

```ocaml
val empty_query_options : unit -> queryOptions Js.t
```
```ocaml
class type  multiQueryOptions = object ... end
```
Like [`queryOptions`](./Js_of_ocaml-Cache-class-type-queryOptions.md) but with a `cacheName` to restrict [`cacheStorage`](./Js_of_ocaml-Cache-class-type-cacheStorage.md)`##match_withOptions` to a single named cache. Create an empty record with [`empty_multi_query_options`](./#val-empty_multi_query_options).

```ocaml
val empty_multi_query_options : unit -> multiQueryOptions Js.t
```
```ocaml
class type  cache = object ... end
```
A single named [`Cache`](#) instance, obtained from [`cacheStorage`](./Js_of_ocaml-Cache-class-type-cacheStorage.md).

```ocaml
class type  cacheStorage = object ... end
```
The set of named [`Cache`](#)s for the current origin, exposed as the global `caches` (see [`caches`](./#val-caches)).

```ocaml
val caches : unit -> cacheStorage Js.t
```
The `caches` global (`CacheStorage`) for the current origin. Guard with [`is_supported`](./#val-is_supported) in environments where the Cache API may be missing.

```ocaml
val is_supported : unit -> bool
```
Whether the `caches` global is available in the current environment.
