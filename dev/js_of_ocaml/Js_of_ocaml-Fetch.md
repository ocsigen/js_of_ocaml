
# Module `Js_of_ocaml.Fetch`

Fetch API.

see [https://developer.mozilla.org/en-US/docs/Web/API/Fetch\_API](https://developer.mozilla.org/en-US/docs/Web/API/Fetch_API) 
see [https://fetch.spec.whatwg.org/](https://fetch.spec.whatwg.org/) 

## Headers

```ocaml
class type  headers = object ... end
```
```ocaml
val headers : headers Js.t Js.constr
```
```ocaml
val headers_of_list : (string * string) list -> headers Js.t
```
Build a `headers` object from a list of `(name, value)` pairs.


## Request

```ocaml
class type  body = object ... end
```
The body-reader methods are Promise-typed — see [`Promise`](./Js_of_ocaml-Promise.md).

```ocaml
class type  requestInit = object ... end
```
Initializer for [`request`](./Js_of_ocaml-Fetch-class-type-request.md) (and [`fetch_with_init`](./#val-fetch_with_init)). All fields are optional; create an empty record with [`empty_request_init`](./#val-empty_request_init) and populate the ones you need.

```ocaml
val empty_request_init : unit -> requestInit Js.t
```
```ocaml
class type  request = object ... end
```
```ocaml
val request : (Js.js_string Js.t -> request Js.t) Js.constr
```
```ocaml
val request_with_init : 
  (Js.js_string Js.t -> requestInit Js.t -> request Js.t) Js.constr
```
```ocaml
val request_of_request : (request Js.t -> request Js.t) Js.constr
```
```ocaml
val request_of_request_with_init : 
  (request Js.t -> requestInit Js.t -> request Js.t) Js.constr
```
`new%js request_of_request_with_init r init` builds a request using `r` as a template, overriding fields set in `init`.


## Response

```ocaml
class type  responseInit = object ... end
```
```ocaml
val empty_response_init : unit -> responseInit Js.t
```
```ocaml
class type  response = object ... end
```
```ocaml
val response : (Js.Unsafe.any -> response Js.t) Js.constr
```
```ocaml
val response_with_init : 
  (Js.Unsafe.any -> responseInit Js.t -> response Js.t) Js.constr
```

## fetch

```ocaml
val fetch : Js.js_string Js.t -> response Js.t Promise.t
```
```ocaml
val fetch_with_init : 
  Js.js_string Js.t ->
  requestInit Js.t ->
  response Js.t Promise.t
```
```ocaml
val fetch_request : request Js.t -> response Js.t Promise.t
```
```ocaml
val is_supported : unit -> bool
```
Whether the `fetch` global is available in the current environment.
