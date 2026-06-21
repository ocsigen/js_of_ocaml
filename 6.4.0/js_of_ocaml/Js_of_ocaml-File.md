
# Module `Js_of_ocaml.File`

File API

see [https://developer.mozilla.org/en-US/docs/Web/API/File](https://developer.mozilla.org/en-US/docs/Web/API/File) the documentation of the API.
```ocaml
class type  blob = object ... end
```
```ocaml
type 'a make_blob =
  ?contentType:string ->
  ?endings:[ `Transparent | `Native ] ->
  'a ->
  blob Js.t
```
```ocaml
val blob_from_string : string make_blob
```
```ocaml
val blob_from_any : 
  [ `blob of blob Js.t
  | `arrayBuffer of Typed_array.arrayBuffer Js.t
  | `arrayBufferView of Typed_array.arrayBufferView Js.t
  | `string of string
  | `js_string of Js.js_string Js.t ]
    list
    make_blob
```
```ocaml
class type  file = object ... end
```
```ocaml
type file_any
```
```ocaml
module CoerceTo : sig ... end
```
```ocaml
class type  fileList = object ... end
```
```ocaml
class type  fileError = object ... end
```
```ocaml
class type 'a progressEvent = object ... end
```
```ocaml
class type  progressEventTarget = object ... end
```
```ocaml
type readyState = 
  | EMPTY
  | LOADING
  | DONE
```
```ocaml
class type  fileReader = object ... end
```
```ocaml
module ReaderEvent : sig ... end
```
```ocaml
val filename : file Js.t -> Js.js_string Js.t
```
`filename file` is the name of `file` (equivalent to `file##.name`).

```ocaml
val fileReader : fileReader Js.t Js.constr
```
```ocaml
val addEventListener : 
  (progressEventTarget Js.t as 'a) ->
  'b Dom.Event.typ ->
  ('a, 'b) Dom.event_listener ->
  bool Js.t ->
  Dom.event_listener_id
```
Add an event listener. This function matches the `addEventListener` DOM method, except that it returns an id for removing the listener.
