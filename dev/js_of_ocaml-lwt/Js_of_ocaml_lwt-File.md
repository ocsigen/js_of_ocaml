
# Module `Js_of_ocaml_lwt.File`

```ocaml
class type  blob = object ... end
```
```ocaml
type 'a make_blob =
  ?contentType:string ->
  ?endings:[ `Transparent | `Native ] ->
  'a ->
  blob Js_of_ocaml.Js.t
```
```ocaml
val blob_from_string : string make_blob
```
```ocaml
val blob_from_any : 
  [ `blob of blob Js_of_ocaml.Js.t
  | `arrayBuffer of Js_of_ocaml.Typed_array.arrayBuffer Js_of_ocaml.Js.t
  | `arrayBufferView of
    Js_of_ocaml.Typed_array.arrayBufferView Js_of_ocaml.Js.t
  | `string of string
  | `js_string of Js_of_ocaml.Js.js_string Js_of_ocaml.Js.t ]
    list
    make_blob
```
```ocaml
class type  file = object ... end
```
```ocaml
type file_any = Js_of_ocaml.File.file_any
```
```ocaml
module CoerceTo = Js_of_ocaml.File.CoerceTo
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
type readyState = Js_of_ocaml.File.readyState = 
  | EMPTY
  | LOADING
  | DONE
```
```ocaml
class type  fileReader = object ... end
```
```ocaml
module ReaderEvent = Js_of_ocaml.File.ReaderEvent
```
```ocaml
val filename : 
  file Js_of_ocaml.Js.t ->
  Js_of_ocaml.Js.js_string Js_of_ocaml.Js.t
```
`filename` handles old firefox without name property

```ocaml
val fileReader : fileReader Js_of_ocaml.Js.t Js_of_ocaml.Js.constr
```
```ocaml
val addEventListener : 
  (progressEventTarget Js_of_ocaml.Js.t as 'a) ->
  'b Js_of_ocaml.Dom.Event.typ ->
  ('a, 'b) Js_of_ocaml.Dom.event_listener ->
  bool Js_of_ocaml.Js.t ->
  Js_of_ocaml.Dom.event_listener_id
```
Add an event listener. This function matches the `addEventListener` DOM method, except that it returns an id for removing the listener.

```ocaml
val readAsBinaryString : 
  Js_of_ocaml.File.blob Js_of_ocaml.Js.t ->
  Js_of_ocaml.Js.js_string Js_of_ocaml.Js.t Lwt.t
```
```ocaml
val readAsText : 
  Js_of_ocaml.File.blob Js_of_ocaml.Js.t ->
  Js_of_ocaml.Js.js_string Js_of_ocaml.Js.t Lwt.t
```
```ocaml
val readAsText_withEncoding : 
  Js_of_ocaml.File.blob Js_of_ocaml.Js.t ->
  Js_of_ocaml.Js.js_string Js_of_ocaml.Js.t ->
  Js_of_ocaml.Js.js_string Js_of_ocaml.Js.t Lwt.t
```
```ocaml
val readAsDataURL : 
  Js_of_ocaml.File.blob Js_of_ocaml.Js.t ->
  Js_of_ocaml.Js.js_string Js_of_ocaml.Js.t Lwt.t
```