
# Module `Js_of_ocaml.XmlHttpRequest`

XmlHttpRequest object.

```ocaml
type readyState = 
  | UNSENT
  | OPENED
  | HEADERS_RECEIVED
  | LOADING
  | DONE
```
```ocaml
type _ response = 
  | ArrayBuffer : Typed_array.arrayBuffer Js.t Js.Opt.t response
  | Blob : File.blob Js.t Js.Opt.t response
  | Document : Dom.element Dom.document Js.t Js.Opt.t response
  | JSON : 'a Js.Opt.t response
  | Text : Js.js_string Js.t response
  | Default : string response
```
```ocaml
class type  xmlHttpRequest = object ... end
```
```ocaml
class type  xmlHttpRequestUpload = object ... end
```
```ocaml
val create : unit -> xmlHttpRequest Js.t
```
The next part of this module allow one to use Ocaml with no need for Javascript documentation.

```ocaml
module Event : sig ... end
```