
# Module `Js_of_ocaml.WebSockets`

WebSocket binding

```ocaml
type readyState = 
  | CONNECTING
  | OPEN
  | CLOSING
  | CLOSED
```
```ocaml
class type 'a closeEvent = object ... end
```
```ocaml
class type 'a messageEvent = object ... end
```
```ocaml
class type  webSocket = object ... end
```
```ocaml
val webSocket : (Js.js_string Js.t -> webSocket Js.t) Js.constr
```
```ocaml
val webSocket_withProtocol : 
  (Js.js_string Js.t -> Js.js_string Js.t -> webSocket Js.t) Js.constr
```
```ocaml
val webSocket_withProtocols : 
  (Js.js_string Js.t ->
    Js.js_string Js.t Js.js_array Js.t ->
    webSocket Js.t)
    Js.constr
```
```ocaml
val is_supported : unit -> bool
```