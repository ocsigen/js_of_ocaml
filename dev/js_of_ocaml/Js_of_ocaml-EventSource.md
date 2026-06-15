
# Module `Js_of_ocaml.EventSource`

EventSource binding

```ocaml
type state = 
  | CONNECTING
  | OPEN
  | CLOSED
```
```ocaml
class type 'a messageEvent = object ... end
```
```ocaml
class type  eventSource = object ... end
```
```ocaml
class type  options = object ... end
```
```ocaml
val withCredentials : bool -> options Js.t
```
```ocaml
val eventSource : (Js.js_string Js.t -> eventSource Js.t) Js.constr
```
```ocaml
val eventSource_options : 
  (Js.js_string Js.t -> options Js.t -> eventSource Js.t) Js.constr
```
```ocaml
val addEventListener : 
  (eventSource Js.t as 'a) ->
  'b Dom.Event.typ ->
  ('a, 'b) Dom.event_listener ->
  bool Js.t ->
  Dom.event_listener_id
```
Add an event listener. This function matches the `addEventListener` DOM method, except that it returns an id for removing the listener.
