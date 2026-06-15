
# Module `Js_of_ocaml.Dom_events`

Javascript events

```ocaml
module Typ : sig ... end
```
```ocaml
type listener
```
```ocaml
val listen : 
  ?capture:bool ->
  (Dom_html.eventTarget as 'a) Js.t ->
  (Dom_html.event as 'b) Js.t Typ.typ ->
  ('a Js.t -> 'b Js.t -> bool) ->
  listener
```
```ocaml
val stop_listen : listener -> unit
```