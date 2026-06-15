
# Module `Js_of_ocaml.MutationObserver`

MutationObserver API

A code example:

```ocaml
  if (MutationObserver.is_supported()) then
    let doc = Dom_html.document in
    let target =
      Js.Opt.get (doc##getElementById (Js.string "observed"))
        (fun () -> assert false)
    in
    let node = (target :> Dom.node Js.t) in
    let f records observer =
      Console.console##debug records ;
      Console.console##debug observer
    in
    MutationObserver.observe ~node ~f
      ~attributes:true ~child_list:true ~character_data:true
      ()
```
see [https://developer.mozilla.org/en-US/docs/Web/API/MutationObserver](https://developer.mozilla.org/en-US/docs/Web/API/MutationObserver) for API documentation.
see [https://dom.spec.whatwg.org/\#mutation-observers](https://dom.spec.whatwg.org/#mutation-observers) for the Web Hypertext Application Technology Working Group (WHATWG) spec.
```ocaml
class type  mutationObserverInit = object ... end
```
```ocaml
class type  mutationRecord = object ... end
```
```ocaml
class type  mutationObserver = object ... end
```
```ocaml
val empty_mutation_observer_init : unit -> mutationObserverInit Js.t
```
```ocaml
val mutationObserver : 
  ((mutationRecord Js.t Js.js_array Js.t ->
     mutationObserver Js.t ->
     unit)
     Js.callback ->
    mutationObserver Js.t)
    Js.constr
```
```ocaml
val is_supported : unit -> bool
```
```ocaml
val observe : 
  node:Dom.node Js.t ->
  f:(mutationRecord Js.t Js.js_array Js.t -> mutationObserver Js.t -> unit) ->
  ?child_list:bool ->
  ?attributes:bool ->
  ?character_data:bool ->
  ?subtree:bool ->
  ?attribute_old_value:bool ->
  ?character_data_old_value:bool ->
  ?attribute_filter:Js.js_string Js.t list ->
  unit ->
  mutationObserver Js.t
```
Helper to create a new observer and connect it to a node
