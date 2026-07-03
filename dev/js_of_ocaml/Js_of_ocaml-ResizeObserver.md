
# Module `Js_of_ocaml.ResizeObserver`

ResizeObserver API

A code example:

```ocaml
  if (ResizeObserver.is_supported ()) then
    let doc = Dom_html.document in
    let target =
      Js.Opt.get (doc##getElementById (Js.string "observed"))
        (fun () -> assert false)
    in
    let node = (target :> Dom.node Js.t) in
    let f entries observer =
      Console.console##debug entries;
      Console.console##debug observer
    in
    ignore
      (ResizeObserver.observe ~node ~f
         ~box:(Js.string "content-box")
         ())
```
see [https://developer.mozilla.org/en-US/docs/Web/API/ResizeObserver](https://developer.mozilla.org/en-US/docs/Web/API/ResizeObserver) for API documentation
see [https://drafts.csswg.org/resize-observer](https://drafts.csswg.org/resize-observer) for W3C draft spec
```ocaml
class type  resizeObserverSize = object ... end
```
```ocaml
class type  resizeObserverEntry = object ... end
```
```ocaml
class type  resizeObserverOptions = object ... end
```
```ocaml
class type  resizeObserver = object ... end
```
```ocaml
val empty_resize_observer_options : unit -> resizeObserverOptions Js.t
```
```ocaml
val resizeObserver : 
  ((resizeObserverEntry Js.t Js.js_array Js.t ->
     resizeObserver Js.t ->
     unit)
     Js.callback ->
    resizeObserver Js.t)
    Js.constr
```
```ocaml
val is_supported : unit -> bool
```
```ocaml
val observe : 
  node:Dom.node Js.t ->
  f:(resizeObserverEntry Js.t Js.js_array Js.t -> resizeObserver Js.t -> unit) ->
  ?box:Js.js_string Js.t ->
  unit ->
  resizeObserver Js.t
```
Helper to create a new observer and connect it to a node.
