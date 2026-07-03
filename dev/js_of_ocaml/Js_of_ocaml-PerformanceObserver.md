
# Module `Js_of_ocaml.PerformanceObserver`

PerformanceObserver API

A code example:

```ocaml
  if (PerformanceObserver.is_supported()) then
    let entry_types = [ "measure" ] in
    let f entries observer =
      let entries = entries##getEntries in
      Console.console##debug entries ;
      Console.console##debug observer
    in
    ignore (PerformanceObserver.observe ~entry_types ~f)
```
see [https://developer.mozilla.org/en-US/docs/Web/API/PerformanceObserver](https://developer.mozilla.org/en-US/docs/Web/API/PerformanceObserver) for API documentation.
```ocaml
class type  performanceObserverInit = object ... end
```
```ocaml
class type  performanceEntry = object ... end
```
```ocaml
class type  performanceObserverEntryList = object ... end
```
```ocaml
class type  performanceObserver = object ... end
```
```ocaml
val performanceObserver : 
  ((performanceObserverEntryList Js.t ->
     performanceObserver Js.t ->
     unit)
     Js.callback ->
    performanceObserver Js.t)
    Js.constr
```
```ocaml
val is_supported : unit -> bool
```
```ocaml
val observe : 
  entry_types:string list ->
  f:(performanceObserverEntryList Js.t -> performanceObserver Js.t -> unit) ->
  performanceObserver Js.t
```