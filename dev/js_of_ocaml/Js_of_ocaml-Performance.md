
# Module `Js_of_ocaml.Performance`

Performance API

A code example:

```ocaml
  let perf = Performance.performance in
  ignore (perf##mark (Js.string "start"));
  do_something ();
  ignore (perf##mark (Js.string "end"));
  let _ =
    perf##measure
      (Js.string "elapsed")
      (Js.def (Js.string "start"))
      (Js.def (Js.string "end"))
  in
  let entries = perf##getEntriesByType (Js.string "measure") in
  Js.array_get entries 0 |> ignore
```
see [https://developer.mozilla.org/en-US/docs/Web/API/Performance](https://developer.mozilla.org/en-US/docs/Web/API/Performance) for API documentation.
```ocaml
class type  performanceEntry = object ... end
```
```ocaml
class type 'detail performanceMark = object ... end
```
```ocaml
class type 'detail performanceMeasure = object ... end
```
```ocaml
class type 'detail performanceMarkOptions = object ... end
```
```ocaml
class type 'detail performanceMeasureOptions = object ... end
```
```ocaml
class type  performance = object ... end
```
```ocaml
val performance : performance Js.t
```
`Window.performance`, the global `Performance` object.

```ocaml
val is_supported : unit -> bool
```
Whether the global `performance` object is defined.

```ocaml
val mark : 
  ?detail:'a ->
  ?startTime:Js.number_t ->
  performance Js.t ->
  Js.js_string Js.t ->
  'a performanceMark Js.t
```
Wrapper for `Performance.mark(name, options?)` taking labeled arguments.

```ocaml
val makeMeasureOptions : 
  ?detail:'a ->
  ?start:Js.js_string Js.t ->
  ?start_time:Js.number_t ->
  ?_end:Js.js_string Js.t ->
  ?end_time:Js.number_t ->
  ?duration:Js.number_t ->
  unit ->
  'a performanceMeasureOptions Js.t
```
Smart constructor for [`performanceMeasureOptions`](./Js_of_ocaml-Performance-class-type-performanceMeasureOptions.md).

Raises `Invalid_argument` if both `?start` and `?start_time`, or both `?_end` and `?end_time`, are provided — they map to the same JavaScript field.
