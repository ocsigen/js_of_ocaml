
# Class type `Performance.performance`

```ocaml
method timeOrigin : Js_of_ocaml__.Js.number_t Js_of_ocaml__.Js.readonly_prop
```
```ocaml
method now : Js_of_ocaml__.Js.number_t Js_of_ocaml__.Js.meth
```
```ocaml
method mark : 'a. Js_of_ocaml__.Js.js_string Js_of_ocaml__.Js.t ->
  'a performanceMark Js_of_ocaml__.Js.t Js_of_ocaml__.Js.meth
```
```ocaml
method mark_options : 'a. Js_of_ocaml__.Js.js_string Js_of_ocaml__.Js.t ->
  'a performanceMarkOptions Js_of_ocaml__.Js.t ->
  'a performanceMark Js_of_ocaml__.Js.t Js_of_ocaml__.Js.meth
```
```ocaml
method measure : 'a. Js_of_ocaml__.Js.js_string Js_of_ocaml__.Js.t ->
  Js_of_ocaml__.Js.js_string Js_of_ocaml__.Js.t Js_of_ocaml__.Js.optdef ->
  Js_of_ocaml__.Js.js_string Js_of_ocaml__.Js.t Js_of_ocaml__.Js.optdef ->
  'a performanceMeasure Js_of_ocaml__.Js.t Js_of_ocaml__.Js.meth
```
```ocaml
method measure_options : 'a. Js_of_ocaml__.Js.js_string Js_of_ocaml__.Js.t ->
  'a performanceMeasureOptions Js_of_ocaml__.Js.t ->
  'a performanceMeasure Js_of_ocaml__.Js.t Js_of_ocaml__.Js.meth
```
```ocaml
method clearMarks : unit Js_of_ocaml__.Js.meth
```
```ocaml
method clearMarks_named : Js_of_ocaml__.Js.js_string Js_of_ocaml__.Js.t ->
  unit Js_of_ocaml__.Js.meth
```
```ocaml
method clearMeasures : unit Js_of_ocaml__.Js.meth
```
```ocaml
method clearMeasures_named : Js_of_ocaml__.Js.js_string Js_of_ocaml__.Js.t ->
  unit Js_of_ocaml__.Js.meth
```
```ocaml
method clearResourceTimings : unit Js_of_ocaml__.Js.meth
```
```ocaml
method getEntries : performanceEntry Js_of_ocaml__.Js.t
                      Js_of_ocaml__.Js.js_array
                      Js_of_ocaml__.Js.t
                      Js_of_ocaml__.Js.meth
```
```ocaml
method getEntriesByName : Js_of_ocaml__.Js.js_string Js_of_ocaml__.Js.t ->
  performanceEntry Js_of_ocaml__.Js.t Js_of_ocaml__.Js.js_array
    Js_of_ocaml__.Js.t
    Js_of_ocaml__.Js.meth
```
```ocaml
method getEntriesByName_type : Js_of_ocaml__.Js.js_string Js_of_ocaml__.Js.t ->
  Js_of_ocaml__.Js.js_string Js_of_ocaml__.Js.t ->
  performanceEntry Js_of_ocaml__.Js.t Js_of_ocaml__.Js.js_array
    Js_of_ocaml__.Js.t
    Js_of_ocaml__.Js.meth
```
```ocaml
method getEntriesByType : Js_of_ocaml__.Js.js_string Js_of_ocaml__.Js.t ->
  performanceEntry Js_of_ocaml__.Js.t Js_of_ocaml__.Js.js_array
    Js_of_ocaml__.Js.t
    Js_of_ocaml__.Js.meth
```
```ocaml
method setResourceTimingBufferSize : int -> unit Js_of_ocaml__.Js.meth
```