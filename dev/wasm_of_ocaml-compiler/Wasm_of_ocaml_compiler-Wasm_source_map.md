
# Module `Wasm_of_ocaml_compiler.Wasm_source_map`

```ocaml
type t
```
```ocaml
val is_empty : Js_of_ocaml_compiler.Source_map.Standard.t -> bool
```
```ocaml
type resize_data = {
  mutable i : int;
  mutable pos : int array;
  mutable delta : int array;
}
```
```ocaml
val resize : 
  resize_data ->
  Js_of_ocaml_compiler.Source_map.Standard.t ->
  Js_of_ocaml_compiler.Source_map.Standard.t
```
```ocaml
val concatenate : 
  (int * Js_of_ocaml_compiler.Source_map.Standard.t) list ->
  Js_of_ocaml_compiler.Source_map.t
```
```ocaml
val iter_sources : 
  Js_of_ocaml_compiler.Source_map.t ->
  (int option -> int option -> string -> unit) ->
  unit
```
```ocaml
val insert_source_contents : 
  Js_of_ocaml_compiler.Source_map.t ->
  (int option -> int option -> string -> string option) ->
  Js_of_ocaml_compiler.Source_map.t
```
```ocaml
val blackbox_filename : string
```
```ocaml
val blackbox_contents : string
```