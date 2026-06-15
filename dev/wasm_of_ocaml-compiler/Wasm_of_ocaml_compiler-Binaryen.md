
# Module `Wasm_of_ocaml_compiler.Binaryen`

```ocaml
type link_input = {
  module_name : string; (* Name under which the module is imported in other modules *)
  file : string; (* File containing the Wasm module *)
  source_map_file : string option;
}
```
```ocaml
val link : 
  ?options:string list ->
  inputs:link_input list ->
  opt_output_sourcemap:string option ->
  output_file:string ->
  unit ->
  unit
```
```ocaml
val dead_code_elimination : 
  dependencies:string ->
  opt_input_sourcemap:string option ->
  input_file:string ->
  opt_output_sourcemap:string option ->
  output_file:string ->
  Js_of_ocaml_compiler.Stdlib.StringSet.t
```
```ocaml
val optimize : 
  profile:Js_of_ocaml_compiler.Profile.t ->
  ?options:string list ->
  opt_input_sourcemap:string option ->
  input_file:string ->
  opt_output_sourcemap:string option ->
  output_file:string ->
  unit ->
  unit
```