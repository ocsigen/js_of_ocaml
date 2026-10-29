# Module `Wasm_of_ocaml_compiler.Binaryen`

```ocaml
type link_input = {
  module_name : string; (* Name under which the module is imported in other modules *)
  file : string; (* File containing the Wasm module *)
  source_map_file : string option;
}
```
```ocaml
val optimizes : Js_of_ocaml_compiler.Profile.t -> bool
```
Whether wasm-opt optimizes the generated code at this optimization level. When it does not, our own passes tighten the code instead; those which only redo what wasm-opt does are skipped when it does.

```ocaml
val optimize : 
  profile:Js_of_ocaml_compiler.Profile.t ->
  ?options:string list ->
  ?reorder_functions:bool ->
  opt_input_sourcemap:string option ->
  input_file:string ->
  opt_output_sourcemap:string option ->
  output_file:string ->
  unit ->
  unit
```
