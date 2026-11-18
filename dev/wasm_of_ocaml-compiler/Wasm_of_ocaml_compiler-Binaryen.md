# Module `Wasm_of_ocaml_compiler.Binaryen`

```ocaml
type link_input = {
  module_name : string; (* Name under which the module is imported in other modules *)
  file : string; (* File containing the Wasm module *)
  source_map_file : string option;
}
```
```ocaml
val available : unit -> bool
```
Whether a recent enough Binaryen's `wasm-opt` is available. `optimize` must only be called when it is.

```ocaml
val optimizes : Js_of_ocaml_compiler.Profile.t -> bool
```
Whether wasm-opt optimizes the generated code at this optimization level (and is available). When it does not, our own passes tighten the code instead; those which only redo what wasm-opt does are skipped when it does.

```ocaml
val warn_if_unavailable : Js_of_ocaml_compiler.Profile.t -> unit
```
Warn that the generated code is not optimized when `wasm-opt` would be used at this optimization level but is not available. Warns at most once.

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
