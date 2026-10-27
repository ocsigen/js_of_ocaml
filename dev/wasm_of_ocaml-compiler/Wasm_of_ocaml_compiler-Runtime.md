# Module `Wasm_of_ocaml_compiler.Runtime`

```ocaml
val link_input : 
  ?load_source_map:(string -> Js_of_ocaml_compiler.Source_map.Standard.t) ->
  Binaryen.link_input ->
  Wasm_link.input
```
Linker input for a module in binary format. Its source map, if any, is loaded with `load_source_map`; it is ignored when this function is not provided.

```ocaml
val link : inputs:Wasm_link.input list -> output_file:string -> unit
```
Link the input modules (in binary format), keeping all their code

```ocaml
val build : 
  allowed_imports:string list option ->
  opt_options:string list ->
  variables:(string * Wax_link.value) list ->
  inputs:Wax_link.input list ->
  output_file:string ->
  unit
```
