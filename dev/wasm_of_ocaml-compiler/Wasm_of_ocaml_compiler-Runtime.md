# Module `Wasm_of_ocaml_compiler.Runtime`

```ocaml
val link_input : 
  ?load_source_map:(string -> Js_of_ocaml_compiler.Source_map.Standard.t) ->
  Binaryen.link_input ->
  Wasm_link.input
```
Linker input for a module in binary format. Its source map, if any, is loaded with `load_source_map`; it is ignored when this function is not provided.

```ocaml
val link : 
  ?remove_dead_code:bool ->
  inputs:Wasm_link.input list ->
  output_file:string ->
  unit ->
  unit
```
Link the input modules (in binary format), keeping all their code unless `remove_dead_code` is set: then, all exports are kept but the code and imports they do not use are removed.

```ocaml
val build : 
  allowed_imports:string list option ->
  opt_options:string list ->
  variables:(string * Wax_link.value) list ->
  inputs:Wax_link.input list ->
  output_file:string ->
  unit
```
