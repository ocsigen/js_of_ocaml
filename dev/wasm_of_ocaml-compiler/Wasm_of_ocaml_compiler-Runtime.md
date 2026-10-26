# Module `Wasm_of_ocaml_compiler.Runtime`

```ocaml
val with_binary_files : 
  options:string list ->
  Binaryen.link_input list ->
  (Binaryen.link_input list -> 'a) ->
  'a
```
Convert the input files in text format to binary format with `wasm-opt`, passing it `options`, and call the continuation with the resulting files. The files are deleted afterwards.

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
  convert_options:string list ->
  opt_options:string list ->
  variables:(string * Wat_preprocess.value) list ->
  inputs:Wat_preprocess.input list ->
  output_file:string ->
  unit
```
