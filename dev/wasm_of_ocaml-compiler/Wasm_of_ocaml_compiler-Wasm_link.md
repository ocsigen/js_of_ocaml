
# Module `Wasm_of_ocaml_compiler.Wasm_link`

```ocaml
type input = {
  module_name : string;
  file : string;
  code : string option;
  opt_source_map : Js_of_ocaml_compiler.Source_map.Standard.t option;
}
```
```ocaml
val f : 
  ?filter_export:(string -> bool) ->
  input list ->
  output_file:string ->
  Js_of_ocaml_compiler.Source_map.t
```