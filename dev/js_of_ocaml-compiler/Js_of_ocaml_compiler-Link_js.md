
# Module `Js_of_ocaml_compiler.Link_js`

```ocaml
val link : 
  output:Stdlib.out_channel ->
  linkall:bool ->
  mklib:bool ->
  files:string list ->
  resolve_sourcemap_url:bool ->
  source_map:Source_map.Encoding_spec.t option ->
  unit
```