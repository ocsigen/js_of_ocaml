
# Module `Wasm_of_ocaml_compiler.Generate`

```ocaml
val init : unit -> unit
```
```ocaml
val start : unit -> Code_generation.context
```
```ocaml
val f : 
  context:Code_generation.context ->
  unit_name:string option ->
  Js_of_ocaml_compiler.Code.program ->
  live_vars:int array ->
  in_cps:Js_of_ocaml_compiler.Effects.in_cps ->
  deadcode_sentinel:Js_of_ocaml_compiler.Code.Var.t ->
  global_flow_data:
    (Js_of_ocaml_compiler.Global_flow.state
     * Js_of_ocaml_compiler.Global_flow.info) ->
  Wasm_ast.var * (string * Js_of_ocaml_compiler.Javascript.expression) list
```
```ocaml
val add_start_function : 
  context:Code_generation.context ->
  Wasm_ast.var ->
  unit
```
```ocaml
val add_init_function : 
  context:Code_generation.context ->
  to_link:string list ->
  unit
```
```ocaml
val add_missing_primitives : 
  context:Code_generation.context ->
  (string * int) list ->
  unit
```
```ocaml
val output : Stdlib.out_channel -> context:Code_generation.context -> unit
```
```ocaml
val wasm_output : 
  Stdlib.out_channel ->
  opt_source_map_file:string option ->
  context:Code_generation.context ->
  unit
```
```ocaml
val compile : 
  unit_name:string option ->
  Js_of_ocaml_compiler.Code.program ->
  string * (string * Js_of_ocaml_compiler.Javascript.expression) list
```
```ocaml
val from_string : 
  prims:string array ->
  debug:Instruct.debug_event list array ->
  unit_name:string option ->
  string ->
  string * (string * Js_of_ocaml_compiler.Javascript.expression) list
```