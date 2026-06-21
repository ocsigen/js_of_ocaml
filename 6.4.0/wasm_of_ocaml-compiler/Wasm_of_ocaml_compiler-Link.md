
# Module `Wasm_of_ocaml_compiler.Link`

```ocaml
module Wasm_binary : sig ... end
```
```ocaml
type unit_data = {
  unit_name : string;
  unit_info : Js_of_ocaml_compiler.Unit_info.t;
  fragments : (string * Js_of_ocaml_compiler.Javascript.expression) list;
}
```
```ocaml
val add_info : 
  Zip.output ->
  build_info:Js_of_ocaml_compiler.Build_info.t ->
  unit_data:unit_data list ->
  unit ->
  unit
```
```ocaml
val build_runtime_arguments : 
  link_spec:(string * int list option) list ->
  separate_compilation:bool ->
  missing_primitives:string list ->
  wasm_dir:string ->
  generated_js:
    (string option * (string * Js_of_ocaml_compiler.Javascript.expression) list)
      list ->
  embedded_files:(string * string) list ->
  unit ->
  Js_of_ocaml_compiler.Javascript.expression
```
```ocaml
val output_js : Js_of_ocaml_compiler.Javascript.program -> string
```
```ocaml
val link : 
  output_file:string ->
  linkall:bool ->
  mklib:bool ->
  enable_source_maps:bool ->
  embedded_files:(string * string) list ->
  files:string list ->
  unit
```
```ocaml
val source_name : int option -> int option -> string -> string
```
```ocaml
val gen_dir : string -> (string -> 'a) -> 'a
```