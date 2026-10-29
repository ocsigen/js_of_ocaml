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
type dependency = {
  name : string;
  export : string option;
  import : (string * string) option;
  reaches : string list;
  root : bool;
}
```
A node of the dependency graph used for dead code elimination, in the format of `wasm-metadce`: `export` and `import` associate the node to an export and an import of the linked module; `reaches` lists the nodes this node depends on (by name).

```ocaml
val parse_dependencies : string -> dependency list
```
Parse a dependency graph in the JSON format of `wasm-metadce`

```ocaml
type output = {
  source_map : Js_of_ocaml_compiler.Source_map.t;
  imports : (string * string) list; (* The imports of the linked module, as pairs (module name, name) *)
}
```
```ocaml
val f : 
  ?filter_export:(string -> bool) ->
  ?dependencies:dependency list ->
  ?names:bool ->
  ?eliminate_duplicates:bool ->
  input list ->
  output_file:string ->
  output
```
Link the input modules. When `dependencies` is provided, dead code is removed: only the exports reachable from the root nodes of the dependency graph are kept, and only the entries reachable from them or from the start functions are kept. The name section is omitted if `names` is false. Unless `eliminate_duplicates` is false, the duplicate functions of each module are removed (wasm-opt does this as well when it runs).
