
# Module `Wasm_of_ocaml_compiler.Typing`

```ocaml
module Integer : sig ... end
```
```ocaml
type boxed_number = 
  | Int32
  | Int64
  | Nativeint
  | Float
  | Float32
```
```ocaml
type boxed_status = 
  | Boxed
  | Unboxed
```
```ocaml
type typ = 
  | Top
  | Int of Integer.kind
  | Number of boxed_number * boxed_status
  | Tuple of typ array
  | Bigarray of Js_of_ocaml_compiler.Optimization_hint.Bigarray.t
  | Null
  | Bot
```
```ocaml
val constant_type : Js_of_ocaml_compiler.Code.constant -> typ
```
```ocaml
val can_unbox_parameters : 
  Call_graph_analysis.t ->
  Js_of_ocaml_compiler.Code.Var.t ->
  bool
```
```ocaml
val bigarray_element_type : 
  Js_of_ocaml_compiler.Optimization_hint.Bigarray.kind ->
  typ
```
```ocaml
type t
```
```ocaml
val var_type : t -> Js_of_ocaml_compiler.Code.Var.t -> typ
```
```ocaml
val return_type : t -> Js_of_ocaml_compiler.Code.Var.t -> typ
```
```ocaml
val reset : unit -> unit
```
```ocaml
val register_prim : string -> unbox:bool -> typ -> unit
```
```ocaml
val f : 
  global_flow_state:Js_of_ocaml_compiler.Global_flow.state ->
  global_flow_info:Js_of_ocaml_compiler.Global_flow.info ->
  fun_info:Call_graph_analysis.t ->
  deadcode_sentinel:Js_of_ocaml_compiler.Code.Var.t ->
  Js_of_ocaml_compiler.Code.program ->
  t
```