
# Module `Js_of_ocaml_compiler.Driver`

```ocaml
type optimized_result = {
  program : Code.program;
  variable_uses : Deadcode.variable_uses;
  trampolined_calls : Effects.trampolined_calls;
  in_cps : Effects.in_cps;
  deadcode_sentinel : Code.Var.t;
  shapes : Shape.t Stdlib.StringMap.t;
}
```
```ocaml
val optimize_for_wasm : 
  shapes:bool ->
  profile:Profile.t ->
  Code.program ->
  optimized_result * (Global_flow.state * Global_flow.info)
```
```ocaml
val f : 
  ?standalone:bool ->
  ?wrap_with_fun:[ `Iife | `Anonymous | `Named of string ] ->
  ?profile:Profile.t ->
  ?shapes:bool ->
  link:[ `All | `All_from of string list | `Needed | `No ] ->
  source_map:bool ->
  formatter:Pretty_print.t ->
  Code.program ->
  Source_map.info * Shape.t Stdlib.StringMap.t
```
```ocaml
val f' : 
  ?standalone:bool ->
  ?wrap_with_fun:[ `Iife | `Anonymous | `Named of string ] ->
  ?profile:Profile.t ->
  link:[ `All | `All_from of string list | `Needed | `No ] ->
  Pretty_print.t ->
  Code.program ->
  unit
```
```ocaml
val from_string : 
  prims:string array ->
  debug:Instruct.debug_event list array ->
  string ->
  Pretty_print.t ->
  unit
```
```ocaml
val link_and_pack : 
  ?standalone:bool ->
  ?wrap_with_fun:[ `Iife | `Anonymous | `Named of string ] ->
  ?link:[ `All | `All_from of string list | `Needed | `No ] ->
  Javascript.statement_list ->
  Javascript.statement_list
```
```ocaml
val simplify_js : Javascript.statement_list -> Javascript.statement_list
```
```ocaml
val name_variables : Javascript.statement_list -> Javascript.statement_list
```
```ocaml
val configure : Pretty_print.t -> unit
```