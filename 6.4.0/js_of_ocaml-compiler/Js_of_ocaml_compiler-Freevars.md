
# Module `Js_of_ocaml_compiler.Freevars`

```ocaml
val iter_block_free_vars : (Code.Var.t -> unit) -> Code.block -> unit
```
```ocaml
val iter_block_bound_vars : (Code.Var.t -> unit) -> Code.block -> unit
```
```ocaml
val iter_instr_free_vars : (Code.Var.t -> unit) -> Code.instr -> unit
```
```ocaml
val iter_last_free_var : (Code.Var.t -> unit) -> Code.last -> unit
```
```ocaml
val find_loops_in_closure : 
  Code.program ->
  Code.Addr.t ->
  Code.Addr.t Js_of_ocaml_compiler.Code.Addr.Map.t
```
```ocaml
val f_mutable : 
  Code.program ->
  Code.Var.Set.t Js_of_ocaml_compiler.Code.Addr.Map.t
```
```ocaml
val f : Code.program -> Code.Var.Set.t Js_of_ocaml_compiler.Code.Addr.Map.t
```