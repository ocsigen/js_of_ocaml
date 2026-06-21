
# Module `Js_of_ocaml_compiler.Global_flow`

```ocaml
type def = 
  | Expr of Code.expr
  | Phi of {
    known : Code.Var.Set.t;
    others : bool;
    unit : bool;
  }
```
```ocaml
type approx = 
  | Top
  | Values of {
    known : Code.Var.Set.t;
    others : bool;
  }
```
```ocaml
type escape_status = 
  | Escape
  | Escape_constant
  | No
```
```ocaml
type info = {
  info_defs : def array;
  info_approximation : approx Code.Var.Tbl.t;
  info_may_escape : Code.Var.ISet.t;
  info_variable_may_escape : escape_status array;
  info_return_vals : Code.Var.Set.t Js_of_ocaml_compiler.Code.Var.Map.t;
}
```
```ocaml
type mutable_fields = 
  | No_field
  | Some_fields of Stdlib.IntSet.t
  | All_fields
```
```ocaml
module VarPairTbl : 
  Stdlib.Hashtbl.S
    with type key =
           Js_of_ocaml_compiler.Code.Var.t * Js_of_ocaml_compiler.Code.Var.t
```
```ocaml
type state = {
  vars : Code.Var.ISet.t;
  deps : Code.Var.t list Code.Var.Tbl.t;
  defs : def array;
  variable_may_escape : escape_status array;
  variable_mutable_fields : mutable_fields array;
  may_escape : escape_status array;
  mutable_fields : mutable_fields array;
  return_values : Code.Var.Set.t Js_of_ocaml_compiler.Code.Var.Map.t;
  functions_from_returned_value : Code.Var.t list
                                  Js_of_ocaml_compiler.Code.Var.Hashtbl.t;
  known_cases : int list Js_of_ocaml_compiler.Code.Var.Hashtbl.t;
  applied_functions : unit VarPairTbl.t;
  function_call_sites : Code.Var.t list Js_of_ocaml_compiler.Code.Var.Hashtbl.t;
  fast : bool;
}
```
```ocaml
val f : fast:bool -> Code.program -> state * info
```
```ocaml
val update_def : info -> Code.Var.t -> Code.expr -> unit
```
```ocaml
val exact_call : info -> Code.Var.t -> int -> bool
```
```ocaml
val get_unique_closure : 
  info ->
  Code.Var.t ->
  (Code.Var.t * Code.Var.t list) option
```
```ocaml
val function_arity : info -> Code.Var.t -> int option
```