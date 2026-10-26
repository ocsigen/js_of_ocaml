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
  info_cps_versions : Code.Var.t Js_of_ocaml_compiler.Code.Var.Hashtbl.t;
  info_cps_closures : (Code.Var.t * Code.Var.t)
                      Js_of_ocaml_compiler.Code.Var.Hashtbl.t;
  info_cps_calls : Code.Var.Set.t;
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
  cps_versions : Code.Var.t Js_of_ocaml_compiler.Code.Var.Hashtbl.t;
  cps_closures : (Code.Var.t * Code.Var.t)
                 Js_of_ocaml_compiler.Code.Var.Hashtbl.t;
  cps_calls : Code.Var.Set.t;
  fast : bool;
}
```
```ocaml
val f : ?cps_calls:Code.Var.Set.t -> fast:bool -> Code.program -> state * info
```
`cps_calls` is the set of variables bound to the result of a CPS call (with double translation, after the CPS transformation; it should be empty otherwise)

```ocaml
val update_def : info -> Code.Var.t -> Code.expr -> unit
```
```ocaml
val exact_call : info -> Code.Var.t -> int -> bool
```
```ocaml
val called_version : info -> call:Code.Var.t -> Code.Var.t -> Code.Var.t * bool
```
`called_version info ~call g` is the version of function `g` which is called at call site `call` (the variable bound to the result of the call), and whether the last argument of the call is a continuation, which is passed the result of the function rather than passed to it. With double translation, a function with two versions is represented by its direct-style version (see `Effects.rewrite_direct_block`), and a CPS call calls its CPS version. A CPS call to a function without CPS version calls its direct-style version and passes the result to the continuation.

```ocaml
val get_unique_closure : 
  info ->
  call:Code.Var.t ->
  Code.Var.t ->
  (Code.Var.t * Code.Var.t list * bool) option
```
The function called at call site `call` through `f`, if it is statically known, with its parameters, and whether the last argument of the call is a continuation (see `called_version`).

```ocaml
val function_arity : info -> Code.Var.t -> int option
```
