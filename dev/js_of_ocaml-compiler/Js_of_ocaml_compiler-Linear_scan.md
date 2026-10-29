# Module `Js_of_ocaml_compiler.Linear_scan`

Variable coalescing by linear scan, shared by `Js_variable_coalescing` and the Wasm backend's `Var_coalescing`.

The client builds a control-flow graph of a function, whose nodes use or define variables; we compute the live range of each variable and assign each variable a representative, so that variables with the same representative have disjoint live ranges.

```ocaml
type node = int
```
```ocaml
type action = 
  | Use of Code.Var.Set.t
  | Def of Code.Var.Set.t
  | DefUse of Code.Var.Set.t * Code.Var.Set.t (* defs, uses *)
  | Nop
```
```ocaml
type graph = {
  entry : node;
  size : int;
  actions : action array;
  succs : node list array;
  hints : Code.Var.t Js_of_ocaml_compiler.Code.Var.Hashtbl.t; (* x -> y when x is a copy of y: x should preferably get the representative of y, making the copy a no-op. *)
  try_blocks : node Stdlib.Int.Hashtbl.t; (* catch_entry -> body_entry. Every variable live at catch_entry is live throughout the try body, since an exception can be raised anywhere in the body. body_entry must come before catch_entry in reverse postorder. *)
}
```
```ocaml
module Builder : sig ... end
```
Graph construction.

```ocaml
val liveness : graph -> Code.Var.Set.t array
```
The variables live at the entry of each node.

```ocaml
module Live_range : sig ... end
```
```ocaml
val live_ranges : 
  graph ->
  Code.Var.Set.t array ->
  candidates:Code.Var.Set.t ->
  param_vars:Code.Var.Set.t ->
  Live_range.t Js_of_ocaml_compiler.Code.Var.Hashtbl.t
```
Live ranges of the `candidates`, given the result of `liveness`. The parameters `param_vars` are all live at the start of the function.

```ocaml
val allocate : 
  ?compatible:(Code.Var.t -> Code.Var.t -> bool) ->
  graph ->
  Live_range.t Js_of_ocaml_compiler.Code.Var.Hashtbl.t ->
  assign:(Code.Var.t -> Code.Var.t -> unit) ->
  int * int
```
Calls `assign x r` with the representative `r` of each variable `x` (possibly `x` itself). Only `compatible` variables (by default, all of them) get the same representative. Returns the number of variables coalesced through a copy hint and opportunistically.
