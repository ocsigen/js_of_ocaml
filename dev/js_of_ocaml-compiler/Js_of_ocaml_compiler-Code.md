
# Module `Js_of_ocaml_compiler.Code`

```ocaml
module Addr : sig ... end
```
```ocaml
module Var : sig ... end
```
```ocaml
type cont = Addr.t * Var.t list
```
```ocaml
type prim = 
  | Vectlength of Optimization_hint.array_kind
  | Array_get
  | Extern of string * Optimization_hint.ccall option
  | Not
  | IsInt
  | Eq
  | Neq
  | Lt
  | Le
  | Ult
```
```ocaml
type array_or_not = 
  | Array
  | NotArray
  | Unknown
```
```ocaml
module Native_string : sig ... end
```
```ocaml
type constant = 
  | String of string
  | NativeString of Native_string.t
  | Float of Stdlib.Int64.t
  | Float32 of Stdlib.Int64.t
  | Float_array of Stdlib.Int64.t array
  | Int of Targetint.t
  | Int32 of Stdlib.Int32.t (* Only produced when compiling to WebAssembly. *)
  | Int64 of Stdlib.Int64.t
  | NativeInt of Stdlib.Int32.t (* Only produced when compiling to WebAssembly. *)
  | Tuple of int * constant array * array_or_not
  | Null_
```
```ocaml
module Constant : sig ... end
```
```ocaml
type loc = 
  | No
  | Before of Addr.t
  | After of Addr.t
```
```ocaml
val noloc : loc
```
```ocaml
val location_of_pc : int -> loc
```
```ocaml
type prim_arg = 
  | Pv of Var.t
  | Pc of constant
```
```ocaml
type special = 
  | Alias_prim of string
```
```ocaml
type mutability = 
  | Immutable
  | Maybe_mutable
```
```ocaml
type field_type = 
  | Non_float
  | Float
```
```ocaml
type expr = 
  | Apply of {
    f : Var.t;
    args : Var.t list;
    exact : bool;
  }
  | Block of int * Var.t array * array_or_not * mutability
  | Field of Var.t * int * field_type
  | Closure of Var.t list
    * cont
    * Optimization_hint.closure_hint option * Parse_info.t option
  | Constant of constant
  | Prim of prim * prim_arg list
  | Special of special
```
```ocaml
type instr = 
  | Let of Var.t * expr
  | Assign of Var.t * Var.t
  | Set_field of Var.t * int * field_type * Var.t
  | Offset_ref of Var.t * int
  | Array_set of Var.t * Var.t * Var.t
  | Event of Parse_info.t
```
```ocaml
type last = 
  | Return of Var.t
  | Raise of Var.t * [ `Normal | `Notrace | `Reraise ]
  | Stop
  | Branch of cont
  | Cond of Var.t * cont * cont
  | Switch of Var.t * cont array
  | Pushtrap of cont * Var.t * cont
  | Poptrap of cont
```
```ocaml
type block = {
  params : Var.t list;
  body : instr list;
  branch : last;
}
```
```ocaml
type program = {
  start : Addr.t;
  blocks : block Addr.Map.t;
  free_pc : Addr.t;
}
```
```ocaml
module Print : sig ... end
```
```ocaml
type 'c fold_blocs =
  block Addr.Map.t ->
  Addr.t ->
  (Addr.t -> 'c -> 'c) ->
  'c ->
  'c
```
```ocaml
type fold_blocs_poly = {
  fold : 'a. 'a fold_blocs;
}
```
```ocaml
val fold_closures : 
  program ->
  (Var.t option -> Var.t list -> cont -> Parse_info.t option -> 'd -> 'd) ->
  'd ->
  'd
```
`fold_closures p f init` folds `f` over all closures in the program `p`, starting from the initial value `init`. For each closure, `f` is called with the following arguments: the closure name (enclosed in `Stdlib.Some`), its parameter list, the address and parameter instantiation of its first block, the optional closure location and the current accumulator. In addition, `f` is called on the initial block `p.start`, with `None` as the closure name. All closures in all blocks of `p` are included in the fold, not only the ones reachable from `p.start`.

```ocaml
val fold_closures_innermost_first : 
  program ->
  (Var.t option -> Var.t list -> cont -> Parse_info.t option -> 'd -> 'd) ->
  'd ->
  'd
```
Similar to [`fold_closures`](./#val-fold_closures), but applies the fold function to the innermost closures first. Unlike with [`fold_closures`](./#val-fold_closures), only the closures reachable from `p.start` are considered.

```ocaml
val fold_closures_outermost_first : 
  program ->
  (Var.t option -> Var.t list -> cont -> Parse_info.t option -> 'd -> 'd) ->
  'd ->
  'd
```
Similar to [`fold_closures`](./#val-fold_closures), but applies the fold function to the outermost closures first. Unlike with [`fold_closures`](./#val-fold_closures), only the closures reachable from `p.start` are considered.

```ocaml
val fold_children : 'c fold_blocs
```
```ocaml
val fold_children_skip_try_body : 'c fold_blocs
```
```ocaml
val poptraps : block Addr.Map.t -> Addr.t -> Addr.Set.t
```
```ocaml
val return_values : program -> Var.Set.t Var.Map.t
```
```ocaml
val traverse : 
  fold_blocs_poly ->
  (Addr.t -> 'c -> 'c) ->
  Addr.t ->
  block Addr.Map.t ->
  'c ->
  'c
```
```ocaml
val preorder_traverse : 
  fold_blocs_poly ->
  (Addr.t -> 'c -> 'c) ->
  Addr.t ->
  block Addr.Map.t ->
  'c ->
  'c
```
```ocaml
val last_instr : instr list -> instr option
```
Last instruction of a block body, ignoring events

```ocaml
val used_blocks : program -> Stdlib.BitSet.t
```
```ocaml
val prepend : program -> instr list -> program
```
```ocaml
val empty : program
```
```ocaml
val compact : program -> program
```
```ocaml
val is_empty : program -> bool
```
```ocaml
val equal : program -> program -> bool
```
```ocaml
val print_diff : program -> program -> unit
```
```ocaml
val check_updates : name:string -> program -> program -> updates:int -> unit
```
```ocaml
val invariant : program -> unit
```
```ocaml
val cont_equal : cont -> cont -> bool
```
```ocaml
val cont_compare : cont -> cont -> int
```