
# Module `Wasm_of_ocaml_compiler.Code_generation`

```ocaml
type constant_global
```
```ocaml
type context = {
  constants : Wasm_ast.expression Js_of_ocaml_compiler.Code.Var.Hashtbl.t;
  mutable data_segments : string Js_of_ocaml_compiler.Code.Var.Map.t;
  mutable constant_globals : constant_global Js_of_ocaml_compiler.Code.Var.Map.t;
  mutable other_fields : Wasm_ast.module_field list;
  mutable imports : (Js_of_ocaml_compiler.Code.Var.t * Wasm_ast.import_desc)
                    Js_of_ocaml_compiler.Stdlib.StringMap.t
                    Js_of_ocaml_compiler.Stdlib.StringMap.t;
  type_names : Js_of_ocaml_compiler.Code.Var.t
               Js_of_ocaml_compiler.Stdlib.String.Hashtbl.t;
  types : Wasm_ast.type_field Js_of_ocaml_compiler.Code.Var.Hashtbl.t;
  mutable closure_envs : Js_of_ocaml_compiler.Code.Var.t
                         Js_of_ocaml_compiler.Code.Var.Map.t; (* GC: mapping of recursive functions to their shared environment *)
  closure_types : (Wasm_ast.value_type option list, int)
                  Js_of_ocaml_compiler.Stdlib.Hashtbl.t;
  mutable apply_funs : Js_of_ocaml_compiler.Code.Var.t
                       Js_of_ocaml_compiler.Stdlib.IntMap.t;
  mutable cps_apply_funs : Js_of_ocaml_compiler.Code.Var.t
                           Js_of_ocaml_compiler.Stdlib.IntMap.t;
  mutable curry_funs : Js_of_ocaml_compiler.Code.Var.t
                       Js_of_ocaml_compiler.Stdlib.IntMap.t;
  mutable cps_curry_funs : Js_of_ocaml_compiler.Code.Var.t
                           Js_of_ocaml_compiler.Stdlib.IntMap.t;
  mutable dummy_funs : Js_of_ocaml_compiler.Code.Var.t
                       Js_of_ocaml_compiler.Stdlib.IntMap.t;
  mutable cps_dummy_funs : Js_of_ocaml_compiler.Code.Var.t
                           Js_of_ocaml_compiler.Stdlib.IntMap.t;
  mutable init_code : Wasm_ast.instruction list;
  mutable fragments : Js_of_ocaml_compiler.Javascript.expression
                      Js_of_ocaml_compiler.Stdlib.StringMap.t;
  mutable globalized_variables : Js_of_ocaml_compiler.Code.Var.Set.t;
  value_type : Wasm_ast.value_type;
  mutable unit_name : string option;
  mutable no_tail_call : unit Js_of_ocaml_compiler.Code.Var.Hashtbl.t;
}
```
```ocaml
val make_context : value_type:Wasm_ast.value_type -> context
```
```ocaml
type 'a t
```
```ocaml
type expression = Wasm_ast.expression t
```
```ocaml
val (let*) : 'a t -> ('a -> 'b t) -> 'b t
```
```ocaml
val return : 'a -> 'a t
```
```ocaml
val instr : Wasm_ast.instruction -> unit t
```
```ocaml
val seq : unit t -> expression -> expression
```
```ocaml
val expression_list : ('a -> 'b t) -> 'a list -> 'b list t
```
```ocaml
module Arith : sig ... end
```
```ocaml
val cast : ?nullable:bool -> Wasm_ast.heap_type -> expression -> expression
```
```ocaml
val load : Wasm_ast.var -> expression
```
```ocaml
val tee : ?typ:Wasm_ast.value_type -> Wasm_ast.var -> expression -> expression
```
```ocaml
val store : 
  ?always:bool ->
  ?typ:Wasm_ast.value_type ->
  Wasm_ast.var ->
  expression ->
  unit t
```
```ocaml
val assign : Wasm_ast.var -> expression -> unit t
```
```ocaml
val drop : expression -> unit t
```
```ocaml
val push : expression -> unit t
```
```ocaml
val loop : Wasm_ast.func_type -> unit t -> unit t
```
```ocaml
val block : Wasm_ast.func_type -> unit t -> unit t
```
```ocaml
val block_expr : Wasm_ast.func_type -> unit t -> expression
```
```ocaml
val if_ : Wasm_ast.func_type -> expression -> unit t -> unit t -> unit t
```
```ocaml
val try_expr : 
  Wasm_ast.func_type ->
  unit t ->
  (Js_of_ocaml_compiler.Code.Var.t * int * Wasm_ast.value_type) list ->
  expression
```
```ocaml
val add_var : ?typ:Wasm_ast.value_type -> Wasm_ast.var -> Wasm_ast.var t
```
```ocaml
val define_var : Wasm_ast.var -> expression -> unit t
```
```ocaml
val is_small_constant : Wasm_ast.expression -> bool t
```
```ocaml
val event : Js_of_ocaml_compiler.Parse_info.t -> unit t
```
```ocaml
val no_event : unit t
```
```ocaml
val hidden_location : Js_of_ocaml_compiler.Parse_info.t
```
```ocaml
type type_def = {
  supertype : Wasm_ast.var option;
  final : bool;
  typ : Wasm_ast.str_type;
}
```
```ocaml
val register_type : string -> (unit -> type_def t) -> Wasm_ast.var t
```
```ocaml
val heap_type_sub : Wasm_ast.heap_type -> Wasm_ast.heap_type -> bool t
```
```ocaml
val register_import : 
  ?allow_tail_call:bool ->
  ?import_module:string ->
  name:string ->
  Wasm_ast.import_desc ->
  Wasm_ast.var t
```
```ocaml
val register_global : 
  Wasm_ast.var ->
  ?exported_name:string ->
  ?constant:bool ->
  Wasm_ast.global_type ->
  Wasm_ast.expression ->
  unit t
```
```ocaml
val get_global : 
  Js_of_ocaml_compiler.Code.Var.t ->
  Wasm_ast.expression option t
```
```ocaml
val register_data_segment : Js_of_ocaml_compiler.Code.Var.t -> string -> unit t
```
```ocaml
val register_init_code : unit t -> unit t
```
```ocaml
val init_code : context -> unit t
```
```ocaml
val register_fragment : 
  string ->
  (unit -> Js_of_ocaml_compiler.Javascript.expression) ->
  unit t
```
```ocaml
val get_context : context t
```
```ocaml
val set_closure_env : 
  Js_of_ocaml_compiler.Code.Var.t ->
  Js_of_ocaml_compiler.Code.Var.t ->
  unit t
```
```ocaml
val get_closure_env : 
  Js_of_ocaml_compiler.Code.Var.t ->
  Js_of_ocaml_compiler.Code.Var.t t
```
```ocaml
val is_closure : Js_of_ocaml_compiler.Code.Var.t -> bool t
```
```ocaml
val unit_name : string option t
```
```ocaml
val need_apply_fun : cps:bool -> arity:int -> Js_of_ocaml_compiler.Code.Var.t t
```
```ocaml
val need_curry_fun : cps:bool -> arity:int -> Js_of_ocaml_compiler.Code.Var.t t
```
```ocaml
val need_dummy_fun : cps:bool -> arity:int -> Js_of_ocaml_compiler.Code.Var.t t
```
```ocaml
val function_body : 
  context:context ->
  param_names:Js_of_ocaml_compiler.Code.Var.t list ->
  body:unit t ->
  (Wasm_ast.var * Wasm_ast.value_type) list * Wasm_ast.instruction list
```
```ocaml
val variable_type : 
  Js_of_ocaml_compiler.Code.Var.t ->
  Wasm_ast.value_type option t
```
```ocaml
val array_placeholder : Js_of_ocaml_compiler.Code.Var.t -> expression
```
```ocaml
val default_value : 
  Wasm_ast.value_type ->
  (Wasm_ast.expression * Wasm_ast.value_type * Wasm_ast.ref_type option) t
```