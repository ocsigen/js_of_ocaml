open Stdlib

type constant_global

type context =
  { constants : (Code.Var.t, Wa_ast.expression) Hashtbl.t
  ; mutable data_segments : (bool * Wa_ast.data list) Code.Var.Map.t
  ; mutable constant_globals : constant_global Code.Var.Map.t
  ; mutable other_fields : Wa_ast.module_field list
  ; mutable imports : (Code.Var.t * Wa_ast.import_desc) StringMap.t StringMap.t
  ; types : (string, Code.Var.t) Hashtbl.t
  ; mutable closure_envs : Code.Var.t Code.Var.Map.t
        (** GC: mapping of recursive functions to their shared environment *)
  ; mutable apply_funs : Code.Var.t Stdlib.IntMap.t
  ; mutable curry_funs : Code.Var.t Stdlib.IntMap.t
  ; mutable dummy_funs : Code.Var.t Stdlib.IntMap.t
  ; mutable init_code : Wa_ast.instruction list
  }

val make_context : unit -> context

type 'a t

type expression = Wa_ast.expression t

val ( let* ) : 'a t -> ('a -> 'b t) -> 'b t

val return : 'a -> 'a t

val instr : Wa_ast.instruction -> unit t

val seq : unit t -> expression -> expression

val expression_list : ('a -> expression) -> 'a list -> Wa_ast.expression list t

module Arith : sig
  val const : int32 -> expression

  val to_int31 : expression -> expression

  val of_int31 : expression -> expression

  val ( + ) : expression -> expression -> expression

  val ( - ) : expression -> expression -> expression

  val ( * ) : expression -> expression -> expression

  val ( / ) : expression -> expression -> expression

  val ( mod ) : expression -> expression -> expression

  val ( lsl ) : expression -> expression -> expression

  val ( lsr ) : expression -> expression -> expression

  val ( asr ) : expression -> expression -> expression

  val ( land ) : expression -> expression -> expression

  val ( lor ) : expression -> expression -> expression

  val ( lxor ) : expression -> expression -> expression

  val ( < ) : expression -> expression -> expression

  val ( <= ) : expression -> expression -> expression

  val ( = ) : expression -> expression -> expression

  val ( <> ) : expression -> expression -> expression

  val ult : expression -> expression -> expression

  val uge : expression -> expression -> expression

  val eqz : expression -> expression
end

val cast : ?nullable:bool -> Wa_ast.heap_type -> expression -> expression

val load : Wa_ast.var -> expression

val tee : ?typ:Wa_ast.value_type -> Wa_ast.var -> expression -> expression

val store : ?always:bool -> ?typ:Wa_ast.value_type -> Wa_ast.var -> expression -> unit t

val assign : Wa_ast.var -> expression -> unit t

val drop : expression -> unit t

val push : expression -> unit t

val loop : Wa_ast.func_type -> unit t -> unit t

val block : Wa_ast.func_type -> unit t -> unit t

val block_expr : Wa_ast.func_type -> unit t -> expression

val if_ : Wa_ast.func_type -> expression -> unit t -> unit t -> unit t

val try_ : Wa_ast.func_type -> unit t -> Code.Var.t -> unit t -> unit t

val add_var : ?typ:Wa_ast.value_type -> Wa_ast.var -> int t

val define_var : Wa_ast.var -> expression -> unit t

val is_small_constant : Wa_ast.expression -> bool t

type type_def =
  { supertype : Wa_ast.var option
  ; final : bool
  ; typ : Wa_ast.str_type
  }

val register_type : string -> (unit -> type_def t) -> Wa_ast.var t

val register_import :
  ?import_module:string -> name:string -> Wa_ast.import_desc -> Wa_ast.var t

val register_global :
  Wa_ast.symbol -> ?constant:bool -> Wa_ast.global_type -> Wa_ast.expression -> unit t

val get_global : Code.Var.t -> Wa_ast.expression option t

val register_data_segment : Code.Var.t -> active:bool -> Wa_ast.data list -> unit t

val get_data_segment : Code.Var.t -> (bool * Wa_ast.data list) t

val register_init_code : unit t -> unit t

val init_code : context -> unit t

val get_context : context t

val set_closure_env : Code.Var.t -> Code.Var.t -> unit t

val get_closure_env : Code.Var.t -> Code.Var.t t

val is_closure : Code.Var.t -> bool t

val need_apply_fun : arity:int -> Code.Var.t t

val need_curry_fun : arity:int -> Code.Var.t t

val need_dummy_fun : arity:int -> Code.Var.t t

val function_body :
     context:context
  -> value_type:Wa_ast.value_type
  -> param_count:int
  -> body:unit t
  -> Wa_ast.value_type list * Wa_ast.instruction list
