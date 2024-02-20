module type S = sig
  type expression = Wa_code_generation.expression

  module Stack : sig
    type stack = Code.Var.t option list

    type info

    val generate_spilling_information :
         Code.program
      -> context:Wa_code_generation.context
      -> closures:Wa_closure_conversion.closure Code.Var.Map.t
      -> pc:Code.Addr.t
      -> env:Code.Var.t
      -> params:Code.Var.t list
      -> info

    val make_info : unit -> info

    val add_spilling :
         info
      -> location:Code.Var.t
      -> stack:stack
      -> live_vars:Code.Var.Set.t
      -> spilled_vars:Code.Var.Set.t
      -> info * stack

    type ctx

    val start_function : context:Wa_code_generation.context -> info -> ctx

    val start_block : context:Wa_code_generation.context -> info -> Code.Addr.t -> ctx

    val perform_reloads :
         ctx
      -> [ `Branch of Code.last | `Instr of Code.instr | `Vars of Code.Var.Set.t ]
      -> unit Wa_code_generation.t

    val perform_spilling :
         ctx
      -> [ `Function | `Instr of Code.Var.t | `Block of Code.Addr.t ]
      -> unit Wa_code_generation.t

    val kill_variables : ctx -> unit

    val assign : ctx -> Code.Var.t -> unit Wa_code_generation.t

    val adjust_stack :
      ctx -> src:Code.Addr.t -> dst:Code.Addr.t -> unit Wa_code_generation.t

    val stack_adjustment_needed : ctx -> src:Code.Addr.t -> dst:Code.Addr.t -> bool
  end

  module Memory : sig
    val allocate :
         Stack.ctx
      -> Code.Var.t
      -> tag:int
      -> [ `Expr of Wa_ast.expression | `Var of Wa_ast.var ] list
      -> expression

    val load_function_pointer :
         cps:bool
      -> arity:int
      -> ?skip_cast:bool
      -> expression
      -> ([ `Index | `Ref of Wa_ast.var ] * Wa_ast.expression) Wa_code_generation.t

    val load_real_closure :
         cps:bool
      -> arity:int
      -> expression
      -> (Wa_ast.var * Wa_ast.expression) Wa_code_generation.t

    val check_function_arity :
         Code.Var.t
      -> cps:bool
      -> arity:int
      -> (typ:Wa_ast.value_type option -> expression -> expression)
      -> unit Wa_code_generation.t
      -> unit Wa_code_generation.t

    val tag : expression -> expression

    val field : expression -> int -> expression

    val set_field : expression -> int -> expression -> unit Wa_code_generation.t

    val array_get : expression -> expression -> expression

    val array_set : expression -> expression -> expression -> unit Wa_code_generation.t

    val float_array_get : expression -> expression -> expression

    val float_array_set :
      expression -> expression -> expression -> unit Wa_code_generation.t

    val gen_array_get : expression -> expression -> expression

    val gen_array_set :
      expression -> expression -> expression -> unit Wa_code_generation.t

    val array_length : expression -> expression

    val float_array_length : expression -> expression

    val gen_array_length : expression -> expression

    val bytes_length : expression -> expression

    val bytes_get : expression -> expression -> expression

    val bytes_set : expression -> expression -> expression -> unit Wa_code_generation.t

    val box_float : Stack.ctx -> Code.Var.t -> expression -> expression

    val unbox_float : expression -> expression

    val box_int32 : Stack.ctx -> Code.Var.t -> expression -> expression

    val unbox_int32 : expression -> expression

    val box_int64 : Stack.ctx -> Code.Var.t -> expression -> expression

    val unbox_int64 : expression -> expression

    val box_nativeint : Stack.ctx -> Code.Var.t -> expression -> expression

    val unbox_nativeint : expression -> expression
  end

  module Value : sig
    val value : Wa_ast.value_type

    val unit : expression

    val val_int : expression -> expression

    val int_val : expression -> expression

    val check_is_not_zero : expression -> expression
    (** Returns an int32 value *)

    val check_is_int : expression -> expression
    (** Returns an int32 value *)

    val not : expression -> expression

    val lt : expression -> expression -> expression

    val le : expression -> expression -> expression

    val eq : expression -> expression -> expression

    val neq : expression -> expression -> expression

    val ult : expression -> expression -> expression

    val is_int : expression -> expression

    val int_add : expression -> expression -> expression

    val int_sub : expression -> expression -> expression

    val int_mul : expression -> expression -> expression

    val int_div : expression -> expression -> expression

    val int_mod : expression -> expression -> expression

    val int_neg : expression -> expression

    val int_or : expression -> expression -> expression

    val int_and : expression -> expression -> expression

    val int_xor : expression -> expression -> expression

    val int_lsl : expression -> expression -> expression

    val int_lsr : expression -> expression -> expression

    val int_asr : expression -> expression -> expression
  end

  module Constant : sig
    val translate : Code.constant -> expression
  end

  module Closure : sig
    val translate :
         context:Wa_code_generation.context
      -> closures:Wa_closure_conversion.closure Code.Var.Map.t
      -> stack_ctx:Stack.ctx
      -> cps:bool
      -> Code.Var.t
      -> expression

    val bind_environment :
         context:Wa_code_generation.context
      -> closures:Wa_closure_conversion.closure Code.Var.Map.t
      -> cps:bool
      -> Code.Var.t
      -> unit Wa_code_generation.t

    val curry_allocate :
         stack_ctx:Stack.ctx
      -> x:Code.Var.t
      -> cps:bool
      -> arity:int
      -> int
      -> f:Code.Var.t
      -> closure:Code.Var.t
      -> arg:Code.Var.t
      -> Wa_ast.expression Wa_code_generation.t

    val curry_load :
         cps:bool
      -> arity:int
      -> int
      -> Code.Var.t
      -> (expression * expression * Wa_ast.value_type option) Wa_code_generation.t

    val dummy : cps:bool -> arity:int -> Wa_ast.expression Wa_code_generation.t
  end

  module Math : sig
    val cos : expression -> expression

    val sin : expression -> expression

    val tan : expression -> expression

    val acos : expression -> expression

    val asin : expression -> expression

    val atan : expression -> expression

    val atan2 : expression -> expression -> expression

    val cosh : expression -> expression

    val sinh : expression -> expression

    val tanh : expression -> expression

    val acosh : expression -> expression

    val asinh : expression -> expression

    val atanh : expression -> expression

    val cbrt : expression -> expression

    val exp : expression -> expression

    val exp2 : expression -> expression

    val log : expression -> expression

    val expm1 : expression -> expression

    val log1p : expression -> expression

    val log2 : expression -> expression

    val log10 : expression -> expression

    val hypot : expression -> expression -> expression

    val power : expression -> expression -> expression

    val fmod : expression -> expression -> expression

    val round : expression -> expression
  end

  val internal_primitives :
    (string, (Code.prim_arg -> expression) -> Code.prim_arg list -> expression) Hashtbl.t

  val handle_exceptions :
       result_typ:Wa_ast.value_type list
    -> fall_through:'a
    -> context:([> `Skip ] as 'b) list
    -> (   result_typ:Wa_ast.value_type list
        -> fall_through:[> `Block of int ]
        -> context:'b list
        -> unit Wa_code_generation.t)
    -> Wa_ast.var
    -> (   result_typ:Wa_ast.value_type list
        -> fall_through:'a
        -> context:'b list
        -> unit Wa_code_generation.t)
    -> unit Wa_code_generation.t

  val post_process_function_body :
       param_count:int
    -> locals:Wa_ast.value_type list
    -> Wa_ast.instruction list
    -> Wa_ast.instruction list

  val entry_point :
       context:Wa_code_generation.context
    -> toplevel_fun:Wa_ast.var
    -> Wa_ast.func_type * unit Wa_code_generation.t
end
