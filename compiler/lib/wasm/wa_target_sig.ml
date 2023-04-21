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
         arity:int
      -> ?skip_cast:bool
      -> expression
      -> ([ `Index | `Ref of Wa_ast.var ] * Wa_ast.expression) Wa_code_generation.t

    val load_function_arity : expression -> expression

    val tag : expression -> expression

    val field : expression -> int -> expression

    val set_field : expression -> int -> expression -> unit Wa_code_generation.t

    val array_get : expression -> expression -> expression

    val array_set : expression -> expression -> expression -> unit Wa_code_generation.t

    val bytes_get : expression -> expression -> expression

    val bytes_set : expression -> expression -> expression -> unit Wa_code_generation.t

    val block_length : expression -> expression
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
      -> Code.Var.t
      -> expression

    val bind_environment :
         context:Wa_code_generation.context
      -> closures:Wa_closure_conversion.closure Code.Var.Map.t
      -> Code.Var.t
      -> unit Wa_code_generation.t

    val curry_allocate :
         stack_ctx:Stack.ctx
      -> x:Code.Var.t
      -> arity:int
      -> int
      -> f:Code.Var.t
      -> closure:Code.Var.t
      -> arg:Code.Var.t
      -> Wa_ast.expression Wa_code_generation.t

    val curry_load :
         arity:int
      -> int
      -> Code.Var.t
      -> (expression * expression * Wa_ast.value_type option) Wa_code_generation.t
  end

  val entry_point : context:Wa_code_generation.context -> unit Wa_code_generation.t
end
