type context =
  { constants : (Wa_ast.var, Wa_ast.expression) Hashtbl.t
  ; mutable data_segments : (bool * Wa_ast.data list) Code.Var.Map.t
  ; mutable other_fields : Wa_ast.module_field list
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

  val ( + ) : expression -> expression -> expression

  val ( - ) : expression -> expression -> expression

  val ( * ) : expression -> expression -> expression

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

  val eqz : expression -> expression
end

val load : Wa_ast.var -> expression

val tee : Wa_ast.var -> expression -> expression

val store : Wa_ast.var -> expression -> unit t

val assign : Wa_ast.var -> expression -> unit t

val drop : expression -> unit t

val loop : Wa_ast.func_type -> unit t -> unit t

val block : Wa_ast.func_type -> unit t -> unit t

val if_ : Wa_ast.func_type -> expression -> unit t -> unit t -> unit t

val add_var : Wa_ast.var -> int t

val define_var : Wa_ast.var -> expression -> unit t

val register_global : string -> Wa_ast.global_type -> Wa_ast.expression -> unit t

val get_context : context t

val register_data_segment : Code.Var.t -> active:bool -> Wa_ast.data list -> unit t

val function_body : context:context -> body:unit t -> int * Wa_ast.instruction list
