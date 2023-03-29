type 'a t

type expression = Wa_ast.expression t

val ( let* ) : 'a t -> ('a -> 'b t) -> 'b t

val return : 'a -> 'a t

val instr : Wa_ast.instruction -> unit t

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

val function_body : body:unit t -> int * Wa_ast.instruction list
