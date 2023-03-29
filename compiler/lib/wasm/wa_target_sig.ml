module type S = sig
  type expression = Wa_code_generation.expression

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

    val int_neg : expression -> expression

    val int_or : expression -> expression -> expression

    val int_and : expression -> expression -> expression

    val int_xor : expression -> expression -> expression

    val int_lsl : expression -> expression -> expression

    val int_lsr : expression -> expression -> expression

    val int_asr : expression -> expression -> expression
  end
end
