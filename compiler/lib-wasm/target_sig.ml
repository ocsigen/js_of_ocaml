(* Wasm_of_ocaml compiler
 * http://www.ocsigen.org/js_of_ocaml/
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU Lesser General Public License as published by
 * the Free Software Foundation, with linking exception;
 * either version 2.1 of the License, or (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
 *)

module type S = sig
  type expression = Code_generation.expression

  module Memory : sig
    val allocate :
         tag:int
      -> deadcode_sentinal:Code.Var.t
      -> [ `Expr of Wasm_ast.expression | `Var of Wasm_ast.var ] list
      -> expression

    val load_function_pointer :
         cps:bool
      -> arity:int
      -> ?skip_cast:bool
      -> expression
      -> (Wasm_ast.var * Wasm_ast.expression) Code_generation.t

    val load_real_closure :
         cps:bool
      -> arity:int
      -> expression
      -> (Wasm_ast.var * Wasm_ast.expression) Code_generation.t

    val check_function_arity :
         Code.Var.t
      -> cps:bool
      -> arity:int
      -> (typ:Wasm_ast.value_type option -> expression -> expression)
      -> unit Code_generation.t
      -> unit Code_generation.t

    val tag : expression -> expression

    val field : expression -> int -> expression

    val set_field : expression -> int -> expression -> unit Code_generation.t

    val array_get : expression -> expression -> expression

    val array_set : expression -> expression -> expression -> unit Code_generation.t

    val float_array_get : expression -> expression -> expression

    val float_array_set : expression -> expression -> expression -> unit Code_generation.t

    val check_is_float_array : expression -> expression

    val gen_array_get : expression -> expression -> expression

    val gen_array_set : expression -> expression -> expression -> unit Code_generation.t

    val array_length : expression -> expression

    val float_array_length : expression -> expression

    val gen_array_length : expression -> expression

    val bytes_length : expression -> expression

    val bytes_get : expression -> expression -> expression

    val bytes_set : expression -> expression -> expression -> unit Code_generation.t

    val box_float : expression -> expression

    val unbox_float : expression -> expression

    val box_int32 : expression -> expression

    val unbox_int32 : expression -> expression

    val box_int64 : expression -> expression

    val unbox_int64 : expression -> expression

    val box_nativeint : expression -> expression

    val unbox_nativeint : expression -> expression
  end

  module Type : sig
    val value : Wasm_ast.value_type

    val func_type : int -> Wasm_ast.func_type

    val primitive_type : int -> Wasm_ast.func_type
  end

  module Value : sig
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

    val block_type : Wasm_ast.value_type Code_generation.t

    val dummy_block : expression

    val as_block : expression -> expression
  end

  module Constant : sig
    val translate : Code.constant -> expression
  end

  module Closure : sig
    val translate :
         context:Code_generation.context
      -> closures:Closure_conversion.closure Code.Var.Map.t
      -> cps:bool
      -> Code.Var.t
      -> expression

    val bind_environment :
         context:Code_generation.context
      -> closures:Closure_conversion.closure Code.Var.Map.t
      -> cps:bool
      -> Code.Var.t
      -> unit Code_generation.t

    val curry_allocate :
         cps:bool
      -> arity:int
      -> int
      -> f:Code.Var.t
      -> closure:Code.Var.t
      -> arg:Code.Var.t
      -> Wasm_ast.expression Code_generation.t

    val curry_load :
         cps:bool
      -> arity:int
      -> int
      -> Code.Var.t
      -> (expression * expression * Wasm_ast.value_type option) Code_generation.t

    val dummy : cps:bool -> arity:int -> Wasm_ast.expression Code_generation.t
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
    (string
    * Primitive.kind
    * ((Code.prim_arg -> expression) -> Code.prim_arg list -> expression))
    list

  val handle_exceptions :
       result_typ:Wasm_ast.value_type list
    -> fall_through:'a
    -> context:([> `Catch | `Skip ] as 'b) list
    -> (   result_typ:Wasm_ast.value_type list
        -> fall_through:[> `Skip ]
        -> context:'b list
        -> unit Code_generation.t)
    -> Wasm_ast.var
    -> (   result_typ:Wasm_ast.value_type list
        -> fall_through:'a
        -> context:'b list
        -> unit Code_generation.t)
    -> unit Code_generation.t

  val post_process_function_body :
       param_names:Wasm_ast.var list
    -> locals:(Wasm_ast.var * Wasm_ast.value_type) list
    -> Wasm_ast.instruction list
    -> Wasm_ast.instruction list

  val entry_point :
       toplevel_fun:Wasm_ast.var
    -> Wasm_ast.func_type * Wasm_ast.var list * unit Code_generation.t
end
