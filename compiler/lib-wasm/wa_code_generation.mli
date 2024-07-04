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

open Stdlib

type constant_global

type context =
  { constants : (Code.Var.t, Wa_ast.expression) Hashtbl.t
  ; mutable data_segments : string Code.Var.Map.t
  ; mutable constant_globals : constant_global Code.Var.Map.t
  ; mutable other_fields : Wa_ast.module_field list
  ; mutable imports : (Code.Var.t * Wa_ast.import_desc) StringMap.t StringMap.t
  ; type_names : (string, Code.Var.t) Hashtbl.t
  ; types : (Code.Var.t, Wa_ast.type_field) Hashtbl.t
  ; mutable closure_envs : Code.Var.t Code.Var.Map.t
        (** GC: mapping of recursive functions to their shared environment *)
  ; mutable apply_funs : Code.Var.t Stdlib.IntMap.t
  ; mutable cps_apply_funs : Code.Var.t Stdlib.IntMap.t
  ; mutable curry_funs : Code.Var.t Stdlib.IntMap.t
  ; mutable cps_curry_funs : Code.Var.t Stdlib.IntMap.t
  ; mutable dummy_funs : Code.Var.t Stdlib.IntMap.t
  ; mutable cps_dummy_funs : Code.Var.t Stdlib.IntMap.t
  ; mutable init_code : Wa_ast.instruction list
  ; mutable string_count : int
  ; mutable strings : string list
  ; mutable string_index : int StringMap.t
  ; mutable fragments : Javascript.expression StringMap.t
  ; mutable globalized_variables : Code.Var.Set.t
  ; value_type : Wa_ast.value_type
  ; mutable unit_name : string option
  }

val make_context : value_type:Wa_ast.value_type -> context

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

val try_expr :
  Wa_ast.func_type -> unit t -> (Code.Var.t * int * Wa_ast.value_type) list -> expression

val add_var : ?typ:Wa_ast.value_type -> Wa_ast.var -> Wa_ast.var t

val define_var : Wa_ast.var -> expression -> unit t

val is_small_constant : Wa_ast.expression -> bool t

val get_i31_value : Wa_ast.var -> Wa_ast.var option t

val event : Parse_info.t -> unit t

val no_event : unit t

val hidden_location : Parse_info.t

type type_def =
  { supertype : Wa_ast.var option
  ; final : bool
  ; typ : Wa_ast.str_type
  }

val register_type : string -> (unit -> type_def t) -> Wa_ast.var t

val heap_type_sub : Wa_ast.heap_type -> Wa_ast.heap_type -> bool t

val register_import :
  ?import_module:string -> name:string -> Wa_ast.import_desc -> Wa_ast.var t

val register_global :
     Wa_ast.var
  -> ?exported_name:string
  -> ?constant:bool
  -> Wa_ast.global_type
  -> Wa_ast.expression
  -> unit t

val get_global : Code.Var.t -> Wa_ast.expression option t

val register_data_segment : Code.Var.t -> string -> unit t

val register_init_code : unit t -> unit t

val init_code : context -> unit t

val register_string : string -> int t

val register_fragment : string -> (unit -> Javascript.expression) -> unit t

val get_context : context t

val set_closure_env : Code.Var.t -> Code.Var.t -> unit t

val get_closure_env : Code.Var.t -> Code.Var.t t

val is_closure : Code.Var.t -> bool t

val unit_name : string option t

val need_apply_fun : cps:bool -> arity:int -> Code.Var.t t

val need_curry_fun : cps:bool -> arity:int -> Code.Var.t t

val need_dummy_fun : cps:bool -> arity:int -> Code.Var.t t

val function_body :
     context:context
  -> param_names:Code.Var.t list
  -> body:unit t
  -> (Wa_ast.var * Wa_ast.value_type) list * Wa_ast.instruction list
