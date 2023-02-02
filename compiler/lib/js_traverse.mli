(* Js_of_ocaml compiler
 * http://www.ocsigen.org/js_of_ocaml/
 * Copyright (C) 2013 Hugo Heuzard
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
open! Stdlib
open Javascript

class type mapper =
  object
    method loc : Javascript.location -> Javascript.location

    method expression : expression -> expression

    method expression_o : expression option -> expression option

    method switch_case : expression -> expression

    method block : Javascript.statement_list -> Javascript.statement_list

    method fun_decl : Javascript.function_declaration -> Javascript.function_declaration

    method class_decl : Javascript.class_declaration -> Javascript.class_declaration

    method initialiser : expression * location -> expression * location

    method initialiser_o :
      (expression * location) option -> (expression * location) option

    method for_binding :
         Javascript.variable_declaration_kind
      -> Javascript.for_binding
      -> Javascript.for_binding

    method variable_declaration :
         Javascript.variable_declaration_kind
      -> Javascript.variable_declaration
      -> Javascript.variable_declaration

    method statement : statement -> statement

    method statements : statement_list -> statement_list

    method statement_o : (statement * location) option -> (statement * location) option

    method ident : ident -> ident

    method formal_parameter_list :
      Javascript.formal_parameter_list -> Javascript.formal_parameter_list

    method program : program -> program

    method function_body : statement_list -> statement_list
  end

class type iterator =
  object
    method fun_decl : Javascript.function_declaration -> unit

    method early_error : Javascript.early_error -> unit

    method expression : Javascript.expression -> unit

    method expression_o : Javascript.expression option -> unit

    method switch_case : Javascript.expression -> unit

    method initialiser : Javascript.expression * Javascript.location -> unit

    method initialiser_o : (Javascript.expression * Javascript.location) option -> unit

    method for_binding :
      Javascript.variable_declaration_kind -> Javascript.for_binding -> unit

    method variable_declaration :
      Javascript.variable_declaration_kind -> Javascript.variable_declaration -> unit

    method statement : Javascript.statement -> unit

    method statement_o : (Javascript.statement * Javascript.location) option -> unit

    method statements : Javascript.statement_list -> unit

    method ident : Javascript.ident -> unit

    method program : Javascript.program -> unit

    method function_body : Javascript.statement_list -> unit
  end

class map : mapper

class iter : iterator

class subst :
  (ident -> ident)
  -> object
       inherit mapper
     end

type t =
  { use : IdentSet.t
  ; def_var : IdentSet.t
  ; def_local : IdentSet.t
  }

type block =
  | Catch of formal_parameter
  | Params of formal_parameter_list
  | Normal

class type freevar =
  object ('a)
    inherit mapper

    method merge_info : 'a -> unit

    method merge_block_info : 'a -> unit

    method record_block : block -> unit

    method def_var : ident -> unit

    method def_local : Javascript.ident -> unit

    method use_var : ident -> unit

    method state : t

    method get_count : int IdentMap.t

    method get_free : IdentSet.t

    method get_def : IdentSet.t

    method get_use : IdentSet.t
  end

class free : freevar

class rename_variable : mapper

class share_constant : mapper

class compact_vardecl :
  object ('a)
    inherit free

    method exc : IdentSet.t
  end

class clean : mapper

class simpl : mapper
