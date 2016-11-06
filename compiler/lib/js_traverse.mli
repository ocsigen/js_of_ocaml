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

open Javascript

class type mapper = object
  method expression : expression -> expression
  method expression_o : expression option -> expression option
  method switch_case : expression -> expression
  method initialiser : (expression * location) -> (expression * location)
  method initialiser_o : (expression * location) option -> (expression * location) option
  method statement : statement -> statement
  method statements : statement_list -> statement_list
  method statement_o : (statement * location) option -> (statement * location) option
  method source : source_element -> source_element
  method sources : source_elements -> source_elements
  method ident : ident -> ident
  method program : program -> program
end

class map : mapper

class subst : (ident -> ident) ->  object
    inherit mapper
  end

open Util

type t = {
  use_name : StringSet.t;
  def_name : StringSet.t;
  def : Code.VarSet.t;
  use : Code.VarSet.t;
  count : int IdentMap.t;
}


class type freevar =
  object('a)
    inherit mapper
    method merge_info : 'a -> unit
    method block : ?catch:bool -> ident list -> unit

    method def_var : ident -> unit
    method use_var : ident -> unit
    method state : t
    method get_free_name : Util.StringSet.t
    method get_free : Code.VarSet.t
    method get_def_name : Util.StringSet.t
    method get_def : Code.VarSet.t
    method get_use_name : Util.StringSet.t
    method get_use : Code.VarSet.t
  end

class free : freevar

class rename_variable : Util.StringSet.t -> freevar

class share_constant : mapper

class compact_vardecl : object('a)
  inherit free
  method exc  : IdentSet.t
end

class clean : mapper
class simpl : mapper
