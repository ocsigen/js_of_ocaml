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

class type mapper = object
  method expression : Javascript.expression -> Javascript.expression
  method expression_o : Javascript.expression option -> Javascript.expression option
  method initialiser : (Javascript.expression * Javascript.node_pc) -> (Javascript.expression * Javascript.node_pc)
  method initialiser_o : (Javascript.expression * Javascript.node_pc) option -> (Javascript.expression * Javascript.node_pc) option
  method statement : Javascript.statement -> Javascript.statement
  method statements : Javascript.statement list -> Javascript.statement list
  method statement_o : Javascript.statement option -> Javascript.statement option
  method source : Javascript.source_element -> Javascript.source_element
  method sources : Javascript.source_element list -> Javascript.source_element list
  method ident : Javascript.ident -> Javascript.ident
  method program : Javascript.program -> Javascript.program
end

class map : mapper

class subst : (Javascript.ident -> Javascript.ident) ->  object
    inherit mapper
  end

open Util

type t = {
  use_name : StringSet.t;
  def_name : StringSet.t;
  def : Code.VarSet.t;
  use : Code.VarSet.t;
  count : int Javascript.IdentMap.t;
}


class type freevar =
  object('a)
    inherit mapper
    method merge_info : 'a -> unit
    method block : ?catch:bool -> Javascript.ident list -> unit

    method def_var : Javascript.ident -> unit
    method use_var : Javascript.ident -> unit
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
  method exc  : Javascript.IdentSet.t
end

class clean : mapper
class simpl : mapper
