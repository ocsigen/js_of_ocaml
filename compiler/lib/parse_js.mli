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

module Lexer : sig
  type t

  val of_file : ?rm_comment:bool -> string -> t

  val of_lexbuf : ?rm_comment:bool -> Lexing.lexbuf -> t

  val of_channel : ?rm_comment:bool -> in_channel -> t

  val fold : f:('a -> Js_token.t -> 'a) -> init:'a -> t -> 'a

  val of_list : Js_token.t list -> t
end

exception Parsing_error of Parse_info.t

val parse : Lexer.t -> Javascript.program

val parse_expr : Lexer.t -> Javascript.expression
