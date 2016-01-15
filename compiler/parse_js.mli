
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



type lexer

exception Parsing_error of Parse_info.t

val strip_comment : lexer -> lexer

val lexer_from_file : ?rm_comment:bool -> string -> lexer
val lexer_from_string : ?rm_comment:bool -> ?offset:Parse_info.t -> string -> lexer
val lexer_from_channel : ?rm_comment:bool -> in_channel -> lexer
val lexer_map : (Js_token.token -> Js_token.token) -> lexer -> lexer
val lexer_fold : ('a -> Js_token.token -> 'a) -> 'a -> lexer -> 'a
val lexer_filter : (Js_token.token -> bool) -> lexer -> lexer
val lexer_from_list : Js_token.token list -> lexer

val parse : lexer -> Javascript.program
val parse_expr : lexer -> Javascript.expression
