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

  type error

  val of_file : string -> t

  val of_string :
       ?report_error:(error -> unit)
    -> ?pos:Lexing.position
    -> ?filename:string
    -> string
    -> t

  val print_error : error -> unit

  val of_channel : in_channel -> t
end

exception Parsing_error of Parse_info.t * string
(** A syntax error: its location and a message describing it, such as
    ["unexpected `}`, expected `)`"]. *)

val parse : [ `Script | `Module ] -> Lexer.t -> Javascript.program

val parse_annotated :
     [ `Script | `Module ]
  -> Lexer.t
  -> ((Js_token.Annot.t * Parse_info.t) list * Javascript.program) list
(** The top-level statements, grouped under the annotations
    ([//Provides: ...]) that precede them. *)

val parse' :
     [ `Script | `Module ]
  -> Lexer.t
  -> ((Js_token.Annot.t * Parse_info.t) list * Javascript.program) list
     * (Js_token.t * Loc.t) list
(** Like {!parse_annotated}, and also returns all the tokens, including
    comments and virtual semicolons. Keeping them makes parsing noticeably
    slower: only use it when the tokens are needed. *)

val parse_expr : Lexer.t -> Javascript.expression
