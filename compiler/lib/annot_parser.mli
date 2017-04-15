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

(* The type of tokens. *)

type token =
  | TWeakdef
  | TVersion
  | TVNum of (string)
  | TSemi
  | TRequires
  | TProvides
  | TOTHER of (string)
  | TIdent of (string)
  | TComma
  | TA_Shallow
  | TA_Pure
  | TA_Object_literal
  | TA_Mutator
  | TA_Mutable
  | TA_Const
  | RPARENT
  | LT
  | LPARENT
  | LE
  | GT
  | GE
  | EQ
  | EOL
  | EOF

(* This exception is raised by the monolithic API functions. *)

exception Error

(* The monolithic API. *)

val annot: (Lexing.lexbuf -> token) -> Lexing.lexbuf -> (Jsoo_primitive.t)
