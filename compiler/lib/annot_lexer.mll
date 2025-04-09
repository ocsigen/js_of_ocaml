{
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
open Annot_parser
}

rule main = parse
  | "Provides" {TProvides}
  | "Requires" {TRequires}
  | "Version" {TVersion}
  | "Weakdef" {TWeakdef}
  | "Always" {TAlways}
  | "If" {TIf}
  | "Alias" {TAlias}
  | "Deprecated: " ([^'\n']* as txt) {TDeprecated txt}
  | "pure" {TA_Pure }
  | "const" {TA_Const }
  | "mutable" {TA_Mutable }
  | "mutator" {TA_Mutator }
  | "shallow" {TA_Shallow}
  | "object_literal" {TA_Object_literal}
  | "!" {TBang}
  | ['a'-'z''A'-'Z''$''_']['a'-'z''A'-'Z''$''_''-''0'-'9']* {
      let x = Lexing.lexeme lexbuf in
      TIdent x}
  | '%' ['a'-'z''A'-'Z''$''_']['a'-'z''A'-'Z''$''_''-''0'-'9']* {
      let x = Lexing.lexeme lexbuf in
      TIdent_percent x}
  | ['0'-'9']+ ('.' (['0'-'9']+)) * {
      let x = Lexing.lexeme lexbuf in
      TVNum x}
  | "(" {LPARENT}
  | ")" {RPARENT}
  | "," {TComma}
  | ":" {TColon}
  | "<=" {LE}
  | "<"  {LT}
  | ">"  {GT}
  | ">=" {GE}
  | "="  {EQ}
  | [' ''\t']+ { main lexbuf }
  | eof { EOF }
  | ['\n'] {EOL}
  | _ { TOTHER(Lexing.lexeme lexbuf) }
