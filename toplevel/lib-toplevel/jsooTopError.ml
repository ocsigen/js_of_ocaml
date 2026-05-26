(* Js_of_ocaml library
 * http://www.ocsigen.org/js_of_ocaml/
 * Copyright (C) 2017 Hugo Heuzard
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

let loc = function
  | Syntaxerr.Error x -> Some (Syntaxerr.location_of_error x)
  | Lexer.Error (_, loc)
  | ((Typecore.Error (loc, _, _)) [@if ocaml_version < (5, 6, 0)])
  | ((Typecore.Error.In_context (loc, _, _)) [@if ocaml_version >= (5, 6, 0)])
  | ((Typetexp.Error (loc, _, _)) [@if ocaml_version < (5, 6, 0)])
  | ((Typetexp.Error.In_context (loc, _, _)) [@if ocaml_version >= (5, 6, 0)])
  | ((Typeclass.Error (loc, _, _)) [@if ocaml_version < (5, 6, 0)])
  | ((Typeclass.Error.In_context (loc, _, _)) [@if ocaml_version >= (5, 6, 0)])
  | ((Typemod.Error (loc, _, _)) [@if ocaml_version < (5, 6, 0)])
  | ((Typemod.Errors (loc, _)) [@if ocaml_version >= (5, 6, 0)])
  | ((Typedecl.Error (loc, _)) [@if ocaml_version < (5, 6, 0)])
  | ((Typedecl.Error.In_context (loc, _)) [@if ocaml_version >= (5, 6, 0)])
  | Translcore.Error (loc, _)
  | Translclass.Error (loc, _)
  | Translmod.Error (loc, _) -> Some loc
  | _ -> None
