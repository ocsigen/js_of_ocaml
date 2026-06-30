(* Js_of_ocaml library
 * http://www.ocsigen.org/js_of_ocaml/
 * Copyright (C) 2014 Hugo Heuzard
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

val lexbuf : string -> int ref -> Format.formatter option -> bytes -> int -> int
(** [lexbuf s p ppf buffer len] is a {!Lexing.from_function}-compatible
    refill function feeding [s] to the lexer one line at a time, tracking the
    current position in [p]. When [ppf] is [Some _], each chunk is echoed to
    it (this is how the toplevel echoes the phrase being read).

    [s] is normalized so that it ends with [;;]: the incremental
    [Toploop.parse_toplevel_phrase] otherwise drops a trailing unterminated
    phrase. *)
