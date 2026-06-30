(* Js_of_ocaml library
 * http://www.ocsigen.org/js_of_ocaml/
 * Copyright (C) 2016 OCamlPro
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

open! Js_of_ocaml_toplevel_protocol

(** The [result] types are defined in the dependency-free [msg] library and
    re-exported here. *)
type loc = Wrapped_intf.loc =
  { loc_start : int * int
  ; loc_end : int * int
  }

type error = Wrapped_intf.error =
  { msg : string
  ; locs : loc list
  }

type warning = error

type 'a result = 'a Wrapped_intf.result =
  | Success of 'a * warning list
  | Error of error * warning list

include
  Wrapped_intf.Wrapped
    with type toplevel := unit
     and type 'a result := 'a result
     and type output := Format.formatter

val clear_check : unit -> unit
(** Discard the scratch typing environment left by [check ~setenv:true],
    restoring the environment as it was just before that call (and thus
    re-enabling {!execute}, {!use} and {!use_mod_string}). All
    definitions actually executed beforehand are kept; only the type-only
    definitions introduced by [check ~setenv:true] are dropped. A no-op when
    no scratch environment is active. *)

val make_lexbuf : ?ppf_code:Format.formatter -> string -> Lexing.lexbuf
(** Build a normalized lexbuf for {!step} from a source string. When [ppf_code]
    is given, the source is echoed to it as it is consumed. The lexbuf holds no
    resource beyond what the GC reclaims; there is nothing to close. *)

val step :
     unit
  -> ?print_outcome:bool
  -> ppf_answer:Format.formatter
  -> Lexing.lexbuf
  -> [ `Phrase of bool | `Eof ] result
(** Parse, preprocess and evaluate the next toplevel phrase from the lexbuf.
    Returns [Success (`Phrase ok)] for a phrase that ran ([ok] is [false] if it
    raised at evaluation time, with the backtrace already on [ppf_answer]),
    [Success `Eof] once the buffer is exhausted, or [Error] on a parse/type
    error. The caller drives the loop and decides whether to continue after a
    failure. *)

val error_of_exn : exn -> error
