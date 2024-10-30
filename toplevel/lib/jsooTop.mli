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

open Format
(** Helper for Js_of_ocaml Toplevel. *)

val use : formatter -> string -> bool
(** [use fmt content] Execute commands [content]. It does not print types nor values. *)

val execute :
     bool
  -> ?pp_code:formatter
  -> ?highlight_location:(Location.t -> unit)
  -> formatter
  -> string
  -> unit
(** [execute print fmt content] Execute [content]. [print] says whether the values and
    types of the results should be printed. [pp_code] formatter can be use to output ocaml
    source during lexing. *)

val initialize : unit -> unit
(** Initialize Js_of_ocaml toplevel. *)
