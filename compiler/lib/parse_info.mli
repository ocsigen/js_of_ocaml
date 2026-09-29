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

type t =
  { src : string option
        (** Path of the source file, when it is known to exist; source maps
            and source excerpts use it. *)
  ; name : string option  (** Name of the source file, as given by the producer. *)
  ; col : int
        (** Column, 0-based ([pos_cnum - pos_bol]), in code points for
            JavaScript sources and in bytes for OCaml sources. Add 1 when
            printing a location for an editor. *)
  ; line : int  (** Line, 1-based ([pos_lnum]). *)
  ; idx : int
        (** Offset from the start of the lexed buffer ([pos_cnum]), in code
            points. Only set by the JavaScript lexer; 0 for OCaml positions. *)
  }
(** A source position, as a {!Lexing.position} with the file name split
    into the name the producer knew and the path resolved on disk.

    Positions come from two producers with slightly different conventions:
    - the JavaScript lexer ({!t_of_pos}, {!t_of_lexbuf}), which counts in
      code points and knows the file only by the name it was given, so
      [src] and [name] are the same, possibly [Some ""] for an anonymous
      string;
    - the OCaml debug events of a bytecode file ({!t_of_position}), whose
      positions are those of the OCaml compiler, in bytes, with [name]
      the file name recorded by the compiler (relative to where it ran)
      and [src] the path where js_of_ocaml found the source, if it did. *)

val zero : t
(** No position: no file, line 0, column 0. *)

val equal : t -> t -> bool

val t_of_lexbuf : Lexing.lexbuf -> t
(** Position of the start of the current lexeme. *)

val t_of_pos : Lexing.position -> t
(** Position from the JavaScript lexer: [src] and [name] are both the
    [pos_fname]. *)

val start_position : t -> Lexing.position
(** Inverse of {!t_of_pos}. *)

val t_of_position : src:string option -> Lexing.position -> t
(** Position from an OCaml debug event: [name] is the [pos_fname] of the
    event, [src] the resolved path of the source file. *)

val to_string : t -> string
(** [file:line:col] with the column as stored (0-based), or ["?"] when no
    file name is known. *)
