(* Js_of_ocaml compiler
 * http://www.ocsigen.org/js_of_ocaml/
 * Copyright (C) 2023 Hugo Heuzard
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

type line =
  { pos_fname : string
  ; pos_lnum : int
  ; pos_bol : int
  }

type t =
  | SameLine of
      { line : line
      ; cnum_start : int
      ; offset : int
      }
  | MultiLine of
      { line_start : line
      ; cnum_start : int
      ; line_end : line
      ; offset : int
      }

let filename = function
  | SameLine { line = { pos_fname; _ }; _ } -> pos_fname
  | MultiLine { line_start = { pos_fname; _ }; _ } -> pos_fname

let line' = function
  | SameLine { line; _ } -> line
  | MultiLine { line_start; _ } -> line_start

let line_end' = function
  | SameLine { line; _ } -> line
  | MultiLine { line_end; _ } -> line_end

let cnum = function
  | SameLine { cnum_start; _ } -> cnum_start
  | MultiLine { cnum_start; _ } -> cnum_start

let line t = (line' t).pos_lnum

let line_end t = (line_end' t).pos_lnum

let column = function
  | SameLine { line = { pos_bol; _ }; cnum_start; _ } -> cnum_start - pos_bol
  | MultiLine { line_start = { pos_bol; _ }; cnum_start; _ } -> cnum_start - pos_bol

let dummy_line = { pos_fname = "<dummy>"; pos_lnum = 0; pos_bol = 0 }

let dummy = SameLine { line = dummy_line; cnum_start = 0; offset = 0 }

let create ?(last_line = dummy_line) (p1 : Lexing.position) (p2 : Lexing.position) : t =
  if p1.pos_fname = p2.pos_fname && p1.pos_lnum = p2.pos_lnum && p1.pos_bol = p2.pos_bol
  then
    let line =
      if
        last_line.pos_fname == p1.pos_fname
        && last_line.pos_lnum = p1.pos_lnum
        && last_line.pos_bol = p1.pos_bol
      then last_line
      else { pos_fname = p1.pos_fname; pos_lnum = p1.pos_lnum; pos_bol = p1.pos_bol }
    in
    SameLine { line; cnum_start = p1.pos_cnum; offset = p2.pos_cnum - p1.pos_cnum }
  else
    let line_start =
      { pos_fname = p1.pos_fname; pos_lnum = p1.pos_lnum; pos_bol = p1.pos_bol }
    in
    let line_end =
      { pos_fname = p2.pos_fname; pos_lnum = p2.pos_lnum; pos_bol = p2.pos_bol }
    in
    MultiLine
      { line_start
      ; cnum_start = p1.pos_cnum
      ; line_end
      ; offset = p2.pos_cnum - p1.pos_cnum
      }

let p1 : t -> Lexing.position = function
  | SameLine { line = { pos_fname; pos_lnum; pos_bol }; cnum_start; offset = _ } ->
      { pos_bol; pos_lnum; pos_fname; pos_cnum = cnum_start }
  | MultiLine
      { line_start = { pos_fname; pos_lnum; pos_bol }
      ; cnum_start
      ; line_end = _
      ; offset = _
      } -> { pos_bol; pos_lnum; pos_fname; pos_cnum = cnum_start }

let p2 : t -> Lexing.position = function
  | SameLine { line = { pos_fname; pos_lnum; pos_bol }; cnum_start; offset } ->
      { pos_bol; pos_lnum; pos_fname; pos_cnum = cnum_start + offset }
  | MultiLine
      { line_end = { pos_fname; pos_lnum; pos_bol }; line_start = _; cnum_start; offset }
    -> { pos_bol; pos_lnum; pos_fname; pos_cnum = cnum_start + offset }
