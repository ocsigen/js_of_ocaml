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
open! Stdlib

type t =
  { src : string option
  ; name : string option
  ; col : int
  ; line : int
  ; idx : int
  }

let zero = { src = None; name = None; col = 0; line = 0; idx = 0 }

let t_of_pos start_p =
  let idx = start_p.Lexing.pos_cnum in
  let line, col = start_p.pos_lnum, start_p.pos_cnum - start_p.pos_bol in
  let name = Some start_p.pos_fname in
  let src = Some start_p.pos_fname in
  { idx; line; col; name; src }

let t_of_lexbuf lexbuf : t = t_of_pos lexbuf.Lexing.lex_start_p

let start_position (t : t) =
  { Lexing.pos_fname = Option.value ~default:"" t.name
  ; pos_lnum = t.line
  ; pos_bol = t.idx - t.col
  ; pos_cnum = t.idx
  }

let t_of_position ~src pos =
  { name = Some pos.Lexing.pos_fname
  ; src
  ; line = pos.Lexing.pos_lnum
  ; col = pos.Lexing.pos_cnum - pos.Lexing.pos_bol
  ; idx = 0
  }

let to_string { name; src; line; col; _ } =
  match name, src with
  | (None | Some ""), (None | Some "") -> "?"
  | _ ->
      let file =
        match name, src with
        | (None | Some ""), Some file -> file
        | Some file, (None | Some "") -> file
        | Some file, Some _file -> file
        | None, None -> assert false
      in
      Format.sprintf "%s:%d:%d" file line col
