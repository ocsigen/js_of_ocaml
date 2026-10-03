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

let equal a b =
  a.line = b.line
  && a.col = b.col
  && a.idx = b.idx
  && Option.equal String.equal a.src b.src
  && Option.equal String.equal a.name b.name

(* A parser calls [t_of_pos] for every location of a file: share the
   [Some pos_fname] between them rather than allocating two for each. *)
let last_fname = ref ("", Some "")

let t_of_pos start_p =
  let idx = start_p.Lexing.pos_cnum in
  let line, col = start_p.pos_lnum, start_p.pos_cnum - start_p.pos_bol in
  let fname = start_p.pos_fname in
  let name =
    let last, name = !last_fname in
    if phys_equal last fname
    then name
    else
      let name = Some fname in
      last_fname := fname, name;
      name
  in
  { idx; line; col; name; src = name }

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

let file { name; src; _ } =
  match name, src with
  | (None | Some ""), (None | Some "") -> None
  | (None | Some ""), Some file | Some file, _ -> Some file

module Debug = struct
  let to_string ({ line; col; _ } as t) =
    match file t with
    | None -> "?"
    | Some file -> Format.sprintf "%s:%d:%d" file line col
end

module Diagnostic = struct
  let to_string ({ line; col; _ } as t) =
    let col = col + 1 in
    match file t with
    | None -> Format.sprintf "line %d, column %d" line col
    | Some file -> Format.sprintf "%s:%d:%d" file line col

  (* The code points of the [n]th line of a file (starting from 1), if it
     can be read. Lines end where the JavaScript lexer ends them: at
     "\r\n", '\n', '\r', U+2028 and U+2029. *)
  let source_line file n =
    match Fs.read_file file with
    | exception Failure _ -> None
    | s ->
        let line, _, rev_line =
          String.fold_utf_8 s (1, false, []) ~f:(fun (line, cr, acc) _ u ->
              match Uchar.to_int u with
              | 0x0a -> (if cr then line else line + 1), false, acc
              | 0x0d -> line + 1, true, acc
              | 0x2028 | 0x2029 -> line + 1, false, acc
              | _ -> line, false, if line = n then u :: acc else acc)
        in
        if n < 1 || line < n then None else Some (Array.of_list (List.rev rev_line))

  (* At most this many code points of the offending line are printed *)
  let excerpt_width = 100

  let with_excerpt t message =
    let msg = Printf.sprintf "%s: %s" (to_string t) message in
    match Option.bind (file t) ~f:(fun file -> source_line file t.line) with
    | None -> msg
    | Some line ->
        (* Columns count code points. Only print a window around the column
           of a long line (minified code, for instance). *)
        let len = Array.length line in
        let first, last =
          if len <= excerpt_width
          then 0, len
          else
            let first = max 0 (min (t.col - (excerpt_width / 2)) (len - excerpt_width)) in
            first, first + excerpt_width
        in
        let text = Buffer.create 128 in
        let marker = Buffer.create 128 in
        if first > 0
        then (
          Buffer.add_string text "...";
          Buffer.add_string marker "   ");
        for i = first to last - 1 do
          Buffer.add_utf_8_uchar text line.(i)
        done;
        if last < len then Buffer.add_string text "...";
        (* Keep the tabs to stay aligned *)
        for i = first to t.col - 1 do
          Buffer.add_char
            marker
            (if i < len && Uchar.equal line.(i) (Uchar.of_char '\t') then '\t' else ' ')
        done;
        let num = string_of_int t.line in
        Printf.sprintf
          "%s\n%s | %s\n%s | %s^"
          msg
          num
          (Buffer.contents text)
          (String.make (String.length num) ' ')
          (Buffer.contents marker)
end
