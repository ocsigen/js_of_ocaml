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
  { src  : string option
  ; name : string option
  ; col  : int
  ; line : int
  ; idx  : int
  ; fol  : bool option;
  }

let zero =
  { src  = None
  ; name = None
  ; col  = 0
  ; line = 0
  ; idx  = 0
  ; fol  = None
  }

module Line_info = struct

  type t_above = t
  type t = {
    mutable acc_pos : int;
    mutable acc_line : int;
    offset : t_above option;
    lines : int array;
    name : string option;
    src : string option
  }

  let rec compute lines acc line pos =
    if line >= Array.length lines
    then
      if pos = 0 then acc, line,0 else assert false
    else
    if lines.(line) >= pos
    then acc, line, pos
    else compute lines (acc + lines.(line) + 1) (succ line) (pos - lines.(line) - 1)

  let get t pos =
    let acc,line,pos =
      if t.acc_pos <= pos
      then
        compute t.lines t.acc_pos t.acc_line (pos - t.acc_pos)
      else
        compute t.lines 0 0 pos
    in
    t.acc_pos <- acc;
    t.acc_line <- line;
    line,pos

  let from_file file =
    let ic = open_in file in
    let lines = ref [] in
    (try
       while true do
         lines:=String.length (input_line ic) :: !lines
       done with End_of_file -> ());
    let lines = Array.of_list (List.rev !lines) in
    let t = {
      acc_pos = 0;
      acc_line = 0;
      offset = None;
      lines;
      name = Some file;
      src  = Some file;
    } in
    close_in ic;
    t

  let from_string ?offset str =
    let pos = ref 0
    and lines = ref [] in
    (try
       while true do
         let idx = String.index_from str !pos '\n' in
         lines:=(idx - !pos)::!lines;
         pos:=idx+1;
       done
     with Not_found -> lines:= (String.length str - !pos) :: !lines);
    let lines = Array.of_list (List.rev !lines) in
    { acc_pos = 0;
      acc_line = 0;
      offset;
      lines;
      name=None;
      src =None}

  let from_channel ic =
    let buf = Buffer.create 1024 in
    let lines = ref [] in
    (try
       while true do
         let l = input_line ic in
         Buffer.add_string buf l;
         Buffer.add_char buf '\n';

         lines:=String.length l :: !lines
       done with End_of_file -> ());
    let lines = Array.of_list (List.rev !lines) in
    let t = {
      acc_pos = 0;
      acc_line = 0;
      offset = None;
      lines;
      name=None;
      src=None;
    } in
    t,Buffer.contents buf

end

type lineinfo = Line_info.t

let relative_path {Line_info.src} file =
  match src with
  | None -> None
  | Some src -> Some (Filename.(concat (dirname src) file))

let make_lineinfo_from_file file = Line_info.from_file file

let make_lineinfo_from_string ?offset str = Line_info.from_string ?offset str

let make_lineinfo_from_channel c = Line_info.from_channel c

let t_of_lexbuf line_info lexbuf : t =
  let idx = lexbuf.Lexing.lex_start_p.Lexing.pos_cnum in
  let line,col = Line_info.get line_info idx in
  let line,col = match line_info.Line_info.offset with
    | None -> line, col
    | Some {line = line_offset; col = col_offset; _} ->
      line + line_offset, col + col_offset
  in
  let name = match line_info.Line_info.offset with
    | Some { name = Some _ as name } -> name
    | _ -> line_info.Line_info.name
  in
  let src = match line_info.Line_info.offset with
    | Some {src = Some _ as src } -> src
    | _ -> line_info.Line_info.src
  in
  { fol = None
  ; idx
  ; line
  ; col
  ; name
  ; src
  }
