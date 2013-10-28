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

type t = {
  name : string;
  col : int;
  line : int;
  idx : int;
}

let zero = {
  name = "";
  col = 0;
  line = 0;
  idx = 0;
}


module Line_info = struct

  type t = {
    mutable acc_pos : int;
    mutable acc_line : int;
    lines : int array;
    name : string;
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
        (* let () = Printf.printf "current pos: %d   line: %d\n" t.acc_pos t.acc_line in *)
        compute t.lines t.acc_pos t.acc_line (pos - t.acc_pos)
      else
        (* let () = Printf.printf "acc:%d < %d\n" t.acc_pos pos in *)
        (* let () = assert false in *)
        compute t.lines 0 0 pos
    in
    t.acc_pos <- acc;
    t.acc_line <- line;
    line + 1 ,pos

  let from_file file =
    let ic = open_in file in
    let rec read_lines (acc : int list) : int list = try read_lines (String.length (input_line ic)::acc) with End_of_file -> List.rev acc in
    let l = read_lines [] in
    let lines = Array.of_list l in
    let t = {
      acc_pos = 0;
      acc_line = 0;
      lines;
      name=file;
    } in
    close_in ic;
    t

  let from_string str =
    let rec loop pos acc =
      try
        let idx = String.index_from str pos '\n' in
        loop (idx + 1) ((idx - pos)::acc)
      with Not_found ->
        let l = List.rev acc in
        Array.of_list l
    in
    let lines = loop 0 [] in
    { acc_pos = 0;
      acc_line = 0;
      lines;
      name=""}
end

type lineinfo = Line_info.t

let make_lineinfo_from_file file = Line_info.from_file file

let make_lineinfo_from_string str = Line_info.from_string str

let t_of_lexbuf line_info lexbuf : t =
  let idx = lexbuf.Lexing.lex_start_p.Lexing.pos_cnum in
  let line,col = Line_info.get line_info idx in
  {
    idx;
    line;
    col;
    name = line_info.Line_info.name;
  }
