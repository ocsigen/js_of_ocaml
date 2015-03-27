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

type map = {
  gen_line : int;
  gen_col : int;
  ori_source : int;
  ori_line : int;
  ori_col : int;
  ori_name : int option
}

type mapping = map list

type t = {
  version : int;
  file : string;
  sourceroot : string option;
  mutable sources : string list;
  mutable sources_content : string option list option;
  mutable names : string list;
  mutable mappings : mapping ;
}

let string_of_mapping mapping =
  let a = Array.of_list mapping in
  let len = Array.length a in
  Array.stable_sort (fun t1 t2 ->
    match compare t1.gen_line t2.gen_line with
      | 0 -> compare t1.gen_col t2.gen_col
      | n -> n) a;
  let buf = Buffer.create 1024 in

  let gen_line = ref 0 in
  let gen_col = ref 0 in
  let ori_source = ref 0 in
  let ori_line = ref 0 in
  let ori_col = ref 0 in
  let ori_name = ref 0 in

  let rec loop prev i =
    if i < len then
      let c = a.(i) in
      if
        prev >= 0 &&
        c.ori_source = a.(prev).ori_source &&
        c.ori_line = a.(prev).ori_line &&
        c.ori_col = a.(prev).ori_col
      then
        (* We already are at this location *)
        loop prev (i + 1)
      else if
        i + 1 < len &&
        c.gen_line = a.(i+1).gen_line && c.gen_col = a.(i+1).gen_col
      then
        (* Only keep one source location per generated location *)
        loop prev (i + 1)
      else begin
        if !gen_line <> c.gen_line then begin
          assert (!gen_line < c.gen_line);
          for _i = !gen_line to c.gen_line - 1 do
            Buffer.add_char buf ';';
          done;
          gen_col := 0; gen_line := c.gen_line
        end else if i > 0 then
          Buffer.add_char buf ',';
          let l =
            c.gen_col - !gen_col ::
            if c.ori_source = -1 then
              []
            else
              c.ori_source - !ori_source ::
              c.ori_line - !ori_line ::
              c.ori_col - !ori_col ::
              match c.ori_name with
              | None   -> []
              | Some n -> let n' = !ori_name in ori_name := n; [n - n']
          in
          gen_col := c.gen_col;
          if c.ori_source <> -1 then begin
            ori_source := c.ori_source;
            ori_line := c.ori_line;
            ori_col := c.ori_col
          end;
          Vlq64.encode_l buf l;
          loop i (i + 1)
        end
  in
  loop (-1) 0;
  Buffer.contents buf

let json t =
  `O [
     "version",       `Float  (float_of_int t.version);
     "file",          `String t.file;
     "sourceRoot",    `String (match t.sourceroot with None -> "" | Some s -> s);
     "names",         `A (List.map (fun s -> `String s) t.names);
     "mappings",      `String (string_of_mapping t.mappings);
     "sources",       `A (List.map (fun s -> `String s) t.sources);
     "sourcesContent",`A
		     (match t.sources_content with
		      | None -> []
		      | Some l ->
			 List.map
			   (function
			     | None -> `Null
			     | Some s -> `String s) l);
   ]
