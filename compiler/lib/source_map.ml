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

let mapping_of_string str =
  let total_len = String.length str in

  let gen_col = ref 0 in
  let ori_source = ref 0 in
  let ori_line = ref 0 in
  let ori_col = ref 0 in
  let ori_name = ref 0 in

  let rec readline line pos acc =
    if pos >= total_len
    then acc
    else
      let last =
        try
          String.index_from str pos ';'
        with Not_found -> total_len
      in
      gen_col := 0;
      let pos, acc = read_tokens line  pos last acc in
      readline (succ line) pos acc
  and read_tokens line start stop acc =
    let last =
      try
        min (String.index_from str start ',') stop
      with Not_found -> stop
    in
    let v = Vlq64.decode_l str ~pos:start ~len:(last - start)  in
    match v with
    | [] -> last + 1 , acc
    | v ->
      let v =
        match v with
        | [g] ->
          gen_col:= !gen_col + g;
          { gen_line = line;
            gen_col  = !gen_col;
            ori_source = -1;
            ori_line = -1;
            ori_col = -1;
            ori_name = None
          }
        | [g;os;ol;oc] ->
          gen_col:= !gen_col + g;
          ori_source := !ori_source + os;
          ori_line := !ori_line + ol;
          ori_col := !ori_col + oc;
          { gen_line = line;
            gen_col  = !gen_col;
            ori_source = !ori_source;
            ori_line = !ori_line;
            ori_col = !ori_col;
            ori_name = None
          }
        | [g;os;ol;oc;on] ->
          gen_col:= !gen_col + g;
          ori_source := !ori_source + os;
          ori_line := !ori_line + ol;
          ori_col := !ori_col + oc;
          ori_name := !ori_name + on;
          { gen_line = line;
            gen_col  = !gen_col;
            ori_source = !ori_source;
            ori_line = !ori_line;
            ori_col = !ori_col;
            ori_name = Some !ori_name
          }
        | _  -> invalid_arg "Source_map.mapping_of_string"
      in
      let acc = v :: acc in
      if last = stop
      then (last + 1, acc)
      else read_tokens line (last + 1) stop acc
  in
  readline 0 0 []
;;

let () =
  let map_str = ";;;;EAEE,EAAE,EAAC,CAAE;ECQY,UACC" in
  let map = mapping_of_string map_str in
  let map_str' = string_of_mapping map in
  (* let map' = mapping_of_string map_str' in *)
  assert (map_str = map_str')

let merge_sources_content a b =
  match a, b with
  | Some a, Some b -> Some (a @ b)
  | _ -> None

let maps ~gen_line_offset ~sources_offset ~names_offset x =
  let gen_line = x.gen_line + gen_line_offset in
  let ori_source = x.ori_source + sources_offset in
  let ori_name = match x.ori_name with
    | None -> None
    | Some ori_name -> Some (ori_name + names_offset)
  in
  { x with
    gen_line;
    ori_source;
    ori_name }

let merge = function
  | [] -> None
  | (gen_line_offset,_file,x)::rest ->
    let rec loop acc ~sources_offset ~names_offset l =
      match l with
      | [] -> acc
      | (gen_line_offset, _,sm) :: rest  ->
        let acc =
          { acc with
            sources = acc.sources @ sm.sources;
            names   = acc.names @ sm.names;
            sources_content = merge_sources_content acc.sources_content sm.sources_content;
            mappings = acc.mappings
                       @ List.map (maps ~gen_line_offset ~sources_offset ~names_offset) sm.mappings
          }
        in
        loop
          acc
          ~sources_offset:(sources_offset + List.length sm.sources)
          ~names_offset:(names_offset + List.length sm.names)
          rest
    in
    let acc =
      { x with
        mappings = List.map (maps ~gen_line_offset ~sources_offset:0 ~names_offset:0) x.mappings
      }
    in
    Some (loop
            acc
            ~sources_offset:(List.length x.sources)
            ~names_offset:(List.length x.names)
            rest)
(*
let empty = {
  version = 3;
  file = "file";
  sourceroot = None;
  sources = [];
  sources_content = None;
  names = [];
  mappings = []
}

let gen (gen_line, gen_col) (ori_line, ori_col) ori_source =
  { gen_line;
    gen_col;
    ori_source;
    ori_line;
    ori_col;
    ori_name = None;
  }

let s1 =
  { empty with
    names = ["na";"nb";"nc"];
    sources = ["sa";"sb"];
    mappings = [
      gen (1,1) (10,10) 1;
      gen (3,3) (20,20) 2;
    ]
  }

let s2 =
  { empty with
    names = ["na2";"nb2"];
    sources = ["sa2"];
    mappings = [
      gen (3,3) (5,5) 1;
    ]
  }

let () =
  let m = merge [(0,"",s1);(20,"",s2)] in
  begin
    match m with
    | None -> ()
    | Some sm ->
      assert (string_of_mapping sm.mappings = ";CCUU;;GCUU;;;;;;;;;;;;;;;;;;;;GCff")
  end;
  *)
