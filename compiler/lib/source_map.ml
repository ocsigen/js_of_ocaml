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

type map =
  | Gen of
      { gen_line : int
      ; gen_col : int
      }
  | Gen_Ori of
      { gen_line : int
      ; gen_col : int
      ; ori_source : int
      ; ori_line : int
      ; ori_col : int
      }
  | Gen_Ori_Name of
      { gen_line : int
      ; gen_col : int
      ; ori_source : int
      ; ori_line : int
      ; ori_col : int
      ; ori_name : int
      }

type mapping = map list

type t =
  { version : int
  ; file : string
  ; sourceroot : string option
  ; sources : string list
  ; sources_content : string option list option
  ; names : string list
  ; mappings : mapping
  }

let empty ~filename =
  { version = 3
  ; file = filename
  ; sourceroot = None
  ; sources = []
  ; sources_content = None
  ; names = []
  ; mappings = []
  }

let gen_line = function
  | Gen { gen_line; _ } | Gen_Ori { gen_line; _ } | Gen_Ori_Name { gen_line; _ } ->
      gen_line

let gen_col = function
  | Gen { gen_col; _ } | Gen_Ori { gen_col; _ } | Gen_Ori_Name { gen_col; _ } -> gen_col

let string_of_mapping mapping =
  let a = Array.of_list mapping in
  let len = Array.length a in
  Array.stable_sort
    ~cmp:(fun t1 t2 ->
      match compare (gen_line t1) (gen_line t2) with
      | 0 -> compare (gen_col t1) (gen_col t2)
      | n -> n)
    a;
  let buf = Buffer.create 1024 in
  (* The binary format encodes lines starting at zero, but
     [ori_line] and [gen_line] are 1 based. *)
  let gen_line_r = ref 1 in
  let gen_col_r = ref 0 in
  let ori_source_r = ref 0 in
  let ori_line_r = ref 1 in
  let ori_col_r = ref 0 in
  let ori_name_r = ref 0 in
  let rec loop prev i =
    if i < len
    then
      let c = a.(i) in
      if i + 1 < len && gen_line c = gen_line a.(i + 1) && gen_col c = gen_col a.(i + 1)
      then (* Only keep one source location per generated location *)
        loop prev (i + 1)
      else (
        if !gen_line_r <> gen_line c
        then (
          assert (!gen_line_r < gen_line c);
          for _i = !gen_line_r to gen_line c - 1 do
            Buffer.add_char buf ';'
          done;
          gen_col_r := 0;
          gen_line_r := gen_line c)
        else if i > 0
        then Buffer.add_char buf ',';
        let l =
          match c with
          | Gen { gen_line = _; gen_col } ->
              let res = [ gen_col - !gen_col_r ] in
              gen_col_r := gen_col;
              res
          | Gen_Ori { gen_line = _; gen_col; ori_source; ori_line; ori_col } ->
              let res =
                [ gen_col - !gen_col_r
                ; ori_source - !ori_source_r
                ; ori_line - !ori_line_r
                ; ori_col - !ori_col_r
                ]
              in
              gen_col_r := gen_col;
              ori_col_r := ori_col;
              ori_line_r := ori_line;
              ori_source_r := ori_source;
              res
          | Gen_Ori_Name
              { gen_line = _; gen_col; ori_source; ori_line; ori_col; ori_name } ->
              let res =
                [ gen_col - !gen_col_r
                ; ori_source - !ori_source_r
                ; ori_line - !ori_line_r
                ; ori_col - !ori_col_r
                ; ori_name - !ori_name_r
                ]
              in
              gen_col_r := gen_col;
              ori_col_r := ori_col;
              ori_line_r := ori_line;
              ori_source_r := ori_source;
              ori_name_r := ori_name;
              res
        in
        Vlq64.encode_l buf l;
        loop i (i + 1))
  in
  loop (-1) 0;
  Buffer.contents buf

let mapping_of_string str =
  let total_len = String.length str in
  let gen_col = ref 0 in
  let ori_source = ref 0 in
  let ori_line = ref 1 in
  let ori_col = ref 0 in
  let ori_name = ref 0 in
  let rec readline line pos acc =
    if pos >= total_len
    then List.rev acc
    else
      let last = try String.index_from str pos ';' with Not_found -> total_len in
      gen_col := 0;
      let pos, acc = if pos = last then pos + 1, acc else read_tokens line pos last acc in
      readline (succ line) pos acc
  and read_tokens line start stop acc =
    let last = try min (String.index_from str start ',') stop with Not_found -> stop in
    let v = Vlq64.decode_l str ~pos:start ~len:(last - start) in
    match v with
    | [] -> last + 1, acc
    | v ->
        let v =
          match v with
          | [ g ] ->
              gen_col := !gen_col + g;
              Gen { gen_line = line; gen_col = !gen_col }
          | [ g; os; ol; oc ] ->
              gen_col := !gen_col + g;
              ori_source := !ori_source + os;
              ori_line := !ori_line + ol;
              ori_col := !ori_col + oc;
              Gen_Ori
                { gen_line = line
                ; gen_col = !gen_col
                ; ori_source = !ori_source
                ; ori_line = !ori_line
                ; ori_col = !ori_col
                }
          | [ g; os; ol; oc; on ] ->
              gen_col := !gen_col + g;
              ori_source := !ori_source + os;
              ori_line := !ori_line + ol;
              ori_col := !ori_col + oc;
              ori_name := !ori_name + on;
              Gen_Ori_Name
                { gen_line = line
                ; gen_col = !gen_col
                ; ori_source = !ori_source
                ; ori_line = !ori_line
                ; ori_col = !ori_col
                ; ori_name = !ori_name
                }
          | _ -> invalid_arg "Source_map.mapping_of_string"
        in
        let acc = v :: acc in
        if last = stop then last + 1, acc else read_tokens line (last + 1) stop acc
  in
  (* The binary format encodes lines starting at zero, but
     [ori_line] and [gen_line] are 1 based. *)
  readline 1 0 []

let maps ~sources_offset ~names_offset x =
  match x with
  | Gen _ -> x
  | Gen_Ori { gen_line; gen_col; ori_source; ori_line; ori_col } ->
      let ori_source = ori_source + sources_offset in
      Gen_Ori { gen_line; gen_col; ori_source; ori_line; ori_col }
  | Gen_Ori_Name { gen_line; gen_col; ori_source; ori_line; ori_col; ori_name } ->
      let ori_source = ori_source + sources_offset in
      let ori_name = ori_name + names_offset in
      Gen_Ori_Name { gen_line; gen_col; ori_source; ori_line; ori_col; ori_name }

let filter_map sm ~f =
  let a = Array.of_list sm.mappings in
  Array.stable_sort
    ~cmp:(fun t1 t2 ->
      match compare (gen_line t1) (gen_line t2) with
      | 0 -> compare (gen_col t1) (gen_col t2)
      | n -> n)
    a;
  let l = Array.to_list a |> List.group ~f:(fun a b -> gen_line a = gen_line b) in

  let rec loop acc mapping =
    match mapping with
    | [] -> List.rev acc
    | x :: xs ->
        let gen_line = gen_line (List.hd x) in
        let acc =
          match f gen_line with
          | None -> acc
          | Some gen_line ->
              List.rev_append_map
                x
                ~f:(function
                  | Gen { gen_line = _; gen_col } -> Gen { gen_line; gen_col }
                  | Gen_Ori { gen_line = _; gen_col; ori_source; ori_line; ori_col } ->
                      Gen_Ori { gen_line; gen_col; ori_source; ori_line; ori_col }
                  | Gen_Ori_Name
                      { gen_line = _; gen_col; ori_source; ori_line; ori_col; ori_name }
                    ->
                      Gen_Ori_Name
                        { gen_line; gen_col; ori_source; ori_line; ori_col; ori_name })
                acc
        in
        loop acc xs
  in
  let mappings = loop [] l in
  { sm with mappings }

let merge = function
  | [] -> None
  | _ :: _ as l ->
      let rec loop acc_rev ~sources_offset ~names_offset l =
        match l with
        | [] -> acc_rev
        | sm :: rest ->
            let acc_rev =
              { acc_rev with
                sources = List.rev_append sm.sources acc_rev.sources
              ; names = List.rev_append sm.names acc_rev.names
              ; sources_content =
                  (match sm.sources_content, acc_rev.sources_content with
                  | Some x, Some acc_rev -> Some (List.rev_append x acc_rev)
                  | None, _ | _, None -> None)
              ; mappings =
                  List.rev_append_map
                    ~f:(maps ~sources_offset ~names_offset)
                    sm.mappings
                    acc_rev.mappings
              }
            in
            loop
              acc_rev
              ~sources_offset:(sources_offset + List.length sm.sources)
              ~names_offset:(names_offset + List.length sm.names)
              rest
      in
      let acc_rev =
        loop
          { (empty ~filename:"") with sources_content = Some [] }
          ~sources_offset:0
          ~names_offset:0
          l
      in
      Some
        { acc_rev with
          mappings = List.rev acc_rev.mappings
        ; sources = List.rev acc_rev.sources
        ; names = List.rev acc_rev.names
        ; sources_content = Option.map ~f:List.rev acc_rev.sources_content
        }
