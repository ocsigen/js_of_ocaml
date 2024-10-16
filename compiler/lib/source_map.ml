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

module Source_content = struct
  type t = Sc_as_Stringlit of string

  let create s = Sc_as_Stringlit (Yojson.Safe.to_string (`String s))

  let of_stringlit (`Stringlit s) = Sc_as_Stringlit s

  let to_json (Sc_as_Stringlit s) = `Stringlit s
end

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

let gen_line = function
  | Gen { gen_line; _ } | Gen_Ori { gen_line; _ } | Gen_Ori_Name { gen_line; _ } ->
      gen_line

let gen_col = function
  | Gen { gen_col; _ } | Gen_Ori { gen_col; _ } | Gen_Ori_Name { gen_col; _ } -> gen_col

module Mappings = struct
  type t = Uninterpreted of string [@@unboxed]

  let empty = Uninterpreted ""

  let of_string : string -> t = fun s -> Uninterpreted s

  let to_string : t -> string = fun (Uninterpreted s) -> s

  let encode mapping =
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
    Uninterpreted (Buffer.contents buf)

  let decode (Uninterpreted str) =
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
        let pos, acc =
          if pos = last then pos + 1, acc else read_tokens line pos last acc
        in
        readline (succ line) pos acc
    and read_tokens line start stop acc =
      let last =
        try min (String.index_from str start ',') stop with Not_found -> stop
      in
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
end

let rewrite_path path =
  if Filename.is_relative path
  then path
  else
    match Build_path_prefix_map.get_build_path_prefix_map () with
    | Some map -> Build_path_prefix_map.rewrite map path
    | None -> path

let invalid () = invalid_arg "Source_map.of_json"

let string_of_stringlit (`Stringlit s) =
  match Yojson.Safe.from_string s with
  | `String s -> s
  | _ -> invalid ()

let stringlit name rest : [ `Stringlit of string ] option =
  try
    match List.assoc name rest with
    | `Stringlit _ as s -> Some s
    | `Null -> None
    | _ -> invalid ()
  with Not_found -> None

let list_stringlit name rest =
  try
    match List.assoc name rest with
    | `List l ->
        Some
          (List.map l ~f:(function
              | `Stringlit _ as s -> s
              | _ -> invalid ()))
    | _ -> invalid ()
  with Not_found -> None

let list_stringlit_opt name rest =
  try
    match List.assoc name rest with
    | `List l ->
        Some
          (List.map l ~f:(function
              | `Stringlit _ as s -> Some s
              | `Null -> None
              | _ -> invalid ()))
    | _ -> invalid ()
  with Not_found -> None

module Standard = struct
  type t =
    { version : int
    ; file : string option
    ; sourceroot : string option
    ; sources : string list
    ; sources_content : Source_content.t option list option
    ; names : string list
    ; mappings : Mappings.t
    }

  let empty =
    { version = 3
    ; file = None
    ; sourceroot = None
    ; sources = []
    ; sources_content = None
    ; names = []
    ; mappings = Mappings.empty
    }

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
    let a = Array.of_list (Mappings.decode sm.mappings) in
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
    { sm with mappings = Mappings.encode mappings }

  let merge = function
    | [] -> None
    | _ :: _ as l ->
        let rec loop acc_rev mappings_rev ~sources_offset ~names_offset l =
          match l with
          | [] -> acc_rev, mappings_rev
          | sm :: rest ->
              let acc_rev, mappings_rev =
                ( { acc_rev with
                    sources = List.rev_append sm.sources acc_rev.sources
                  ; names = List.rev_append sm.names acc_rev.names
                  ; sources_content =
                      (match sm.sources_content, acc_rev.sources_content with
                      | Some x, Some acc_rev -> Some (List.rev_append x acc_rev)
                      | None, _ | _, None -> None)
                  ; mappings = Mappings.empty
                  }
                , List.rev_append_map
                    ~f:(maps ~sources_offset ~names_offset)
                    (Mappings.decode sm.mappings)
                    mappings_rev )
              in
              loop
                acc_rev
                mappings_rev
                ~sources_offset:(sources_offset + List.length sm.sources)
                ~names_offset:(names_offset + List.length sm.names)
                rest
        in
        let acc_rev, mappings_rev =
          loop
            { empty with sources_content = Some [] }
            []
            ~sources_offset:0
            ~names_offset:0
            l
        in
        Some
          { acc_rev with
            mappings = Mappings.encode (List.rev mappings_rev)
          ; sources = List.rev acc_rev.sources
          ; names = List.rev acc_rev.names
          ; sources_content = Option.map ~f:List.rev acc_rev.sources_content
          }

  let json t =
    let stringlit s = `Stringlit (Yojson.Safe.to_string (`String s)) in
    `Assoc
      (List.filter_map
         ~f:(fun (name, v) ->
           match v with
           | None -> None
           | Some v -> Some (name, v))
         [ "version", Some (`Intlit (string_of_int t.version))
         ; ( "file"
           , match t.file with
             | None -> None
             | Some file -> Some (stringlit (rewrite_path file)) )
         ; ( "sourceRoot"
           , match t.sourceroot with
             | None -> None
             | Some s -> Some (stringlit (rewrite_path s)) )
         ; "names", Some (`List (List.map t.names ~f:(fun s -> stringlit s)))
         ; ( "sources"
           , Some (`List (List.map t.sources ~f:(fun s -> stringlit (rewrite_path s)))) )
         ; "mappings", Some (stringlit (Mappings.to_string t.mappings))
         ; ( "sourcesContent"
           , match t.sources_content with
             | None -> None
             | Some l ->
                 Some
                   (`List
                     (List.map l ~f:(function
                         | None -> `Null
                         | Some x -> Source_content.to_json x))) )
         ])

  let of_json (json : Yojson.Raw.t) =
    match json with
    | `Assoc (("version", `Intlit version) :: rest) when int_of_string version = 3 ->
        let string name json = Option.map ~f:string_of_stringlit (stringlit name json) in
        let file = string "file" rest in
        let sourceroot = string "sourceRoot" rest in
        let names =
          match list_stringlit "names" rest with
          | None -> []
          | Some l -> List.map ~f:string_of_stringlit l
        in
        let sources =
          match list_stringlit "sources" rest with
          | None -> []
          | Some l -> List.map ~f:string_of_stringlit l
        in
        let sources_content =
          match list_stringlit_opt "sourcesContent" rest with
          | None -> None
          | Some l ->
              Some
                (List.map l ~f:(function
                    | None -> None
                    | Some s -> Some (Source_content.of_stringlit s)))
        in
        let mappings =
          match string "mappings" rest with
          | None -> Mappings.empty
          | Some s -> Mappings.of_string s
        in
        { version = int_of_float (float_of_string version)
        ; file
        ; sourceroot
        ; names
        ; sources_content
        ; sources
        ; mappings
        }
    | _ -> invalid ()

  let to_string m = Yojson.Raw.to_string (json m)

  let to_file m file = Yojson.Raw.to_file file (json m)
end
(* IO *)

module Index = struct
  type offset =
    { gen_line : int
    ; gen_column : int
    }

  type t =
    { version : int
    ; file : string option
    ; sections : (offset * [ `Map of Standard.t ]) list
    }

  let json t =
    let stringlit s = `Stringlit (Yojson.Safe.to_string (`String s)) in
    `Assoc
      (List.filter_map
         ~f:(fun (name, v) ->
           match v with
           | None -> None
           | Some v -> Some (name, v))
         [ "version", Some (`Intlit (string_of_int t.version))
         ; ( "file"
           , match t.file with
             | None -> None
             | Some file -> Some (stringlit (rewrite_path file)) )
         ; ( "sections"
           , Some
               (`List
                 (List.map
                    ~f:(fun ({ gen_line; gen_column }, `Map sm) ->
                      `Assoc
                        [ ( "offset"
                          , `Assoc
                              [ "line", `Intlit (string_of_int gen_line)
                              ; "column", `Intlit (string_of_int gen_column)
                              ] )
                        ; "map", Standard.json sm
                        ])
                    t.sections)) )
         ])

  let intlit ~errmsg name json =
    match List.assoc name json with
    | `Intlit i -> int_of_string i
    | _ -> invalid_arg errmsg
    | exception Not_found -> invalid_arg errmsg

  let section_of_json : Yojson.Raw.t -> offset * [ `Map of Standard.t ] = function
    | `Assoc json ->
        let offset =
          match List.assoc "offset" json with
          | `Assoc fields ->
              let gen_line =
                intlit
                  "line"
                  fields
                  ~errmsg:
                    "Source_map_io.Index.of_json: field 'line' absent or invalid from \
                     section"
              in
              let gen_column =
                intlit
                  "column"
                  fields
                  ~errmsg:
                    "Source_map_io.Index.of_json: field 'column' absent or invalid from \
                     section"
              in
              { gen_line; gen_column }
          | _ ->
              invalid_arg "Source_map_io.Index.of_json: 'offset' field of unexpected type"
        in
        (match List.assoc "url" json with
        | _ ->
            invalid_arg
              "Source_map_io.Index.of_json: URLs in index maps are not currently \
               supported"
        | exception Not_found -> ());
        let map =
          try Standard.of_json (List.assoc "map" json) with
          | Not_found -> invalid_arg "Source_map_io.Index.of_json: field 'map' absent"
          | Invalid_argument _ ->
              invalid_arg "Source_map_io.Index.of_json: invalid sub-map object"
        in
        offset, `Map map
    | _ -> invalid_arg "Source_map_io.Index.of_json: section of unexpected type"

  let of_json = function
    | `Assoc fields -> (
        let string name json = Option.map ~f:string_of_stringlit (stringlit name json) in
        let file = string "file" fields in
        match List.assoc "sections" fields with
        | `List sections ->
            let sections = List.map ~f:section_of_json sections in
            { version = 3; file; sections }
        | _ -> invalid_arg "Source_map_io.Index.of_json: `sections` is not an array"
        | exception Not_found ->
            invalid_arg "Source_map_io.Index.of_json: no `sections` field")
    | _ -> invalid_arg "Source_map_io.of_json: map is not an object"

  let to_string m = Yojson.Raw.to_string (json m)

  let to_file m file = Yojson.Raw.to_file file (json m)
end

type t =
  [ `Standard of Standard.t
  | `Index of Index.t
  ]

let of_json = function
  | `Assoc fields as json -> (
      match List.assoc "sections" fields with
      | _ -> `Index (Index.of_json json)
      | exception Not_found -> `Standard (Standard.of_json json))
  | _ -> invalid_arg "Source_map_io.of_json: map is not an object"

let of_string s = of_json (Yojson.Raw.from_string s)

let of_file f = of_json (Yojson.Raw.from_file f)

let to_string = function
  | `Standard m -> Standard.to_string m
  | `Index i -> Index.to_string i

let to_file x f =
  match x with
  | `Standard m -> Standard.to_file m f
  | `Index i -> Index.to_file i f
