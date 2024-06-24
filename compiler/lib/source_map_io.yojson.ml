(* Js_of_ocaml compiler
 * http://www.ocsigen.org/js_of_ocaml/
 * Copyright (C) 2017 Hugo Heuzard
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

open Source_map

let rewrite_path path =
  if Filename.is_relative path
  then path
  else
    match Build_path_prefix_map.get_build_path_prefix_map () with
    | Some map -> Build_path_prefix_map.rewrite map path
    | None -> path

(* Escapes special characters and wrap in double quotes *)
let stringlit_of_string s = `Stringlit (Yojson.Basic.to_string (`String s))

let json t =
  let mappings =
    `Stringlit ("\"" ^ Mappings.to_string t.mappings ^ "\"")
    (* Nothing to escape *)
  in
  let fields =
    [ "version", `Intlit (Int.to_string t.version)
    ; "file", stringlit_of_string (rewrite_path t.file)
    ; ( "sourceRoot"
      , stringlit_of_string
          (match t.sourceroot with
          | None -> ""
          | Some s -> rewrite_path s) )
    ; "names", `List (List.map (fun s -> stringlit_of_string s) t.names)
    ; ( "sources"
      , `List (List.map (fun s -> stringlit_of_string (rewrite_path s)) t.sources) )
    ; "mappings", mappings
    ]
  in
  match t.sources_contents with
  | None -> `Assoc fields
  | Some cs ->
      `Assoc
        (fields
        @ [ ( "sourcesContent"
            , `List (List.map (fun t -> `Stringlit (Source_text.to_json_string t)) cs) )
          ])

let invalid () = invalid_arg "Source_map.of_json"

let string_of_stringlit (`Stringlit s) =
  match Yojson.Basic.from_string s with
  | `String s -> s
  | _ -> invalid_arg "Source_map_io.string_of_stringlit: not a JSON string literal"

let string name rest =
  match List.assoc name rest with
  | `Stringlit _ as s -> Some (string_of_stringlit s)
  | `Null -> None
  | _ -> invalid ()
  | exception Not_found -> None

let list_string name rest =
  try
    match List.assoc name rest with
    | `List l ->
        Some
          (List.map
             (function
               | `Stringlit _ as lit -> string_of_stringlit lit
               | _ -> invalid ())
             l)
    | _ -> invalid ()
  with Not_found -> None

let stringlit_opt name assoc =
  match List.assoc name assoc with
  | `Stringlit s -> Some s
  | _ | (exception Not_found) -> None

let stringlit_list_opt name assoc =
  match List.assoc name assoc with
  | `List l ->
      Some
        (List.map
           (function
             | `Stringlit lit -> lit
             | _ -> invalid ())
           l)
  | _ -> invalid ()
  | exception Not_found -> None

let standard_map_of_json json =
  match json with
  | `Assoc (("version", version) :: rest) ->
      (match version with
      | `Floatlit version when Float.equal (Float.of_string version) 3.0 -> ()
      | `Intlit version when Int.equal (int_of_string version) 3 -> ()
      | `Floatlit _ | `Intlit _ -> invalid_arg "Source_map_io.of_json: version != 3"
      | _ -> invalid_arg "Source_map_io.of_json: version field is not a number");
      (match List.assoc "sections" rest with
      | _ -> invalid_arg "Source_map_io.standard_map_of_json: not a standard map"
      | exception Not_found -> ());
      let file = string "file" rest in
      let sourceroot = string "sourceRoot" rest in
      let names = list_string "names" rest in
      let sources = list_string "sources" rest in
      let sources_contents = stringlit_list_opt "sourcesContent" rest in
      let mappings = stringlit_opt "mappings" rest in
      let mappings =
        Option.map
          (fun mappings ->
            assert (
              String.length mappings >= 2
              && Char.equal mappings.[0] '"'
              && Char.equal mappings.[String.length mappings - 1] '"');
            Mappings.of_string (String.sub mappings 1 (String.length mappings - 2)))
          mappings
      in
      { version = 3
      ; file = Option.value file ~default:""
      ; sourceroot
      ; names = Option.value names ~default:[]
      ; sources_contents =
          Option.map (List.map Source_text.of_json_string) sources_contents
      ; sources = Option.value sources ~default:[]
      ; mappings = Option.value mappings ~default:Mappings.empty
      }
  | _ -> invalid ()

let to_string m = Yojson.Raw.to_string (json m)

let to_file m file = Yojson.Raw.to_file file (json m)

let enabled = true

module Index = struct
  let json t =
    `Assoc
      [ "version", `Intlit (Int.to_string t.Index.version)
      ; "file", stringlit_of_string (rewrite_path t.file)
      ; ( "sections"
        , `List
            (List.map
               (fun ({ Index.gen_line; gen_column }, `Map sm) ->
                 `Assoc
                   [ ( "offset"
                     , `Assoc
                         [ "line", `Intlit (Int.to_string gen_line)
                         ; "column", `Intlit (Int.to_string gen_column)
                         ] )
                   ; "map", json sm
                   ])
               t.sections) )
      ]

  let intlit ~errmsg name json =
    match List.assoc name json with
    | `Intlit i -> int_of_string i
    | _ -> invalid_arg errmsg
    | exception Not_found -> invalid_arg errmsg

  let section_of_json : Yojson.Raw.t -> Index.offset * [ `Map of t ] = function
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
              Index.{ gen_line; gen_column }
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
          try standard_map_of_json (List.assoc "map" json) with
          | Not_found -> invalid_arg "Source_map_io.Index.of_json: field 'map' absent"
          | Invalid_argument _ ->
              invalid_arg "Source_map_io.Index.of_json: invalid sub-map object"
        in
        offset, `Map map
    | _ -> invalid_arg "Source_map_io.Index.of_json: section of unexpected type"

  let of_json = function
    | `Assoc fields -> (
        let file = string "file" fields in
        match List.assoc "sections" fields with
        | `List sections ->
            let sections = List.map section_of_json sections in
            { Index.version = 3; file = Option.value file ~default:""; sections }
        | _ -> invalid_arg "Source_map_io.Index.of_json: `sections` is not an array"
        | exception Not_found ->
            invalid_arg "Source_map_io.Index.of_json: no `sections` field")
    | _ -> invalid_arg "Source_map_io.of_json: map is not an object"

  let to_string m = Yojson.Raw.to_string (json m)

  let to_file m file = Yojson.Raw.to_file file (json m)
end

let of_json = function
  | `Assoc fields as json -> (
      match List.assoc "sections" fields with
      | _ -> `Index (Index.of_json json)
      | exception Not_found -> `Standard (standard_map_of_json json))
  | _ -> invalid_arg "Source_map_io.of_json: map is not an object"

let of_string s = of_json (Yojson.Raw.from_string s)
