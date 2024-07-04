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

module Line_edits = struct
  type action =
    | Keep
    | Drop
    | Add of { count : int }

  let pp_action fmt =
    let open Format in
    function
    | Keep -> pp_print_string fmt "Keep"
    | Drop -> pp_print_string fmt "Drop"
    | Add { count } -> fprintf fmt "@[<hov>Add@ {@ count =@ %d@ }@]" count

  type t = action list

  let pp fmt = Format.(pp_print_list pp_action fmt)
end

module Mappings = struct
  type t = Uninterpreted of string [@@unboxed]

  let empty = Uninterpreted ""

  let of_string : string -> t = fun s -> Uninterpreted s

  let to_string : t -> string = fun (Uninterpreted s) -> s

  type carries =
    { carry_source : int
    ; carry_line : int
    ; carry_col : int
    ; carry_name : int
    }

  let update_carries_from_segment
      ~carry_source
      ~carry_line
      ~carry_col
      ~carry_name
      ~pos
      ~len
      str =
    (* Note: we don't care about the first field since we do linewise editing,
       and it is reset for every line. *)
    match Vlq64.decode_l str ~pos ~len with
    | [ _gen_col ] -> { carry_source; carry_line; carry_col; carry_name }
    | [ _gen_col; source; line; col ] ->
        { carry_source = carry_source + source
        ; carry_line = carry_line + line
        ; carry_col = carry_col + col
        ; carry_name
        }
    | [ _gen_col; source; line; col; name ] ->
        { carry_source = carry_source + source
        ; carry_line = carry_line + line
        ; carry_col = carry_col + col
        ; carry_name = carry_name + name
        }
    | _ -> invalid_arg "Mapping.update_carries_from_segment: invalid segment"

  let update_carries_and_write_segment
      ~carry_source
      ~carry_line
      ~carry_col
      ~carry_name
      ~pos
      ~len
      str
      ~buf =
    match Vlq64.decode_l str ~pos ~len with
    | [ gen_col ] ->
        Vlq64.encode_l buf [ gen_col ];
        carry_source, carry_line, carry_col, carry_name
    | [ gen_col; source; line; col ] ->
        Vlq64.encode_l
          buf
          [ gen_col; source + carry_source; line + carry_line; col + carry_col ];
        0, 0, 0, carry_name
    | [ gen_col; source; line; col; name ] ->
        Vlq64.encode_l
          buf
          [ gen_col
          ; source + carry_source
          ; line + carry_line
          ; col + carry_col
          ; name + carry_name
          ];
        0, 0, 0, 0
    | _ ->
        invalid_arg
          (Format.sprintf
             "Mapping.update_carries_from_segment %s"
             (String.sub ~pos ~len str))

  (* Fold [f] over the segments in string [str.(pos..len - 1)]. *)
  let fold_on_segments ~str ~pos ~len f ~init =
    let rec loop acc pos end_ =
      if pos >= end_
      then acc
      else
        let next_delim =
          try min (String.index_from str pos ',') end_ with Not_found -> end_
        in
        let len = next_delim - pos in
        if len <= 0 then acc else loop (f acc str ~pos ~len) (next_delim + 1) end_
    in
    loop init pos (pos + len)

  let update_carries_from_line
      ~carry_source
      ~carry_line
      ~carry_col
      ~carry_name
      ~pos
      ~len
      str =
    fold_on_segments
      ~str
      ~pos
      ~len
      ~init:{ carry_source; carry_line; carry_col; carry_name }
      (fun acc str ~pos ~len ->
        let { carry_source; carry_line; carry_col; carry_name } = acc in
        update_carries_from_segment
          ~carry_source
          ~carry_line
          ~carry_col
          ~carry_name
          ~pos
          ~len
          str)

  let update_carries_and_write_line
      ~carry_source
      ~carry_line
      ~carry_col
      ~carry_name
      ~pos
      ~len
      str
      ~buf =
    let _, carries =
      fold_on_segments
        ~str
        ~pos
        ~len
        ~init:(true, { carry_source; carry_line; carry_col; carry_name })
        (fun (is_first, acc) str ~pos ~len ->
          let { carry_source; carry_line; carry_col; carry_name } = acc in
          if not is_first then Buffer.add_char buf ',';
          let carry_source, carry_line, carry_col, carry_name =
            update_carries_and_write_segment
              ~carry_source
              ~carry_line
              ~carry_col
              ~carry_name
              ~pos
              ~len
              str
              ~buf
          in
          false, { carry_source; carry_line; carry_col; carry_name })
    in
    Buffer.add_char buf ';';
    carries

  (* If [strict], then the number of [Keep] and [Drop] elements in [edits]
     should be the same as the number of generated lines covered by the
     mappings [orig]. Otherwise, there may be more edit actions, in which case
     [Keep] just adds a line without mappings and [Drop] does nothing. *)
  let rec edit_loop
      ~orig
      ~carry_source
      ~carry_line
      ~carry_col
      ~carry_name
      ~strict
      buf
      offset_in_orig
      edits =
    let open Line_edits in
    if offset_in_orig >= String.length orig
    then (
      (* No more lines in the mappings string *)
      match edits with
      | [] -> { carry_source; carry_line; carry_col; carry_name }
      | _ :: _ ->
          List.iter edits ~f:(function
              | Add { count } -> Buffer.add_string buf (String.make count ';')
              | Keep ->
                  if strict
                  then
                    invalid_arg
                      "Mapping.edit: more Keep or Drop edits than lines in mappings";
                  Buffer.add_char buf ';'
              | Drop ->
                  if strict
                  then
                    invalid_arg
                      "Mapping.edit: more Keep or Drop edits than lines in mappings");
          { carry_source; carry_line; carry_col; carry_name })
    else
      match edits with
      | [] -> { carry_source; carry_line; carry_col; carry_name }
      | Keep :: rem ->
          let next_group_delim =
            try String.index_from orig offset_in_orig ';'
            with Not_found -> String.length orig
          in
          let { carry_source; carry_line; carry_col; carry_name } =
            update_carries_and_write_line
              ~carry_source
              ~carry_line
              ~carry_col
              ~carry_name
              ~pos:offset_in_orig
              ~len:(next_group_delim - offset_in_orig)
              orig
              ~buf
          in
          edit_loop
            ~orig
            ~carry_source
            ~carry_line
            ~carry_col
            ~carry_name
            ~strict
            buf
            (* Skip the ';' *)
            (next_group_delim + 1)
            rem
      | Drop :: rem ->
          let next_group_delim =
            try String.index_from orig offset_in_orig ';'
            with Not_found -> String.length orig
          in
          let { carry_source; carry_line; carry_col; carry_name } =
            update_carries_from_line
              ~carry_source
              ~carry_line
              ~carry_col
              ~carry_name
              ~pos:offset_in_orig
              ~len:(next_group_delim - offset_in_orig)
              orig
          in
          edit_loop
            ~orig
            ~carry_source
            ~carry_line
            ~carry_col
            ~carry_name
            ~strict
            buf
            (next_group_delim + 1)
            rem
      | Add { count } :: rem ->
          Buffer.add_string buf (String.make count ';');
          edit_loop
            ~orig
            ~carry_source
            ~carry_line
            ~carry_col
            ~carry_name
            ~strict
            buf
            offset_in_orig
            rem

  let edit ~strict (Uninterpreted orig) edits =
    let buf = Buffer.create 8_192 in
    let _ =
      edit_loop
        ~strict
        ~orig
        ~carry_source:0
        ~carry_line:0
        ~carry_col:0
        ~carry_name:0
        buf
        0
        edits
    in
    Uninterpreted (Buffer.contents buf)

  let num_gen_lines m =
    let rec loop count pos =
      if pos >= String.length m
      then count
      else
        let next_delim =
          try String.index_from m pos ';' with Not_found -> String.length m
        in
        if next_delim >= String.length m - 1
        then (* This was the last line *)
          count + 1
        else loop (count + 1) (next_delim + 1)
    in
    loop 0 0

  (* Fold [f] over the ';'-separated groups in string [str.(pos..len - 1)]. *)
  let fold_on_lines ~str f ~init =
    let rec loop acc pos =
      if pos >= String.length str
      then acc
      else
        let next_delim =
          try min (String.index_from str pos ';') (String.length str)
          with Not_found -> String.length str
        in
        let len = next_delim - pos in
        loop (f acc str ~pos ~len) (next_delim + 1)
    in
    loop init 0

  let sum_offsets mapping =
    fold_on_lines
      ~str:mapping
      ~init:{ carry_source = 0; carry_line = 0; carry_col = 0; carry_name = 0 }
      (fun { carry_source; carry_line; carry_col; carry_name } str ~pos ~len ->
        update_carries_from_line
          ~carry_source
          ~carry_line
          ~carry_col
          ~carry_name
          ~pos
          ~len
          str)

  let concat ~source_count1 ~name_count1 (Uninterpreted m1) (Uninterpreted m2) =
    match m1, m2 with
    | "", m2 -> Uninterpreted m2
    | m1, "" -> Uninterpreted m1
    | _, _ ->
        let buf = Buffer.create 8_192 in
        (* First do a pass on [m1] to accumulate its carries. *)
        let { carry_source; carry_line; carry_col; carry_name } = sum_offsets m1 in
        Buffer.add_string buf m1;
        if not (Char.equal m1.[String.length m1 - 1] ';') then Buffer.add_char buf ';';
        let _ =
          edit_loop
            ~orig:m2
              (* Make the initial absolute offsets in [m2] relative. Also account
                 for the fact that fields [sources] and [names] of [m2] will be
                 concatenated to those of [m1]. *)
            ~carry_source:(source_count1 - carry_source)
            ~carry_line:~-carry_line
            ~carry_col:~-carry_col
            ~carry_name:(name_count1 - carry_name)
            ~strict:true
            buf
            0
            (List.init ~len:(num_gen_lines m2) ~f:(Fun.const Line_edits.Keep))
        in
        Uninterpreted (Buffer.contents buf)

  let gen_line = function
    | Gen { gen_line; _ } | Gen_Ori { gen_line; _ } | Gen_Ori_Name { gen_line; _ } ->
        gen_line

  let gen_col = function
    | Gen { gen_col; _ } | Gen_Ori { gen_col; _ } | Gen_Ori_Name { gen_col; _ } -> gen_col

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

module Source_text = struct
  type t = Uninterpreted of string [@@unboxed]

  let of_json_string : string -> t = fun s -> Uninterpreted s

  let to_json_string : t -> string = fun (Uninterpreted s) -> s

  let empty = Uninterpreted ""

  let to_json = function
    | None -> `Null
    | Some text -> `String text

  let encode t =
    let json = Yojson.Basic.to_string (to_json t) in
    Uninterpreted json

  let of_json = function
    | `String s -> Some s
    | `Null -> None
    | _ -> invalid_arg "Source_map.Sources_contents.of_json: expected string or null"

  let decode (Uninterpreted s) : string option =
    (* The two stages of the encoding, in reverse. *)
    try of_json (Yojson.Basic.from_string s)
    with Yojson.Json_error s ->
      invalid_arg
        ("Source_map.Sources_contents.decode: This is not a valid JSON object: " ^ s)
end

type t =
  { version : int
  ; file : string
  ; sourceroot : string option
  ; sources : string list
  ; sources_contents : Source_text.t list option
  ; names : string list
  ; mappings : Mappings.t
  }

let empty ~filename =
  { version = 3
  ; file = filename
  ; sourceroot = None
  ; sources = []
  ; sources_contents = None
  ; names = []
  ; mappings = Mappings.empty
  }

let concat ~file ~sourceroot s1 s2 =
  if not (Int.equal s1.version s2.version)
  then invalid_arg "Source_map.concat: different versions";
  { version = s1.version
  ; file
  ; sourceroot
  ; sources = s1.sources @ s2.sources
  ; sources_contents =
      (match s1.sources_contents, s2.sources_contents with
      | None, contents | contents, None -> contents
      | Some c1, Some c2 -> Some (c1 @ c2))
  ; names = s1.names @ s2.names
  ; mappings =
      Mappings.concat
        ~source_count1:(List.length s1.sources)
        ~name_count1:(List.length s1.names)
        s1.mappings
        s2.mappings
  }

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
    [ "version", `Intlit (string_of_int t.version)
    ; "file", stringlit_of_string (rewrite_path t.file)
    ; ( "sourceRoot"
      , stringlit_of_string
          (match t.sourceroot with
          | None -> ""
          | Some s -> rewrite_path s) )
    ; "names", `List (List.map ~f:(fun s -> stringlit_of_string s) t.names)
    ; ( "sources"
      , `List (List.map ~f:(fun s -> stringlit_of_string (rewrite_path s)) t.sources) )
    ; "mappings", mappings
    ]
  in
  match t.sources_contents with
  | None -> `Assoc fields
  | Some cs ->
      `Assoc
        (fields
        @ [ ( "sourcesContent"
            , `List (List.map ~f:(fun t -> `Stringlit (Source_text.to_json_string t)) cs)
            )
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
             ~f:(function
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
           ~f:(function
             | `Stringlit lit -> lit
             | _ -> invalid ())
           l)
  | _ -> invalid ()
  | exception Not_found -> None

let standard_map_of_json json =
  match json with
  | `Assoc (("version", version) :: rest) ->
      (match version with
      | `Floatlit version when Float.equal (float_of_string version) 3.0 -> ()
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
          ~f:(fun mappings ->
            assert (
              String.length mappings >= 2
              && Char.equal mappings.[0] '"'
              && Char.equal mappings.[String.length mappings - 1] '"');
            Mappings.of_string
              (String.sub mappings ~pos:1 ~len:(String.length mappings - 2)))
          mappings
      in
      { version = 3
      ; file = Option.value file ~default:""
      ; sourceroot
      ; names = Option.value names ~default:[]
      ; sources_contents =
          Option.map ~f:(List.map ~f:Source_text.of_json_string) sources_contents
      ; sources = Option.value sources ~default:[]
      ; mappings = Option.value mappings ~default:Mappings.empty
      }
  | _ -> invalid ()

let to_string m = Yojson.Raw.to_string (json m)

let to_file m file = Yojson.Raw.to_file file (json m)

module Index = struct
  type offset =
    { gen_line : int
    ; gen_column : int
    }

  (* Type synonym to avoid confusion between toplevel [t] and this submodule's [t]. *)
  type map = t

  type t =
    { version : int
    ; file : string
    ; sections : (offset * [ `Map of map ]) list
    }

  let json t =
    `Assoc
      [ "version", `Intlit (string_of_int t.version)
      ; "file", stringlit_of_string (rewrite_path t.file)
      ; ( "sections"
        , `List
            (List.map
               ~f:(fun ({ gen_line; gen_column }, `Map sm) ->
                 `Assoc
                   [ ( "offset"
                     , `Assoc
                         [ "line", `Intlit (string_of_int gen_line)
                         ; "column", `Intlit (string_of_int gen_column)
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

  let section_of_json : Yojson.Raw.t -> offset * [ `Map of map ] = function
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
            let sections = List.map ~f:section_of_json sections in
            { version = 3; file = Option.value file ~default:""; sections }
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
