(* Wasm_of_ocaml compiler
 * http://www.ocsigen.org/js_of_ocaml/
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

open Stdlib

type resize_data =
  { mutable i : int
  ; mutable pos : int array
  ; mutable delta : int array
  }

type t = Yojson.Raw.t

type input = Vlq64.input =
  { string : string
  ; mutable pos : int
  ; len : int
  }

let rec next' src mappings pos len =
  pos < len
  &&
  match mappings.[pos] with
  | ',' ->
      src.pos <- pos + 1;
      true
  | _ -> next' src mappings (pos + 1) len

let next src = next' src src.string src.pos src.len

let flush buf src start pos =
  if start < pos then Buffer.add_substring buf src.string start (pos - start)

let rec resize_rec buf start src resize_data i col0 delta0 col =
  let pos = src.pos in
  let delta = Vlq64.decode src in
  let col = col + delta in
  if col < col0
  then
    if next src
    then resize_rec buf start src resize_data i col0 delta0 col
    else flush buf src start src.len
  else
    let delta = delta + delta0 in
    adjust buf start src resize_data i col delta pos

and adjust buf start src (resize_data : resize_data) i col delta pos =
  assert (delta > 0);
  if i < resize_data.i
  then
    let col0 = resize_data.pos.(i) in
    let delta0 = resize_data.delta.(i) in
    if col < col0
    then (
      flush buf src start pos;
      Vlq64.encode buf delta;
      let start = src.pos in
      if next src
      then resize_rec buf start src resize_data (i + 1) col0 delta0 col
      else flush buf src start src.len)
    else
      let delta = delta + delta0 in
      adjust buf start src resize_data (i + 1) col delta pos
  else (
    flush buf src start pos;
    Vlq64.encode buf delta;
    let start = src.pos in
    flush buf src start src.len)

let resize_mappings (resize_data : resize_data) mappings =
  if String.equal mappings "" || resize_data.i = 0
  then mappings
  else
    let col0 = resize_data.pos.(0) in
    let delta0 = resize_data.delta.(0) in
    let buf = Buffer.create (String.length mappings) in
    resize_rec
      buf
      0
      { Vlq64.string = mappings; pos = 0; len = String.length mappings }
      resize_data
      1
      col0
      delta0
      0;
    Buffer.contents buf

let resize resize_data (sm : Source_map.Standard.t) =
  let mappings = Source_map.Mappings.to_string sm.mappings in
  let mappings = resize_mappings resize_data mappings in
  { sm with mappings = Source_map.Mappings.of_string_unsafe mappings }

let is_empty { Source_map.Standard.mappings; _ } = Source_map.Mappings.is_empty mappings

let concatenate l =
  Source_map.Index
    { version = 3
    ; file = None
    ; sections =
        List.map
          ~f:(fun (ofs, map) ->
            { Source_map.Index.offset = { gen_line = 0; gen_column = ofs }; map })
          l
    }

let iter_sources' (sm : Source_map.Standard.t) i f =
  let l = sm.sources in
  let single = List.length l = 1 in
  List.iteri ~f:(fun j nm -> f i (if single then None else Some j) nm) l

let iter_sources sm f =
  match sm with
  | Source_map.Standard sm -> iter_sources' sm None f
  | Index { sections; _ } ->
      let single_map = List.length sections = 1 in
      List.iteri
        ~f:(fun i entry ->
          iter_sources' entry.Source_map.Index.map (if single_map then None else Some i) f)
        sections

let blackbox_filename = "/builtin/blackbox.ml"

let blackbox_contents = "(* generated code *)"

let insert_source_contents' (sm : Source_map.Standard.t) i f =
  let l = sm.sources in
  let single = List.length l = 1 in
  let contents =
    List.mapi
      ~f:(fun j name ->
        if String.equal name blackbox_filename
        then Some (Source_map.Source_content.create blackbox_contents)
        else
          match f i (if single then None else Some j) name with
          | Some c -> Some (Source_map.Source_content.of_stringlit (`Stringlit c))
          | None -> None)
      l
  in
  let sm = { sm with sources_content = Some contents } in
  let sm =
    if List.mem ~eq:String.equal blackbox_filename sm.sources
    then { sm with ignore_list = [ blackbox_filename ] }
    else sm
  in
  sm

let insert_source_contents sm f =
  match sm with
  | Source_map.Standard sm -> Source_map.Standard (insert_source_contents' sm None f)
  | Index ({ sections; _ } as sm) ->
      let single_map = List.length sections = 1 in
      let sections =
        List.mapi
          ~f:(fun i entry ->
            { entry with
              Source_map.Index.map =
                insert_source_contents'
                  entry.Source_map.Index.map
                  (if single_map then None else Some i)
                  f
            })
          sections
      in
      Index { sm with sections }
