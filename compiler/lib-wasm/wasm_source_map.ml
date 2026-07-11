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

let is_empty { Source_map.Standard.mappings; _ } = Source_map.Mappings.is_empty mappings

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
