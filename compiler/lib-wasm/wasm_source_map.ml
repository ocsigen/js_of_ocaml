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

(* Shift each generated column by the cumulative [resize_data] delta at
   this column, dropping the segments in the [dead_ranges] [\[start, end)]
   (sorted and disjoint), which belong to removed code, as well as the
   segments whose column would become negative. The fields following the
   generated column of a segment (source index, original line and column,
   name index) are encoded as deltas relative to the previous segment
   with these fields: the deltas of a dropped segment are folded into the
   next segment emitted with the corresponding field.

   The segment without origin which terminates a function is located at
   the start of the next function. If it is dropped with the next
   function, the location of the last segment emitted would extend past
   its function: we emit a segment without origin at the start of the
   range instead, unless a segment is kept at the end of the range. *)
let resize_mappings ~dead_ranges (resize_data : resize_data) mappings =
  if String.equal mappings "" || (resize_data.i = 0 && List.is_empty dead_ranges)
  then mappings
  else
    let src = { Vlq64.string = mappings; pos = 0; len = String.length mappings } in
    let buf = Buffer.create (String.length mappings) in
    let col = ref 0 in
    let new_col_acc = ref 0 in
    let idx = ref 0 in
    let shift = ref 0 in
    let pending_source = ref 0 in
    let pending_line = ref 0 in
    let pending_col = ref 0 in
    let pending_name = ref 0 in
    let emitted = ref false in
    let ranges = ref dead_ranges in
    (* When a segment of the current range has been dropped, the output
       column of the start of the range *)
    let range_start = ref None in
    let last_has_origin = ref false in
    let advance col =
      while !idx < resize_data.i && col >= resize_data.pos.(!idx) do
        shift := !shift + resize_data.delta.(!idx);
        incr idx
      done
    in
    let emit_column new_col ~has_origin =
      if !emitted then Buffer.add_char buf ',';
      emitted := true;
      Vlq64.encode buf (new_col - !new_col_acc);
      new_col_acc := new_col;
      last_has_origin := has_origin
    in
    let end_range ~next =
      (match !range_start with
      | Some new_col when !last_has_origin && new_col >= 0 && not next ->
          emit_column new_col ~has_origin:false
      | Some _ | None -> ());
      range_start := None
    in
    let rec leave_ranges col =
      match !ranges with
      | (_, end_) :: rem when col >= end_ ->
          end_range ~next:(col = end_);
          ranges := rem;
          leave_ranges col
      | _ -> ()
    in
    (* Called before advancing to [col] *)
    let in_range col =
      match !ranges with
      | (start, _) :: _ when col >= start ->
          if Option.is_none !range_start
          then (
            advance start;
            range_start := Some (start + !shift));
          true
      | _ -> false
    in
    (* The generated column is already decoded: read the other fields of
       the segment *)
    let rec read_tail acc =
      if src.pos < src.len && Vlq64.in_alphabet src.string.[src.pos]
      then read_tail (Vlq64.decode src :: acc)
      else List.rev acc
    in
    let accumulate tail =
      match tail with
      | source :: line :: column :: rest -> (
          pending_source := !pending_source + source;
          pending_line := !pending_line + line;
          pending_col := !pending_col + column;
          match rest with
          | name :: _ -> pending_name := !pending_name + name
          | [] -> ())
      | _ -> ()
    in
    let emit_tail tail =
      match tail with
      | source :: line :: column :: rest -> (
          Vlq64.encode buf (source + !pending_source);
          Vlq64.encode buf (line + !pending_line);
          Vlq64.encode buf (column + !pending_col);
          pending_source := 0;
          pending_line := 0;
          pending_col := 0;
          match rest with
          | [] -> ()
          | name :: rest ->
              Vlq64.encode buf (name + !pending_name);
              pending_name := 0;
              List.iter ~f:(fun x -> Vlq64.encode buf x) rest)
      | fields -> List.iter ~f:(fun x -> Vlq64.encode buf x) fields
    in
    let rec segment () =
      if src.pos < src.len && Vlq64.in_alphabet src.string.[src.pos]
      then (
        col := !col + Vlq64.decode src;
        let tail = read_tail [] in
        leave_ranges !col;
        let dropped = in_range !col in
        advance !col;
        let new_col = !col + !shift in
        if dropped || new_col < 0
        then accumulate tail
        else (
          emit_column new_col ~has_origin:(not (List.is_empty tail));
          emit_tail tail));
      if src.pos < src.len && Char.equal src.string.[src.pos] ','
      then (
        src.pos <- src.pos + 1;
        segment ())
    in
    segment ();
    end_range ~next:false;
    Buffer.contents buf

let resize ?(dead_ranges = []) resize_data (sm : Source_map.Standard.t) =
  let mappings = Source_map.Mappings.to_string sm.mappings in
  let mappings = resize_mappings ~dead_ranges resize_data mappings in
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
