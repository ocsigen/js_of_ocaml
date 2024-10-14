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
open! Stdlib

let times = Debug.find "times"

let debug = Debug.find "link"

let sourceMappingURL = "//# sourceMappingURL="

let sourceMappingURL_base64 = "//# sourceMappingURL=data:application/json;base64,"

module Line_reader : sig
  type t

  val open_ : string -> t

  val next : t -> string

  val peek : t -> string option

  val drop : t -> drop_action:(unit -> unit) -> unit
  (** [drop_action] is the function to call if a line was effectively dropped
      (if EOF is reached, this function may return without dropping a line). *)

  val close : t -> unit

  val lnum : t -> int

  val fname : t -> string
end = struct
  type t =
    { ic : in_channel
    ; fname : string
    ; mutable next : string option
    ; mutable lnum : int
    }

  let close t = close_in t.ic

  let open_ fname =
    let ic = open_in_bin fname in
    { ic; lnum = 0; fname; next = None }

  let next t =
    let lnum = t.lnum in
    let s =
      match t.next with
      | None -> input_line t.ic
      | Some s ->
          t.next <- None;
          s
    in
    t.lnum <- lnum + 1;
    s

  let peek t =
    match t.next with
    | Some x -> Some x
    | None -> (
        try
          let s = input_line t.ic in
          t.next <- Some s;
          Some s
        with End_of_file -> None)

  let drop t ~drop_action =
    match t.next with
    | Some _ ->
        t.next <- None;
        t.lnum <- t.lnum + 1;
        drop_action ()
    | None -> (
        try
          let (_ : string) = input_line t.ic in
          t.lnum <- t.lnum + 1;
          drop_action ()
        with End_of_file -> ())

  let lnum t = t.lnum

  let fname t = t.fname
end

module Line_writer : sig
  type t

  val of_channel : out_channel -> t

  val write : ?source:Line_reader.t -> t -> add:(int -> unit) -> string -> unit
  (** [write ~source t s ~add] writes [s], followed by a newline, and calls
      [edit], giving it in argument the number of "line number" pragma lines
      emitted before writing [s]. *)

  val write_lines : ?source:Line_reader.t -> t -> total:(int -> unit) -> string -> unit
  (** [write_lines ~source t s ~total] writes all lines in [s], ensures that the last
      line ends with a newline, and calls [total], giving it in argument the total number
      of lines written, including "line number" pragma lines. *)

  val lnum : t -> int
end = struct
  type t =
    { oc : out_channel
    ; mutable lnum : int
    ; mutable source : (string * int) option
    }

  let of_channel oc = { oc; source = None; lnum = 0 }

  let write ?source t ~add s =
    let source =
      match source with
      | None -> None
      | Some ic -> Some (Line_reader.fname ic, Line_reader.lnum ic)
    in
    let emit fname lnum =
      output_string t.oc (Printf.sprintf "//# %d %S\n" lnum fname);
      1
    in
    let lnum_off =
      match t.source, source with
      | _, None -> 0
      | None, Some (fname, lnum) -> emit fname lnum
      | Some (fname1, lnum1), Some (fname2, lnum2) ->
          if String.equal fname1 fname2 && lnum1 + 1 = lnum2 then 0 else emit fname2 lnum2
    in
    add lnum_off;
    output_string t.oc s;
    output_string t.oc "\n";
    let lnum_off = lnum_off + 1 in
    t.source <- source;
    t.lnum <- t.lnum + lnum_off

  let write_lines ?source t ~total lines =
    let l = String.split_on_char ~sep:'\n' lines in
    let lcount = ref 0 in
    let rec w = function
      | [ "" ] | [] -> ()
      | s :: xs ->
          let () = write ?source t s ~add:(fun n -> lcount := !lcount + n + 1) in
          w xs
    in
    w l;
    total !lcount

  let lnum t = t.lnum
end

type action =
  | Keep
  | Drop
  | Unit
  | Build_info of Build_info.t
  | Source_map of Source_map.t

let prefix_kind line =
  match String.is_prefix ~prefix:sourceMappingURL line with
  | false -> (
      match Build_info.parse line with
      | Some bi -> `Build_info bi
      | None -> (
          match Unit_info.parse Unit_info.empty line with
          | Some _ -> `Unit
          | None -> `Other))
  | true -> (
      match String.is_prefix ~prefix:sourceMappingURL_base64 line with
      | true -> `Json_base64 (String.length sourceMappingURL_base64)
      | false -> `Url (String.length sourceMappingURL))

let rule_out_index_map = function
  | `Standard sm -> sm
  | `Index _ -> failwith "unexpected index map at this stage"

let action ~resolve_sourcemap_url ~drop_source_map file line =
  match prefix_kind line, drop_source_map with
  | `Other, (true | false) -> Keep
  | `Unit, (true | false) -> Unit
  | `Build_info bi, _ -> Build_info bi
  | (`Json_base64 _ | `Url _), true -> Drop
  | `Json_base64 offset, false ->
      Source_map
        (rule_out_index_map (Source_map.of_string (Base64.decode_exn ~off:offset line)))
  | `Url _, false when not resolve_sourcemap_url -> Drop
  | `Url offset, false ->
      let url = String.sub line ~pos:offset ~len:(String.length line - offset) in
      let base = Filename.dirname file in
      let ic = open_in_bin (Filename.concat base url) in
      let l = in_channel_length ic in
      let content = really_input_string ic l in
      close_in ic;
      Source_map (rule_out_index_map (Source_map.of_string content))

module Units : sig
  val read : Line_reader.t -> drop_action:(unit -> unit) -> Unit_info.t -> Unit_info.t

  val scan_file : string -> Build_info.t option * Unit_info.t list
end = struct
  let rec read ic ~drop_action uinfo =
    match Line_reader.peek ic with
    | None -> uinfo
    | Some line -> (
        match Unit_info.parse uinfo line with
        | None -> uinfo
        | Some uinfo ->
            Line_reader.drop ~drop_action ic;
            read ic ~drop_action uinfo)

  let find_unit_info ~drop_action ic =
    let rec find_next ic =
      match Line_reader.peek ic with
      | None -> None
      | Some line -> (
          match prefix_kind line with
          | `Json_base64 _ | `Url _ | `Other | `Build_info _ ->
              Line_reader.drop ~drop_action ic;
              find_next ic
          | `Unit -> Some (read ic ~drop_action Unit_info.empty))
    in
    find_next ic

  let find_build_info ~drop_action ic =
    let rec find_next ic =
      match Line_reader.peek ic with
      | None -> None
      | Some line -> (
          match prefix_kind line with
          | `Json_base64 _ | `Url _ | `Other ->
              Line_reader.drop ~drop_action ic;
              find_next ic
          | `Build_info bi -> Some bi
          | `Unit -> None)
    in
    find_next ic

  let scan_file file =
    let ic = Line_reader.open_ file in
    let drop_action () = () in
    let rec scan_all ic acc =
      match find_unit_info ~drop_action ic with
      | None -> List.rev acc
      | Some x -> scan_all ic (x :: acc)
    in
    let build_info = find_build_info ~drop_action ic in
    let units = scan_all ic [] in
    Line_reader.close ic;
    build_info, units
end

let link ~output ~linkall ~mklib ~toplevel ~files ~resolve_sourcemap_url ~source_map =
  (* we currently don't do anything with [toplevel]. It could be used
     to conditionally include link_info ?*)
  ignore (toplevel : bool);
  let t = Timer.make () in
  let oc = Line_writer.of_channel output in
  let warn_effects = ref false in
  let files = List.map files ~f:(fun file -> file, Units.scan_file file) in
  let missing, to_link, all =
    List.fold_right
      files
      ~init:(StringSet.empty, StringSet.empty, StringSet.empty)
      ~f:(fun (_file, (build_info, units)) acc ->
        let cmo_file =
          match build_info with
          | Some bi -> (
              match Build_info.kind bi with
              | `Cmo -> true
              | `Cma | `Exe | `Runtime | `Unknown -> false)
          | None -> false
        in
        List.fold_right
          units
          ~init:acc
          ~f:(fun (info : Unit_info.t) (requires, to_link, all) ->
            let all = StringSet.union all info.provides in
            if (not (Config.Flag.auto_link ()))
               || mklib
               || cmo_file
               || linkall
               || info.force_link
               || not (StringSet.is_empty (StringSet.inter requires info.provides))
            then
              ( StringSet.diff (StringSet.union info.requires requires) info.provides
              , StringSet.union to_link info.provides
              , all )
            else requires, to_link, all))
  in
  let _skip = StringSet.diff all to_link in
  if (not (StringSet.is_empty missing)) && not mklib
  then
    failwith
      (Printf.sprintf
         "Could not find compilation unit for %s"
         (String.concat ~sep:", " (StringSet.elements missing)));
  if times () then Format.eprintf "  scan: %a@." Timer.print t;
  let sm = ref [] in
  let build_info = ref None in
  let t = Timer.make () in
  let sym = ref Ocaml_compiler.Symtable.GlobalMap.empty in
  let sym_js = ref [] in
  List.iter files ~f:(fun (_, (_, units)) ->
      List.iter units ~f:(fun (u : Unit_info.t) ->
          StringSet.iter
            (fun s ->
              ignore
                (Ocaml_compiler.Symtable.GlobalMap.enter
                   sym
                   (Ocaml_compiler.Symtable.Global.Glob_compunit s)
                  : int);
              sym_js := s :: !sym_js)
            u.Unit_info.provides));

  let build_info_emitted = ref false in
  List.iter files ~f:(fun (file, (build_info_for_file, units)) ->
      let is_runtime =
        match build_info_for_file with
        | Some bi -> (
            match Build_info.kind bi with
            | `Runtime -> Some bi
            | `Cma | `Exe | `Cmo | `Unknown -> None)
        | None -> None
      in
      let sm_for_file = ref None in
      let ic = Line_reader.open_ file in
      let old_line_count = Line_writer.lnum oc in
      let edits = ref [] in
      let emit_drop_action edits () = edits := Source_map.Line_edits.Drop :: !edits in
      let skip ic = Line_reader.drop ~drop_action:(emit_drop_action edits) ic in
      let copy ic oc =
        let line = Line_reader.next ic in
        Line_writer.write
          ~source:ic
          ~add:(fun count -> edits := Add { count } :: !edits)
          oc
          line;
        (* Note: line actions are in reverse order compared to the actual generated
           lines *)
        edits := Source_map.Line_edits.Keep :: !edits
      in
      let write_line oc str =
        Line_writer.write oc str ~add:(fun count ->
            edits := Source_map.Line_edits.(Add { count = count + 1 }) :: !edits)
      in
      let write_lines oc str =
        Line_writer.write_lines oc str ~total:(fun count ->
            edits := Source_map.Line_edits.(Add { count }) :: !edits)
      in
      let rec read () =
        match Line_reader.peek ic with
        | None -> ()
        | Some line ->
            (match
               action
                 ~resolve_sourcemap_url
                 ~drop_source_map:Poly.(source_map = None)
                 file
                 line
             with
            | Keep -> copy ic oc
            | Build_info bi ->
                skip ic;
                if not !build_info_emitted
                then (
                  let bi = Build_info.with_kind bi (if mklib then `Cma else `Unknown) in
                  write_lines oc (Build_info.to_string bi);
                  build_info_emitted := true)
            | Drop -> skip ic
            | Unit ->
                let u =
                  Units.read ic ~drop_action:(emit_drop_action edits) Unit_info.empty
                in
                if StringSet.cardinal (StringSet.inter u.Unit_info.provides to_link) > 0
                then (
                  if u.effects_without_cps && not !warn_effects
                  then (
                    warn_effects := true;
                    warn
                      "Warning: your program contains effect handlers; you should \
                       probably run js_of_ocaml with option '--enable=effects'@.");
                  (if mklib
                   then
                     let u = if linkall then { u with force_link = true } else u in
                     write_lines oc (Unit_info.to_string u));
                  let size = ref 0 in
                  while
                    match Line_reader.peek ic with
                    | None -> false
                    | Some line -> (
                        match prefix_kind line with
                        | `Other ->
                            size := !size + String.length line + 1;
                            true
                        | `Json_base64 _ | `Url _ | `Build_info _ | `Unit -> false)
                  do
                    copy ic oc
                  done;
                  if debug ()
                  then
                    Format.eprintf
                      "Copy %d bytes for %s@."
                      !size
                      (match is_runtime with
                      | None -> String.concat ~sep:", " (StringSet.elements u.provides)
                      | Some _ -> "the js runtime"))
                else (
                  if debug ()
                  then
                    Format.eprintf
                      "Skip %s@."
                      (String.concat ~sep:"," (StringSet.elements u.provides));
                  while
                    match Line_reader.peek ic with
                    | None -> false
                    | Some line -> (
                        match prefix_kind line with
                        | `Other -> true
                        | `Json_base64 _ | `Url _ | `Build_info _ | `Unit -> false)
                  do
                    skip ic
                  done)
            | Source_map x ->
                skip ic;
                sm_for_file := Some x);
            read ()
      in
      read ();
      write_line oc "";
      Line_reader.close ic;
      (match is_runtime with
      | None -> ()
      | Some bi ->
          Build_info.configure bi;
          let primitives =
            List.fold_left units ~init:StringSet.empty ~f:(fun acc (u : Unit_info.t) ->
                StringSet.union acc (StringSet.of_list u.primitives))
          in
          let code = Parse_bytecode.link_info ~symbols:!sym ~primitives ~crcs:[] in
          let b = Buffer.create 100 in
          let fmt = Pretty_print.to_buffer b in
          Driver.configure fmt;
          Driver.f'
            ~standalone:false
            ~link:`No
            ~wrap_with_fun:`Iife
            fmt
            (Parse_bytecode.Debug.create ~include_cmis:false false)
            code;
          let content = Buffer.contents b in
          write_lines oc content);
      (match !sm_for_file with
      | None -> ()
      | Some x ->
          sm := (file, x, List.rev !edits, Line_writer.lnum oc - old_line_count) :: !sm);
      match !build_info, build_info_for_file with
      | None, None -> ()
      | Some _, None -> ()
      | None, Some build_info_for_file -> build_info := Some (file, build_info_for_file)
      | Some (first_file, bi), Some build_info_for_file ->
          build_info :=
            Some (first_file, Build_info.merge first_file bi file build_info_for_file));
  if times () then Format.eprintf "  emit: %a@." Timer.print t;
  let t = Timer.make () in
  match source_map with
  | None -> ()
  | Some (file, init_sm) ->
      let sourcemaps_and_line_counts =
        List.rev_map !sm ~f:(fun (file, sm, edits, lcount) ->
            if debug ()
            then (
              Format.eprintf "@[<v>line actions for '%s' (lcount %d)@," file lcount;
              Format.eprintf "%a@," Source_map.Line_edits.pp edits;
              Format.eprintf "@]");
            let mappings = sm.Source_map.mappings in
            let mappings = Source_map.Mappings.edit ~strict:false mappings edits in
            { sm with mappings }, lcount)
      in
      let merged_sourcemap =
        let open Source_map in
        assert (String.equal (Mappings.to_string init_sm.mappings) "");
        { version = init_sm.version
        ; file = init_sm.file
        ; Index.sections =
            (let _, sections =
               List.fold_left
                 sourcemaps_and_line_counts
                 ~f:(fun (cur_ofs, sections) (sm, generated_line_count) ->
                   let offset = Index.{ gen_line = cur_ofs; gen_column = 0 } in
                   cur_ofs + generated_line_count, (offset, `Map sm) :: sections)
                 ~init:(0, [])
             in
             List.rev sections)
        }
      in
      (* preserve some info from [init_sm] *)
      let merged_sourcemap =
        { merged_sourcemap with
          sections =
            List.map merged_sourcemap.sections ~f:(fun (ofs, `Map sm) ->
                ofs, `Map { sm with sourceroot = init_sm.sourceroot })
        }
      in
      (match file with
      | None ->
          let data = Source_map.Index.to_string merged_sourcemap in
          let s = sourceMappingURL_base64 ^ Base64.encode_exn data in
          Line_writer.write oc s ~add:(fun _ -> ()) |> ignore
      | Some file ->
          Source_map.Index.to_file merged_sourcemap file;
          let s = sourceMappingURL ^ Filename.basename file in
          Line_writer.write oc s ~add:(fun _ -> ()) |> ignore);
      if times () then Format.eprintf "  sourcemap: %a@." Timer.print t

let link ~output ~linkall ~mklib ~toplevel ~files ~resolve_sourcemap_url ~source_map =
  try link ~output ~linkall ~toplevel ~mklib ~files ~resolve_sourcemap_url ~source_map
  with Build_info.Incompatible_build_info { key; first = f1, v1; second = f2, v2 } ->
    let string_of_v = function
      | None -> "<empty>"
      | Some v -> v
    in
    failwith
      (Printf.sprintf
         "Incompatible build info detected while linking.\n - %s: %s=%s\n - %s: %s=%s"
         f1
         key
         (string_of_v v1)
         f2
         key
         (string_of_v v2))
