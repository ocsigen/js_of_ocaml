(* Js_of_ocaml compiler
 * http://www.ocsigen.org/js_of_ocaml/
 * Copyright (C) 2020 Hugo Heuzard
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

open! Js_of_ocaml_compiler.Stdlib
open Js_of_ocaml_compiler
open Cmdliner

type t =
  { common : Jsoo_cmdline.Arg.t
  ; source_map : Source_map.Encoding_spec.t option
  ; js_files : string list
  ; output_file : string option
  ; resolve_sourcemap_url : bool
  ; linkall : bool
  ; mklib : bool
  ; dynlink : bool
  ; esm : bool
  ; bundle : bool
  ; no_tree_shake : bool
  }

let options =
  let output_file =
    let doc = "Set output file name to [$(docv)]." in
    Arg.(value & opt (some string) None & info [ "o" ] ~docv:"FILE" ~doc)
  in
  let no_sourcemap =
    let doc =
      "Don't generate source map. All other source map related flags will be ignored."
    in
    Arg.(value & flag & info [ "no-sourcemap"; "no-source-map" ] ~doc)
  in
  let sourcemap =
    let doc = "Generate source map." in
    Arg.(value & flag & info [ "sourcemap"; "source-map" ] ~doc)
  in
  let sourcemap_inline_in_js =
    let doc = "Inline sourcemap in the generated JavaScript." in
    Arg.(value & flag & info [ "source-map-inline" ] ~doc)
  in
  let sourcemap_empty =
    let doc = "Always generate empty source maps." in
    Arg.(value & flag & info [ "empty-sourcemap"; "empty-source-map" ] ~doc)
  in
  let sourcemap_root =
    let doc = "root dir for source map." in
    Arg.(value & opt (some string) None & info [ "source-map-root" ] ~doc)
  in
  let resolve_sourcemap_url =
    let doc = "Resolve source map url." in
    Arg.(value & opt bool false & info [ "resolve-sourcemap-url" ] ~doc)
  in
  let js_files =
    let doc = "Link JavaScript files [$(docv)]." in
    Arg.(value & pos_all string [] & info [] ~docv:"JS_FILES" ~doc)
  in
  let linkall =
    let doc = "Link all compilation units." in
    Arg.(value & flag & info [ "linkall" ] ~doc)
  in
  let mklib =
    let doc =
      "Build a library (.cma.js file) with the js files (.cmo.js files) given on the \
       command line. Similar to ocamlc -a."
    in
    Arg.(value & flag & info [ "a" ] ~doc)
  in
  let dynlink =
    let doc = "Enable dynlink/toplevel support." in
    Arg.(value & flag & info [ "dynlink"; "toplevel" ] ~doc)
  in
  let esm =
    let doc =
      "Experimental: link ECMAScript modules produced by 'js_of_ocaml --esm'. The output \
       is an entry module importing every file in order; with '--bundle' the modules are \
       merged into a single self-contained module instead."
    in
    Arg.(value & flag & info [ "esm" ] ~doc)
  in
  let bundle =
    let doc =
      "Experimental: bundle the modules into a single module (requires '--esm')."
    in
    Arg.(value & flag & info [ "bundle" ] ~doc)
  in
  let no_tree_shake =
    let doc = "Experimental: disable tree shaking when bundling." in
    Arg.(value & flag & info [ "no-tree-shake" ] ~doc)
  in
  let build_t
      common
      no_sourcemap
      sourcemap
      sourcemap_inline_in_js
      sourcemap_empty
      sourcemap_root
      output_file
      resolve_sourcemap_url
      js_files
      linkall
      mklib
      dynlink
      esm
      bundle
      no_tree_shake =
    let chop_extension s = try Filename.chop_extension s with Invalid_argument _ -> s in
    let source_map =
      if (not no_sourcemap) && (sourcemap || sourcemap_inline_in_js)
      then
        let file, sm_output_file =
          match output_file with
          | Some file when sourcemap_inline_in_js -> Some file, None
          | Some file -> Some file, Some (chop_extension file ^ ".map")
          | None -> None, None
        in
        let source_map =
          { (Source_map.Standard.empty ~inline_source_content:true) with
            file
          ; sourceroot = sourcemap_root
          }
        in
        let spec =
          { Source_map.Encoding_spec.output_file = sm_output_file
          ; source_map
          ; keep_empty = sourcemap_empty
          }
        in
        Some spec
      else None
    in
    `Ok
      { common
      ; output_file
      ; js_files
      ; source_map
      ; resolve_sourcemap_url
      ; linkall
      ; mklib
      ; dynlink
      ; esm
      ; bundle
      ; no_tree_shake
      }
  in
  let t =
    Term.(
      const build_t
      $ Lazy.force Jsoo_cmdline.Arg.t
      $ no_sourcemap
      $ sourcemap
      $ sourcemap_inline_in_js
      $ sourcemap_empty
      $ sourcemap_root
      $ output_file
      $ resolve_sourcemap_url
      $ js_files
      $ linkall
      $ mklib
      $ dynlink
      $ esm
      $ bundle
      $ no_tree_shake)
  in
  Term.ret t

(* ES module linking (experimental). Paths are used as module identifiers:
   normalize them so that resolved import specifiers and command-line paths
   agree on separators and [.]/[..] components. *)
let normalize_path path =
  let path =
    if Sys.win32
    then String.map path ~f:(fun c -> if Char.equal c '\\' then '/' else c)
    else path
  in
  let parts = String.split_on_char ~sep:'/' path in
  let rec normalize acc = function
    | [] -> List.rev acc
    | "." :: rest -> normalize acc rest
    | ".." :: rest -> (
        match acc with
        | (".." | "") :: _ | [] -> normalize (".." :: acc) rest
        | _ :: acc' -> normalize acc' rest)
    | part :: rest -> normalize (part :: acc) rest
  in
  String.concat ~sep:"/" (normalize [] parts)

let absolute_path path =
  normalize_path
    (if Filename.is_relative path then Filename.concat (Sys.getcwd ()) path else path)

(* The specifier importing [path] from a module located in [dir]. *)
let relative_specifier ~dir path =
  let rec strip_common_prefix l1 l2 =
    match l1, l2 with
    | x :: r1, y :: r2 when String.equal x y -> strip_common_prefix r1 r2
    | _ -> l1, l2
  in
  let dir_parts = String.split_on_char ~sep:'/' (absolute_path dir) in
  let path_parts = String.split_on_char ~sep:'/' (absolute_path path) in
  let dir_rest, path_rest = strip_common_prefix dir_parts path_parts in
  let ups = List.map dir_rest ~f:(fun _ -> "..") in
  let parts =
    match ups with
    | [] -> "." :: path_rest
    | _ -> ups @ path_rest
  in
  String.concat ~sep:"/" parts

(* A compiled .js file may hold several compilation units (a .cma): each unit
   starts with a '//# unitInfo: Provides:' line. Such a file is a container,
   not a valid module by itself — units may declare the same names (helpers,
   import bindings) — and is only meant to be consumed by the linker, which
   splits it back into one module per unit. *)
let split_units content =
  let unit_start = "//# unitInfo: Provides:" in
  let lines = String.split_on_char ~sep:'\n' content in
  let finish name acc chunks =
    match acc with
    | [] -> chunks
    | _ -> (name, String.concat ~sep:"\n" (List.rev acc)) :: chunks
  in
  let rec loop name acc chunks = function
    | [] -> List.rev (finish name acc chunks)
    | line :: rest ->
        if String.starts_with ~prefix:unit_start line
        then
          let unit_name =
            String.trim
              (String.sub
                 line
                 ~pos:(String.length unit_start)
                 ~len:(String.length line - String.length unit_start))
          in
          (* An empty 'Provides:' is file-level metadata (e.g. the runtime,
             which provides no OCaml unit), not a unit boundary. *)
          if String.is_empty unit_name
          then loop name (line :: acc) chunks rest
          else loop (Some unit_name) [ line ] (finish name acc chunks) rest
        else loop name (line :: acc) chunks rest
  in
  match loop None [] [] lines with
  | (None, _header) :: (_ :: _ as units) -> units
  | units -> units

(* Linker inputs: plain modules (no unit information, e.g. the runtime) and
   compilation units. Units reference each other with [import X from
   "./X.js"] specifiers where [X] is a unit name: the linker resolves those
   by name, wherever the unit actually comes from. *)
type input =
  | Plain of string (* path *)
  | Unit of
      { name : string
      ; file : string (* file the unit comes from *)
      ; content : string
      ; whole_file : bool (* the unit is the whole file, not a chunk *)
      }

let read_inputs files =
  let inputs =
    List.concat_map files ~f:(fun file ->
        let content = Fs.read_file file in
        match split_units content with
        | [] | [ (None, _) ] -> [ Plain file ]
        | [ (Some name, _) ] -> [ Unit { name; file; content; whole_file = true } ]
        | units ->
            List.map units ~f:(fun (name, chunk) ->
                match name with
                | Some name -> Unit { name; file; content = chunk; whole_file = false }
                | None -> assert false))
  in
  let (_ : StringSet.t) =
    List.fold_left inputs ~init:StringSet.empty ~f:(fun acc input ->
        match input with
        | Plain _ -> acc
        | Unit { name; _ } ->
            if StringSet.mem name acc
            then failwith (Printf.sprintf "unit '%s' provided by several files" name)
            else StringSet.add name acc)
  in
  inputs

(* The unit name of an [import X from "./X.js"] specifier. *)
let unit_of_specifier spec =
  match String.drop_prefix ~prefix:"./" spec with
  | Some rest when String.ends_with ~suffix:".js" rest && not (String.contains rest '/')
    -> Some (String.sub rest ~pos:0 ~len:(String.length rest - 3))
  | Some _ | None -> None

let parse_string ~filename content =
  let lexer = Parse_js.Lexer.of_string ~filename content in
  Parse_js.parse `Module lexer

(* Rebase the relative import specifiers of a unit extracted from a file in
   [src_dir] so that the unit can live in [dst_dir]. Imports of other units
   are left alone: all units end up next to each other in [dst_dir]. *)
let rebase_imports ~unit_names ~src_dir ~dst_dir program =
  List.map program ~f:(fun ((stmt : Javascript.statement), loc) ->
      match stmt with
      | Import (({ from = Utf8_string.Utf8 spec; _ } as import), pi)
        when (String.starts_with ~prefix:"./" spec
             || String.starts_with ~prefix:"../" spec)
             &&
             match unit_of_specifier spec with
             | Some unit -> not (StringSet.mem unit unit_names)
             | None -> true ->
          let target = normalize_path (Filename.concat src_dir spec) in
          let spec = relative_specifier ~dir:dst_dir target in
          ( Javascript.Import ({ import with from = Utf8_string.of_string_exn spec }, pi)
          , loc )
      | _ -> stmt, loc)

let print_program output program =
  let pp = Pretty_print.to_out_channel output in
  Driver.configure pp;
  let program = Driver.name_variables program in
  let (_ : Source_map.info) = Js_output.program pp program in
  ()

let link_esm ~output_file ~files ~bundle ~tree_shake ~linkall =
  let output_dir =
    match output_file with
    | Some file -> Filename.dirname file
    | None -> "."
  in
  let with_output f =
    match output_file with
    | None -> f stdout
    | Some file -> Filename.gen_file file f
  in
  let inputs = read_inputs files in
  let unit_names =
    List.fold_left inputs ~init:StringSet.empty ~f:(fun acc input ->
        match input with
        | Plain _ -> acc
        | Unit { name; _ } -> StringSet.add name acc)
  in
  (* Units coming from a multi-unit container (.cma.js) behave like archive
     members: they are linked only when another linked unit references them
     (through an import), matching ocamlc. [--linkall] links them all. Units
     given as standalone files and plain modules are always linked. *)
  let root = function
    | Plain _ -> true
    | Unit { whole_file; _ } -> whole_file || linkall
  in
  if not bundle
  then
    (* Emit an entry module importing each root in link order. ES semantics
       guarantee the modules are evaluated in declaration order (post-order
       depth-first), and each unit's own imports evaluate its dependencies
       first. Units import each other as siblings ("./X.js"), so they must
       all live in one directory: materialize them into the entry's
       directory, except unit files already there under their unit name. *)
    let specifiers =
      List.filter_map inputs ~f:(fun input ->
          match input with
          | Plain file -> Some (relative_specifier ~dir:output_dir file)
          | Unit { name; file; content; whole_file } ->
              let spec = "./" ^ name ^ ".js" in
              let in_place =
                whole_file
                && String.equal
                     (absolute_path file)
                     (absolute_path (Filename.concat output_dir (name ^ ".js")))
              in
              if not in_place
              then begin
                let src_dir = Filename.dirname (absolute_path file) in
                let program =
                  parse_string ~filename:file content
                  |> rebase_imports ~unit_names ~src_dir ~dst_dir:output_dir
                in
                Filename.gen_file
                  (Filename.concat output_dir (name ^ ".js"))
                  (fun out -> print_program out program)
              end;
              if root input then Some spec else None)
    in
    with_output (fun output ->
        List.iter specifiers ~f:(fun spec -> Printf.fprintf output "import %S;\n" spec))
  else
    (* Bundle in memory: each unit becomes a module with a synthetic id
       ('<path>!<unit>'); [Filename.dirname] still resolves their relative
       imports against the file's directory, and imports of other units are
       resolved by unit name. *)
    let sections =
      List.map inputs ~f:(fun input ->
          match input with
          | Plain file -> absolute_path file, None, Fs.read_file file
          | Unit { name; file; content; whole_file } ->
              let id =
                if whole_file
                then absolute_path file
                else Printf.sprintf "%s!%s" (absolute_path file) name
              in
              id, Some name, content)
    in
    let sources = String.Hashtbl.create 64 in
    let unit_ids = String.Hashtbl.create 64 in
    List.iter sections ~f:(fun (id, name, content) ->
        String.Hashtbl.add sources id content;
        Option.iter name ~f:(fun name -> String.Hashtbl.add unit_ids name id));
    let parse path =
      match String.Hashtbl.find_opt sources path with
      | Some content -> parse_string ~filename:path content
      | None -> parse_string ~filename:path (Fs.read_file path)
    in
    let resolve ~from specifier =
      match unit_of_specifier specifier with
      | Some unit when String.Hashtbl.mem unit_ids unit ->
          String.Hashtbl.find_opt unit_ids unit
      | Some _ | None ->
          if
            Filename.is_relative specifier
            && (String.starts_with ~prefix:"./" specifier
               || String.starts_with ~prefix:"../" specifier)
          then
            let dir = Filename.dirname from in
            let resolved = normalize_path (Filename.concat dir specifier) in
            if Sys.file_exists resolved then Some resolved else None
          else None
    in
    let entry_points =
      List.filter_map (List.combine inputs sections) ~f:(fun (input, (id, _, _)) ->
          if root input then Some id else None)
    in
    let program = Esm_bundle.bundle_modules ~parse ~resolve ~entry_points ~tree_shake in
    (* The bundle of a linked program is itself a program: drop the export
       statements the bundler generates for entry modules (each entry unit
       exports its module block as [default]). *)
    let program =
      List.filter program ~f:(fun (stmt, _) ->
          match (stmt : Javascript.statement) with
          | Export _ -> false
          | _ -> true)
    in
    let program = Driver.simplify_js program in
    with_output (fun output -> print_program output program)

let f
    { common
    ; output_file
    ; source_map
    ; resolve_sourcemap_url
    ; js_files
    ; linkall
    ; mklib
    ; esm
    ; bundle
    ; no_tree_shake
    ; dynlink =
        _
        (* TODO: when [dynlink] (i.e. --dynlink/--toplevel) is false, the linker
         could optimize global references. Currently, [caml_register_global] and
         [caml_get_global] use string-keyed lookups on [caml_global_data]. Since
         the linker already knows the full symbol table, it could replace these
         with index-based accesses ([caml_register_global_by_index]) and skip
         emitting [caml_set_link_info] entirely. *)
    } =
  Config.set_target `JavaScript;
  Jsoo_cmdline.Arg.eval common;
  Linker.reset ();
  if esm
  then
    link_esm ~output_file ~files:js_files ~bundle ~tree_shake:(not no_tree_shake) ~linkall
  else
    let with_output f =
      match output_file with
      | None -> f stdout
      | Some file -> Filename.gen_file file f
    in
    with_output (fun output ->
        Link_js.link
          ~output
          ~linkall
          ~mklib
          ~files:js_files
          ~source_map
          ~resolve_sourcemap_url)

let info =
  Info.make
    ~name:"link"
    ~doc:"Js_of_ocaml linker"
    ~description:
      "js_of_ocaml-link is a JavaScript linker. It can concatenate multiple JavaScript \
       files keeping sourcemap information."

let command =
  let t = Cmdliner.Term.(const f $ options) in
  Cmdliner.Cmd.v info t
