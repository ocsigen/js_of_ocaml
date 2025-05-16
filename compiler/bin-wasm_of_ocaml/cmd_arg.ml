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

open! Js_of_ocaml_compiler.Stdlib
open Js_of_ocaml_compiler
open Cmdliner

let is_dir_sep = function
  | '/' -> true
  | '\\' when String.equal Filename.dir_sep "\\" -> true
  | _ -> false

let trim_trailing_dir_sep s =
  if String.equal s ""
  then s
  else
    let len = String.length s in
    let j = ref (len - 1) in
    while !j >= 0 && is_dir_sep (String.unsafe_get s !j) do
      decr j
    done;
    if !j >= 0 then String.sub s ~pos:0 ~len:(!j + 1) else String.sub s ~pos:0 ~len:1

let normalize_include_dirs dirs = List.map dirs ~f:trim_trailing_dir_sep

let normalize_effects (effects : [ `Cps | `Jspi ] option) common : Config.effects_backend
    =
  match effects with
  | None ->
      (* For backward compatibility, consider that [--enable effects] alone means
        [--effects cps] *)
      if List.mem ~eq:String.equal "effects" common.Jsoo_cmdline.Arg.optim.enable
      then `Cps
      else `Jspi
  | Some ((`Cps | `Jspi) as e) -> e

type t =
  { common : Jsoo_cmdline.Arg.t
  ; (* compile option *)
    profile : Profile.t option
  ; runtime_files : string list
  ; runtime_only : bool
  ; output_file : string * bool
  ; input_file : string option
  ; enable_source_maps : bool
  ; sourcemap_root : string option
  ; sourcemap_don't_inline_content : bool
  ; params : (string * string) list
  ; include_dirs : string list
  ; effects : Config.effects_backend
  }

let options () =
  let runtime_files =
    let doc = "Link JavaScript and WebAssembly files [$(docv)]. " in
    Arg.(value & pos_left ~rev:true 0 string [] & info [] ~docv:"RUNTIME_FILES" ~doc)
  in
  let output_file =
    let doc = "Set output file name to [$(docv)]." in
    Arg.(value & opt (some string) None & info [ "o" ] ~docv:"FILE" ~doc)
  in
  let input_file =
    let doc = "Compile the bytecode program [$(docv)]. " in
    Arg.(required & pos ~rev:true 0 (some string) None & info [] ~docv:"PROGRAM" ~doc)
  in
  let profile =
    let doc = "Set optimization profile : [$(docv)]." in
    let profile =
      List.map Profile.all ~f:(fun p -> string_of_int (Profile.to_int p), p)
    in
    Arg.(value & opt (some (enum profile)) None & info [ "opt" ] ~docv:"NUM" ~doc)
  in
  let linkall =
    let doc = "Currently ignored (for compatibility with Js_of_ocaml)." in
    Arg.(value & flag & info [ "linkall" ] ~doc)
  in
  let no_sourcemap =
    let doc = "Disable sourcemap output." in
    Arg.(value & flag & info [ "no-sourcemap"; "no-source-map" ] ~doc)
  in
  let sourcemap =
    let doc = "Output source locations." in
    Arg.(value & flag & info [ "sourcemap"; "source-map"; "source-map-inline" ] ~doc)
  in
  let sourcemap_don't_inline_content =
    let doc = "Do not inline sources in source map." in
    Arg.(value & flag & info [ "source-map-no-source" ] ~doc)
  in
  let sourcemap_root =
    let doc = "root dir for source map." in
    Arg.(value & opt (some string) None & info [ "source-map-root" ] ~doc)
  in
  let set_param =
    let doc = "Set compiler options." in
    let all = List.map (Config.Param.all ()) ~f:(fun (x, _) -> x, x) in
    Arg.(
      value
      & opt_all (list (pair ~sep:'=' (enum all) string)) []
      & info [ "set" ] ~docv:"PARAM=VALUE" ~doc)
  in
  let include_dirs =
    let doc = "Add [$(docv)] to the list of include directories." in
    Arg.(value & opt_all string [] & info [ "I" ] ~docv:"DIR" ~doc)
  in
  let effects =
    let doc =
      "Select an implementation of effect handlers. [$(docv)] should be one of $(b,jspi) \
       (the default) or $(b,cps)."
    in
    Arg.(
      value
      & opt (some (enum [ "jspi", `Jspi; "cps", `Cps ])) None
      & info [ "effects" ] ~docv:"KIND" ~doc)
  in
  let build_t
      common
      set_param
      include_dirs
      profile
      _
      sourcemap
      no_sourcemap
      sourcemap_don't_inline_content
      sourcemap_root
      output_file
      input_file
      runtime_files
      effects =
    let chop_extension s = try Filename.chop_extension s with Invalid_argument _ -> s in
    let output_file =
      let ext =
        try
          snd
            (List.find
               ~f:(fun (ext, _) -> Filename.check_suffix input_file ext)
               [ ".cmo", ".wasmo"; ".cma", ".wasma" ])
        with Not_found -> ".js"
      in
      match output_file with
      | Some s -> s, true
      | None -> chop_extension input_file ^ ext, false
    in
    let params : (string * string) list = List.flatten set_param in
    let enable_source_maps = (not no_sourcemap) && sourcemap in
    let include_dirs = normalize_include_dirs include_dirs in
    let effects = normalize_effects effects common in
    `Ok
      { common
      ; params
      ; include_dirs
      ; profile
      ; output_file
      ; input_file = Some input_file
      ; runtime_files
      ; runtime_only = false
      ; enable_source_maps
      ; sourcemap_root
      ; sourcemap_don't_inline_content
      ; effects
      }
  in
  let t =
    Term.(
      const build_t
      $ Lazy.force Jsoo_cmdline.Arg.t
      $ set_param
      $ include_dirs
      $ profile
      $ linkall
      $ sourcemap
      $ no_sourcemap
      $ sourcemap_don't_inline_content
      $ sourcemap_root
      $ output_file
      $ input_file
      $ runtime_files
      $ effects)
  in
  Term.ret t

let options_runtime_only () =
  let runtime_files =
    let doc = "Link JavaScript and WebAssembly files [$(docv)]. " in
    Arg.(value & pos_all string [] & info [] ~docv:"RUNTIME_FILES" ~doc)
  in
  let output_file =
    let doc = "Set output file name to [$(docv)]." in
    Arg.(required & opt (some string) None & info [ "o" ] ~docv:"FILE" ~doc)
  in
  let no_sourcemap =
    let doc =
      "Don't generate source map. All other source map related flags will be ignored."
    in
    Arg.(value & flag & info [ "no-sourcemap"; "no-source-map" ] ~doc)
  in
  let sourcemap =
    let doc = "Generate source map." in
    Arg.(value & flag & info [ "sourcemap"; "source-map"; "source-map-inline" ] ~doc)
  in
  let sourcemap_don't_inline_content =
    let doc = "Do not inline sources in source map." in
    Arg.(value & flag & info [ "source-map-no-source" ] ~doc)
  in
  let sourcemap_root =
    let doc = "root dir for source map." in
    Arg.(value & opt (some string) None & info [ "source-map-root" ] ~doc)
  in
  let include_dirs =
    let doc = "Add [$(docv)] to the list of include directories." in
    Arg.(value & opt_all string [] & info [ "I" ] ~docv:"DIR" ~doc)
  in
  let set_param =
    let doc = "Set compiler options." in
    let all = List.map (Config.Param.all ()) ~f:(fun (x, _) -> x, x) in
    Arg.(
      value
      & opt_all (list (pair ~sep:'=' (enum all) string)) []
      & info [ "set" ] ~docv:"PARAM=VALUE" ~doc)
  in
  let effects =
    let doc =
      "Select an implementation of effect handlers. [$(docv)] should be one of $(b,jspi) \
       (the default) or $(b,cps)."
    in
    Arg.(
      value
      & opt (some (enum [ "jspi", `Jspi; "cps", `Cps ])) None
      & info [ "effects" ] ~docv:"KIND" ~doc)
  in
  let build_t
      common
      set_param
      include_dirs
      sourcemap
      no_sourcemap
      sourcemap_don't_inline_content
      sourcemap_root
      output_file
      runtime_files
      effects =
    let params : (string * string) list = List.flatten set_param in
    let enable_source_maps = (not no_sourcemap) && sourcemap in
    let include_dirs = normalize_include_dirs include_dirs in
    let effects = normalize_effects effects common in
    `Ok
      { common
      ; params
      ; include_dirs
      ; profile = None
      ; output_file = output_file, true
      ; input_file = None
      ; runtime_files
      ; runtime_only = true
      ; enable_source_maps
      ; sourcemap_root
      ; sourcemap_don't_inline_content
      ; effects
      }
  in
  let t =
    Term.(
      const build_t
      $ Lazy.force Jsoo_cmdline.Arg.t
      $ set_param
      $ include_dirs
      $ sourcemap
      $ no_sourcemap
      $ sourcemap_don't_inline_content
      $ sourcemap_root
      $ output_file
      $ runtime_files
      $ effects)
  in
  Term.ret t
