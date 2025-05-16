(* Js_of_ocaml compiler
 * http://www.ocsigen.org/js_of_ocaml/
 * Copyright (C) 2014 Hugo Heuzard
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

let normalize_effects (effects : [ `Cps | `Double_translation ] option) common :
    Config.effects_backend =
  match effects with
  | None ->
      (* For backward compatibility, consider that [--enable effects] alone means
         [--effects cps] *)
      if List.mem ~eq:String.equal "effects" common.Jsoo_cmdline.Arg.optim.enable
      then `Cps
      else `Disabled
  | Some ((`Cps | `Double_translation) as e) -> (e :> Config.effects_backend)

type t =
  { common : Jsoo_cmdline.Arg.t
  ; (* compile option *)
    profile : Profile.t option
  ; source_map : Source_map.Encoding_spec.t option
  ; runtime_files : string list
  ; no_runtime : bool
  ; include_runtime : bool
  ; output_file : [ `Name of string | `Stdout ] * bool
  ; bytecode : [ `File of string | `Stdin | `None ]
  ; params : (string * string) list
  ; static_env : (string * string) list
  ; wrap_with_fun : [ `Iife | `Named of string | `Anonymous ]
  ; target_env : Target_env.t
  ; (* toplevel *)
    dynlink : bool
  ; linkall : bool
  ; toplevel : bool
  ; export_file : string option
  ; no_cmis : bool
  ; (* filesystem *)
    include_dirs : string list
  ; fs_files : string list
  ; fs_output : string option
  ; fs_external : bool
  ; keep_unit_names : bool
  ; effects : Config.effects_backend
  }

let wrap_with_fun_conv =
  let conv s =
    if String.equal s ""
    then Ok `Anonymous
    else if Javascript.is_ident s
    then Ok (`Named s)
    else Error (`Msg "must be empty or a valid JavaScript identifier")
  in
  let printer fmt o =
    Format.fprintf
      fmt
      "%s"
      (match o with
      | `Anonymous -> ""
      | `Named s -> s
      | `Iife -> "<Immediately Invoked Function Expression>")
  in
  Arg.conv (conv, printer)

let toplevel_section = "OPTIONS (TOPLEVEL)"

let options =
  let filesystem_section = "OPTIONS (FILESYSTEM)" in
  let js_files =
    let doc =
      "Link JavaScript files [$(docv)]. "
      ^ "One can refer to path relative to Findlib packages with "
      ^ "the syntax '+pkg_name/file.js'"
    in
    Arg.(value & pos_left ~rev:true 0 string [] & info [] ~docv:"JS_FILES" ~doc)
  in
  let output_file =
    let doc = "Set output file name to [$(docv)]." in
    Arg.(value & opt (some string) None & info [ "o" ] ~docv:"FILE" ~doc)
  in
  let input_file =
    let doc =
      "Compile the bytecode program [$(docv)]. "
      ^ "Use '-' to read from the standard input instead."
    in
    Arg.(required & pos ~rev:true 0 (some string) None & info [] ~docv:"PROGRAM" ~doc)
  in
  let keep_unit_names =
    let doc = "Keep unit name" in
    Arg.(value & flag & info [ "keep-unit-names" ] ~doc)
  in
  let profile =
    let doc = "Set optimization profile : [$(docv)]." in
    let profile =
      List.map Profile.all ~f:(fun p -> string_of_int (Profile.to_int p), p)
    in
    Arg.(value & opt (some (enum profile)) None & info [ "opt" ] ~docv:"NUM" ~doc)
  in
  let noruntime =
    let doc = "Do not include the standard runtime." in
    Arg.(value & flag & info [ "noruntime"; "no-runtime" ] ~doc)
  in
  let include_runtime =
    let doc =
      "Include (partial) runtime when compiling cmo and cma files to JavaScript."
    in
    Arg.(value & flag & info [ "include-runtime" ] ~doc)
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
  let sourcemap_don't_inline_content =
    let doc = "Do not inline sources in source map." in
    Arg.(value & flag & info [ "source-map-no-source" ] ~doc)
  in
  let sourcemap_empty =
    let doc = "Always generate empty source maps." in
    Arg.(value & flag & info [ "empty-sourcemap"; "empty-source-map" ] ~doc)
  in
  let sourcemap_root =
    let doc = "root dir for source map." in
    Arg.(value & opt (some string) None & info [ "source-map-root" ] ~doc)
  in
  let wrap_with_function =
    let doc =
      "Wrap the generated JavaScript code inside a function that needs to be applied \
       with the global object."
    in
    Arg.(value & opt wrap_with_fun_conv `Iife & info [ "wrap-with-fun" ] ~doc)
  in
  let set_param =
    let doc = "Set compiler options." in
    let all = List.map (Config.Param.all ()) ~f:(fun (x, _) -> x, x) in
    Arg.(
      value
      & opt_all (list (pair ~sep:'=' (enum all) string)) []
      & info [ "set" ] ~docv:"PARAM=VALUE" ~doc)
  in
  let set_env =
    let doc = "Set environment variable statically." in
    Arg.(
      value
      & opt_all (list (pair ~sep:'=' string string)) []
      & info [ "setenv" ] ~docv:"PARAM=VALUE" ~doc)
  in
  let target_env =
    let doc = "Runtime compile target." in
    let options = List.map ~f:(fun env -> Target_env.to_string env, env) Target_env.all in
    let docv = Printf.sprintf "{%s}" (String.concat ~sep:"," (List.map ~f:fst options)) in
    Arg.(
      value & opt (enum options) Target_env.Isomorphic & info [ "target-env" ] ~docv ~doc)
  in
  let toplevel =
    let doc =
      "Compile a toplevel and embed necessary cmis (unless '--no-cmis' is provided). \
       Exported compilation units can be configured with '--export'.  Note you you'll \
       also need to link against js_of_ocaml-toplevel."
    in
    Arg.(value & flag & info [ "toplevel" ] ~docs:toplevel_section ~doc)
  in
  let export_file =
    let doc =
      "File containing the list of unit to export in a toplevel, with Dynlink or with \
       --linkall. If absent, all units will be exported."
    in
    Arg.(value & opt (some string) None & info [ "export" ] ~docs:toplevel_section ~doc)
  in
  let dynlink =
    let doc =
      "Enable dynlink of bytecode files.  Use this if you want to be able to use the \
       Dynlink module. Note that you'll also need to link with \
       'js_of_ocaml-compiler.dynlink'."
    in
    Arg.(value & flag & info [ "dynlink" ] ~doc)
  in
  let linkall =
    let doc =
      "Link all primitives and compilation units. Exported compilation units can be \
       configured with '--export'."
    in
    Arg.(value & flag & info [ "linkall" ] ~doc)
  in
  let no_cmis =
    let doc = "Do not include cmis when compiling toplevel." in
    Arg.(value & flag & info [ "nocmis"; "no-cmis" ] ~docs:toplevel_section ~doc)
  in
  let include_dirs =
    let doc = "Add [$(docv)] to the list of include directories." in
    Arg.(
      value & opt_all string [] & info [ "I" ] ~docs:filesystem_section ~docv:"DIR" ~doc)
  in
  let fs_files =
    let doc = "Register [$(docv)] to the pseudo filesystem." in
    Arg.(
      value
      & opt_all string []
      & info [ "file" ] ~docs:filesystem_section ~docv:"FILE" ~doc)
  in
  let fs_external =
    Arg.(
      value
      & vflag
          true
          [ ( true
            , info
                [ "extern-fs" ]
                ~docs:filesystem_section
                ~doc:
                  "Configure pseudo-filesystem to allow registering files from outside. \
                   (default)" )
          ; ( false
            , info
                [ "no-extern-fs" ]
                ~docs:filesystem_section
                ~doc:
                  "Configure pseudo-filesystem to NOT allow registering files from \
                   outside." )
          ])
  in
  let fs_output =
    let doc = "Output the filesystem to [$(docv)]." in
    Arg.(
      value
      & opt (some string) None
      & info [ "ofs" ] ~docs:filesystem_section ~docv:"FILE" ~doc)
  in
  let effects =
    let doc =
      "Select an implementation of effect handlers. [$(docv)] should be one of $(b,cps) \
       or $(b,double-translation). Effects won't be supported if unspecified."
    in
    Arg.(
      value
      & opt (some (enum [ "cps", `Cps; "double-translation", `Double_translation ])) None
      & info [ "effects" ] ~docv:"KIND" ~doc)
  in
  let build_t
      common
      set_param
      set_env
      dynlink
      linkall
      toplevel
      export_file
      wrap_with_fun
      include_dirs
      fs_files
      fs_output
      fs_external
      no_cmis
      profile
      no_runtime
      include_runtime
      no_sourcemap
      sourcemap
      sourcemap_inline_in_js
      sourcemap_don't_inline_content
      sourcemap_empty
      sourcemap_root
      target_env
      output_file
      input_file
      js_files
      keep_unit_names
      effects =
    let inline_source_content = not sourcemap_don't_inline_content in
    let chop_extension s = try Filename.chop_extension s with Invalid_argument _ -> s in
    let runtime_files = js_files in
    let fs_external = fs_external || (toplevel && no_cmis) in
    let bytecode =
      match input_file with
      | "-" -> `Stdin
      | x -> `File x
    in
    let output_file =
      match output_file with
      | Some "-" -> `Stdout, true
      | Some s -> `Name s, true
      | None -> (
          match bytecode with
          | `File s -> `Name (chop_extension s ^ ".js"), false
          | `Stdin -> `Stdout, false)
    in
    let source_map =
      if (not no_sourcemap) && (sourcemap || sourcemap_inline_in_js)
      then
        let file, sm_output_file =
          match output_file with
          | `Name file, _ when sourcemap_inline_in_js -> Some file, None
          | `Name file, _ -> Some file, Some (chop_extension file ^ ".map")
          | `Stdout, _ -> None, None
        in
        let source_map =
          { (Source_map.Standard.empty ~inline_source_content) with
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
    let params : (string * string) list = List.flatten set_param in
    let static_env : (string * string) list = List.flatten set_env in
    let include_dirs = normalize_include_dirs include_dirs in
    let effects = normalize_effects effects common in
    `Ok
      { common
      ; params
      ; profile
      ; static_env
      ; wrap_with_fun
      ; dynlink
      ; linkall
      ; target_env
      ; toplevel
      ; export_file
      ; include_dirs
      ; runtime_files
      ; no_runtime
      ; include_runtime
      ; fs_files
      ; fs_output
      ; fs_external
      ; no_cmis
      ; output_file
      ; bytecode
      ; source_map
      ; keep_unit_names
      ; effects
      }
  in
  let t =
    Term.(
      const build_t
      $ Lazy.force Jsoo_cmdline.Arg.t
      $ set_param
      $ set_env
      $ dynlink
      $ linkall
      $ toplevel
      $ export_file
      $ wrap_with_function
      $ include_dirs
      $ fs_files
      $ fs_output
      $ fs_external
      $ no_cmis
      $ profile
      $ noruntime
      $ include_runtime
      $ no_sourcemap
      $ sourcemap
      $ sourcemap_inline_in_js
      $ sourcemap_don't_inline_content
      $ sourcemap_empty
      $ sourcemap_root
      $ target_env
      $ output_file
      $ input_file
      $ js_files
      $ keep_unit_names
      $ effects)
  in
  Term.ret t

let options_runtime_only =
  let filesystem_section = "OPTIONS (FILESYSTEM)" in
  let js_files =
    let doc =
      "Link JavaScript files [$(docv)]. "
      ^ "One can refer to path relative to Findlib packages with "
      ^ "the syntax '+pkg_name/file.js'"
    in
    Arg.(value & pos_all string [] & info [] ~docv:"JS_FILES" ~doc)
  in
  let output_file =
    let doc = "Set output file name to [$(docv)]." in
    Arg.(required & opt (some string) None & info [ "o" ] ~docv:"FILE" ~doc)
  in
  let noruntime =
    let doc = "Do not include the standard runtime." in
    Arg.(value & flag & info [ "noruntime"; "no-runtime" ] ~doc)
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
  let sourcemap_don't_inline_content =
    let doc = "Do not inline sources in source map." in
    Arg.(value & flag & info [ "source-map-no-source" ] ~doc)
  in
  let sourcemap_empty =
    let doc = "Always generate empty source maps." in
    Arg.(value & flag & info [ "empty-sourcemap"; "empty-source-map" ] ~doc)
  in
  let sourcemap_root =
    let doc = "root dir for source map." in
    Arg.(value & opt (some string) None & info [ "source-map-root" ] ~doc)
  in
  let toplevel =
    let doc =
      "Compile a toplevel and embed necessary cmis (unless '--no-cmis' is provided). \
       Exported compilation units can be configured with '--export'.  Note you you'll \
       also need to link against js_of_ocaml-toplevel."
    in
    Arg.(value & flag & info [ "toplevel" ] ~docs:toplevel_section ~doc)
  in
  let no_cmis =
    let doc = "Do not include cmis when compiling toplevel." in
    Arg.(value & flag & info [ "nocmis"; "no-cmis" ] ~docs:toplevel_section ~doc)
  in
  let target_env =
    let doc = "Runtime compile target." in
    let options = List.map ~f:(fun env -> Target_env.to_string env, env) Target_env.all in
    let docv = Printf.sprintf "{%s}" (String.concat ~sep:"," (List.map ~f:fst options)) in
    Arg.(
      value & opt (enum options) Target_env.Isomorphic & info [ "target-env" ] ~docv ~doc)
  in
  let wrap_with_function =
    let doc =
      "Wrap the generated JavaScript code inside a function that needs to be applied \
       with the global object."
    in
    Arg.(value & opt wrap_with_fun_conv `Iife & info [ "wrap-with-fun" ] ~doc)
  in
  let set_param =
    let doc = "Set compiler options." in
    let all = List.map (Config.Param.all ()) ~f:(fun (x, _) -> x, x) in
    Arg.(
      value
      & opt_all (list (pair ~sep:'=' (enum all) string)) []
      & info [ "set" ] ~docv:"PARAM=VALUE" ~doc)
  in
  let set_env =
    let doc = "Set environment variable statically." in
    Arg.(
      value
      & opt_all (list (pair ~sep:'=' string string)) []
      & info [ "setenv" ] ~docv:"PARAM=VALUE" ~doc)
  in
  let include_dirs =
    let doc = "Add [$(docv)] to the list of include directories." in
    Arg.(
      value & opt_all string [] & info [ "I" ] ~docs:filesystem_section ~docv:"DIR" ~doc)
  in
  let fs_files =
    let doc = "Register [$(docv)] to the pseudo filesystem." in
    Arg.(
      value
      & opt_all string []
      & info [ "file" ] ~docs:filesystem_section ~docv:"FILE" ~doc)
  in
  let fs_external =
    Arg.(
      value
      & vflag
          true
          [ ( true
            , info
                [ "extern-fs" ]
                ~docs:filesystem_section
                ~doc:
                  "Configure pseudo-filesystem to allow registering files from outside. \
                   (default)" )
          ; ( false
            , info
                [ "no-extern-fs" ]
                ~docs:filesystem_section
                ~doc:
                  "Configure pseudo-filesystem to NOT allow registering files from \
                   outside." )
          ])
  in
  let fs_output =
    let doc = "Output the filesystem to [$(docv)]." in
    Arg.(
      value
      & opt (some string) None
      & info [ "ofs" ] ~docs:filesystem_section ~docv:"FILE" ~doc)
  in
  let effects =
    let doc =
      "Select an implementation of effect handlers. [$(docv)] should be one of $(b,cps) \
       or $(b,double-translation). Effects won't be supported if unspecified."
    in
    Arg.(
      value
      & opt (some (enum [ "cps", `Cps; "double-translation", `Double_translation ])) None
      & info [ "effects" ] ~docv:"KIND" ~doc)
  in
  let build_t
      common
      toplevel
      no_cmis
      set_param
      set_env
      wrap_with_fun
      fs_files
      fs_output
      fs_external
      include_dirs
      no_runtime
      no_sourcemap
      sourcemap
      sourcemap_inline_in_js
      sourcemap_don't_inline_content
      sourcemap_empty
      sourcemap_root
      target_env
      output_file
      js_files
      effects =
    let inline_source_content = not sourcemap_don't_inline_content in
    let chop_extension s = try Filename.chop_extension s with Invalid_argument _ -> s in
    let runtime_files = js_files in
    let output_file =
      match output_file with
      | "-" -> `Stdout, true
      | s -> `Name s, true
    in
    let source_map =
      if (not no_sourcemap) && (sourcemap || sourcemap_inline_in_js)
      then
        let file, sm_output_file =
          match output_file with
          | `Name file, _ when sourcemap_inline_in_js -> Some file, None
          | `Name file, _ -> Some file, Some (chop_extension file ^ ".map")
          | `Stdout, _ -> None, None
        in
        let source_map =
          { (Source_map.Standard.empty ~inline_source_content) with
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
    let params : (string * string) list = List.flatten set_param in
    let static_env : (string * string) list = List.flatten set_env in
    let include_dirs = normalize_include_dirs include_dirs in
    let effects = normalize_effects effects common in
    `Ok
      { common
      ; params
      ; profile = None
      ; static_env
      ; wrap_with_fun
      ; dynlink = false
      ; linkall = false
      ; target_env
      ; toplevel
      ; export_file = None
      ; include_dirs
      ; runtime_files
      ; no_runtime
      ; include_runtime = false
      ; fs_files
      ; fs_output
      ; fs_external
      ; no_cmis
      ; output_file
      ; bytecode = `None
      ; source_map
      ; keep_unit_names = false
      ; effects
      }
  in
  let t =
    Term.(
      const build_t
      $ Lazy.force Jsoo_cmdline.Arg.t
      $ toplevel
      $ no_cmis
      $ set_param
      $ set_env
      $ wrap_with_function
      $ fs_files
      $ fs_output
      $ fs_external
      $ include_dirs
      $ noruntime
      $ no_sourcemap
      $ sourcemap
      $ sourcemap_inline_in_js
      $ sourcemap_don't_inline_content
      $ sourcemap_empty
      $ sourcemap_root
      $ target_env
      $ output_file
      $ js_files
      $ effects)
  in
  Term.ret t
