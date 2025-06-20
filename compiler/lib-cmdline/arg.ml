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

open Js_of_ocaml_compiler
open Js_of_ocaml_compiler.Stdlib
open Cmdliner

type 'a on_off =
  { enable : 'a
  ; disable : 'a
  }

type t =
  { debug : string list on_off
  ; optim : string list on_off
  ; quiet : bool
  ; werror : bool
  ; warnings : (bool * Warning.t) list
  ; custom_header : string option
  }

let debug =
  lazy
    (let doc = "enable debug [$(docv)]." in
     let all = List.map (Debug.available ()) ~f:(fun s -> s, s) in
     let arg =
       Arg.(value & opt_all (list (enum all)) [] & info [ "debug" ] ~docv:"SECTION" ~doc)
     in
     Term.(const List.flatten $ arg))

let enable =
  lazy
    (let doc = "Enable optimization [$(docv)]." in
     let all = List.map (Config.Flag.available ()) ~f:(fun s -> s, s) in
     let arg =
       Arg.(value & opt_all (list (enum all)) [] & info [ "enable" ] ~docv:"OPT" ~doc)
     in
     Term.(const List.flatten $ arg))

let disable =
  lazy
    (let doc = "Disable optimization [$(docv)]." in
     let all = List.map (Config.Flag.available ()) ~f:(fun s -> s, s) in
     let arg =
       Arg.(value & opt_all (list (enum all)) [] & info [ "disable" ] ~docv:"OPT" ~doc)
     in
     Term.(const List.flatten $ arg))

let parse_warning s =
  let err s = `Msg (Printf.sprintf "Unknown warning %s" s) in
  if String.is_empty s
  then Error (err s)
  else
    match Warning.parse s with
    | Some n -> Ok (true, n)
    | None -> (
        match String.drop_prefix ~prefix:"no-" s with
        | Some n -> (
            match Warning.parse n with
            | Some n -> Ok (false, n)
            | None -> Error (err n))
        | None -> Error (err s))

let print_warning fmt (b, w) =
  Format.fprintf
    fmt
    "%s%s"
    (match b with
    | true -> ""
    | false -> "")
    (Warning.name w)

let warnings : (bool * Warning.t) list Term.t =
  let doc = "Enable or disable the warnings specified by the argument [$(docv)]." in
  let c : 'a Arg.conv = Arg.conv ~docv:"" (parse_warning, print_warning) in
  Arg.(value & opt_all c [] & info [ "w" ] ~docv:"WARN" ~doc)

let pretty =
  let doc = "Pretty print the output." in
  Arg.(value & flag & info [ "pretty" ] ~doc)

let debuginfo =
  let doc = "Output debug information." in
  Arg.(value & flag & info [ "debuginfo"; "debug-info" ] ~doc)

let noinline =
  let doc = "Disable inlining." in
  Arg.(value & flag & info [ "noinline"; "no-inline" ] ~doc)

let is_quiet =
  let doc = "suppress non-error messages." in
  Arg.(value & flag & info [ "quiet"; "q" ] ~doc)

let is_werror =
  let doc = "turn all warnings into errors." in
  Arg.(value & flag & info [ "Werror" ] ~doc)

let custom_header =
  let doc =
    "Provide a custom header for the generated JavaScript file, useful for making the \
     script an executable file with #!/usr/bin/env node"
  in
  Arg.(value & opt (some string) None & info [ "custom-header" ] ~doc)

let t =
  lazy
    Term.(
      const
        (fun
          debug
          enable
          disable
          pretty
          debuginfo
          noinline
          quiet
          (warnings : (bool * Warning.t) list)
          werror
          c_header
        ->
          let enable = if pretty then "pretty" :: enable else enable in
          let enable = if debuginfo then "debuginfo" :: enable else enable in
          let disable = if noinline then "inline" :: disable else disable in
          let disable_if_pretty name disable =
            if pretty && not (List.mem ~eq:String.equal name enable)
            then name :: disable
            else disable
          in
          let disable = disable_if_pretty "shortvar" disable in
          let disable = disable_if_pretty "share" disable in
          { debug = { enable = debug; disable = [] }
          ; optim = { enable; disable }
          ; warnings
          ; quiet
          ; werror
          ; custom_header = c_header
          })
      $ Lazy.force debug
      $ Lazy.force enable
      $ Lazy.force disable
      $ pretty
      $ debuginfo
      $ noinline
      $ is_quiet
      $ warnings
      $ is_werror
      $ custom_header)

let on_off on off t =
  List.iter ~f:on t.enable;
  List.iter ~f:off t.disable

let eval t =
  Config.Flag.(on_off enable disable t.optim);
  Debug.(on_off enable disable t.debug);
  List.iter t.warnings ~f:(function
    | true, w -> Warning.enable w
    | false, w -> Warning.disable w);
  Warning.quiet := t.quiet;
  Warning.werror := t.werror
