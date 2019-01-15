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
  ; disable : 'a }

type t =
  { debug : string list on_off
  ; optim : string list on_off
  ; quiet : bool
  ; custom_header : string option }

let debug =
  let doc = "enable debug [$(docv)]." in
  let all = List.map (Debug.available ()) ~f:(fun s -> s, s) in
  let arg =
    Arg.(value & opt_all (list (enum all)) [] & info ["debug"] ~docv:"SECTION" ~doc)
  in
  Term.(pure List.flatten $ arg)

let enable =
  let doc = "Enable optimization [$(docv)]." in
  let all = List.map (Config.Flag.available ()) ~f:(fun s -> s, s) in
  let arg =
    Arg.(value & opt_all (list (enum all)) [] & info ["enable"] ~docv:"OPT" ~doc)
  in
  Term.(pure List.flatten $ arg)

let disable =
  let doc = "Disable optimization [$(docv)]." in
  let all = List.map (Config.Flag.available ()) ~f:(fun s -> s, s) in
  let arg =
    Arg.(value & opt_all (list (enum all)) [] & info ["disable"] ~docv:"OPT" ~doc)
  in
  Term.(pure List.flatten $ arg)

let pretty =
  let doc = "Pretty print the output." in
  Arg.(value & flag & info ["pretty"] ~doc)

let debuginfo =
  let doc = "Output debug information." in
  Arg.(value & flag & info ["debuginfo"; "debug-info"] ~doc)

let noinline =
  let doc = "Disable inlining." in
  Arg.(value & flag & info ["noinline"; "no-inline"] ~doc)

let is_quiet =
  let doc = "suppress non-error messages." in
  Arg.(value & flag & info ["quiet"; "q"] ~doc)

let custom_header =
  let doc =
    "Provide a custom header for the generated JavaScript file, useful for making the \
     script an executable file with #!/usr/bin/env node"
  in
  Arg.(value & opt (some string) None & info ["custom-header"] ~doc)

let t =
  Term.(
    pure (fun debug enable disable pretty debuginfo noinline quiet c_header ->
        let enable = if pretty then "pretty" :: enable else enable in
        let enable = if debuginfo then "debuginfo" :: enable else enable in
        let disable = if noinline then "inline" :: disable else disable in
        let disable_if_pretty name disable =
          if pretty && not (List.mem name ~set:enable) then name :: disable else disable
        in
        let disable = disable_if_pretty "shortvar" disable in
        let disable = disable_if_pretty "share" disable in
        { debug = {enable = debug; disable = []}
        ; optim = {enable; disable}
        ; quiet
        ; custom_header = c_header } )
    $ debug
    $ enable
    $ disable
    $ pretty
    $ debuginfo
    $ noinline
    $ is_quiet
    $ custom_header)

let on_off on off t = List.iter ~f:on t.enable; List.iter ~f:off t.disable

let eval t =
  Config.Flag.(on_off enable disable t.optim);
  Debug.(on_off enable disable t.debug);
  quiet := t.quiet
