(* Js_of_ocaml compiler
 * http://www.ocsigen.org/js_of_ocaml/
 * Copyright (C) 2010 Jérôme Vouillon
 * Laboratoire PPS - CNRS Université Paris Diderot
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

let _ =
  Sys.catch_break true;
  let argv = Jsoo_cmdline.normalize_argv ~warn:(warn "%s") Sys.argv in
  let argv =
    let like_arg x = String.length x > 0 && Char.equal x.[0] '-' in
    let like_command x =
      String.length x > 0
      && (not (Char.equal x.[0] '-'))
      && String.for_all x ~f:(function
             | 'a' .. 'z' | 'A' .. 'Z' | '-' -> true
             | _ -> false)
    in
    match Array.to_list argv with
    | exe :: maybe_command :: rest ->
        if like_command maybe_command || like_arg maybe_command
        then argv
        else
          (* Keep compatibility with js_of_ocaml < 3.6.0 *)
          Array.of_list (exe :: Cmdliner.Cmd.name Compile.command :: maybe_command :: rest)
    | _ -> argv
  in
  try
    match
      Cmdliner.Cmd.eval_value
        ~catch:false
        ~argv
        (Cmdliner.Cmd.group
           ~default:Compile.term
           (Compile.info "js_of_ocaml")
           [ Link.command
           ; Build_fs.command
           ; Build_runtime.command
           ; Print_runtime.command
           ; Check_runtime.command
           ; Compile.command
           ])
    with
    | Ok (`Ok () | `Help | `Version) ->
        if !warnings > 0 && !werror
        then (
          Format.eprintf "%s: all warnings being treated as errors@." Sys.argv.(0);
          exit 1)
        else exit 0
    | Error `Term -> exit 1
    | Error `Parse -> exit Cmdliner.Cmd.Exit.cli_error
    | Error `Exn -> ()
    (* should not happen *)
  with
  | (Match_failure _ | Assert_failure _ | Not_found) as exc ->
      let backtrace = Printexc.get_backtrace () in
      Format.eprintf
        "%s: You found a bug. Please report it at \
         https://github.com/ocsigen/js_of_ocaml/issues :@."
        Sys.argv.(0);
      Format.eprintf "Error: %s@." (Printexc.to_string exc);
      prerr_string backtrace;
      exit Cmdliner.Cmd.Exit.internal_error
  | Magic_number.Bad_magic_number s ->
      Format.eprintf "%s: Error: Not an ocaml bytecode file@." Sys.argv.(0);
      Format.eprintf "%s: Error: Invalid magic number %S@." Sys.argv.(0) s;
      exit 1
  | Magic_number.Bad_magic_version h ->
      Format.eprintf "%s: Error: Bytecode version mismatch.@." Sys.argv.(0);
      let k =
        match Magic_number.kind h with
        | (`Cmo | `Cma | `Exe) as x -> x
        | `Other _ -> assert false
      in
      let comp =
        if Magic_number.compare h (Magic_number.current k) < 0
        then "an older"
        else "a newer"
      in
      Format.eprintf
        "%s: Error: Your ocaml bytecode and the js_of_ocaml compiler have to be compiled \
         with the same version of ocaml.@."
        Sys.argv.(0);
      Format.eprintf
        "%s: Error: The Js_of_ocaml compiler has been compiled with ocaml version %s.@."
        Sys.argv.(0)
        Sys.ocaml_version;
      Format.eprintf
        "%s: Error: Its seems that your ocaml bytecode has been compiled with %s version \
         of ocaml.@."
        Sys.argv.(0)
        comp;
      exit 1
  | Failure s ->
      Format.eprintf "%s: Error: %s@." Sys.argv.(0) s;
      exit 1
  | exc ->
      let backtrace = Printexc.get_backtrace () in
      Format.eprintf "%s: Error: %s@." Sys.argv.(0) (Printexc.to_string exc);
      prerr_string backtrace;
      exit 1
