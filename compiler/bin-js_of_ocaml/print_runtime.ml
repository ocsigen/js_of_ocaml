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

let f () =
  let output = stdout in
  let new_line () = output_string output "\n" in
  List.iter Js_of_ocaml_compiler_runtime_files.runtime ~f:(fun x ->
      output_string output (Printf.sprintf "//# 1 %S" (Builtins.File.name x));
      new_line ();
      output_string output (Builtins.File.content x);
      new_line ())

let info =
  Info.make
    ~name:"print-standard-runtime"
    ~doc:"Print standard runtime to stdout"
    ~description:"js_of_ocaml-print-standard-runtime dump the standard runtime to stdout."

let command =
  let t = Cmdliner.Term.(const f $ const ()) in
  Cmdliner.Cmd.v info t
