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

let info =
  Info.make
    ~name:"build-runtime"
    ~doc:"Build standalone runtime. Used for separate compilation."
    ~description:
      "Js_of_ocaml is a compiler from OCaml bytecode to Javascript. It makes it possible \
       to run pure OCaml programs in JavaScript environments like web browsers and \
       Node.js."

let command =
  let t = Cmdliner.Term.(const Compile.run $ Cmd_arg.options_runtime_only) in
  Cmdliner.Cmd.v info t
