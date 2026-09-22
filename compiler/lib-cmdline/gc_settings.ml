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

(* The compiler allocates a lot, and most of it is short-lived, but it
   still keeps a large live heap when compiling a large program. With
   the default [space_overhead] (120), the major GC accounts for a
   large part of the compilation time. A larger value makes it work
   less at the expense of some more memory: at 200, compiling a
   toplevel is about 6% faster and uses 20% more memory. The setting
   from the environment (OCAMLRUNPARAM) takes precedence. *)
let space_overhead = 200

let set_by_environment () =
  let has_option var =
    match Sys.getenv_opt var with
    | None -> false
    | Some s ->
        List.exists (String.split_on_char ~sep:',' s) ~f:(fun opt ->
            String.length opt >= 2 && Char.equal opt.[0] 'o' && Char.equal opt.[1] '=')
  in
  has_option "OCAMLRUNPARAM" || has_option "CAMLRUNPARAM"

let init () =
  if not (set_by_environment ()) then Gc.set { (Gc.get ()) with space_overhead }
