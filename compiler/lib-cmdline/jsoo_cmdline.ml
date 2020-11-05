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

open Js_of_ocaml_compiler.Stdlib
module Arg = Arg

let normalize_argv ?(warn = fun _ -> ()) a =
  let bad = ref [] in
  let a =
    Array.map
      ~f:(fun s ->
        let size = String.length s in
        if size <= 2
        then s
        else if
          Char.equal s.[0] '-'
          && (not (Char.equal s.[1] '-'))
          && not (Char.equal s.[2] '=')
        then (
          bad := s :: !bad;
          (* long option with one dash lets double the dash *)
          "-" ^ s)
        else s)
      a
  in
  if not (List.is_empty !bad)
  then
    warn
      (Format.sprintf
         "[Warning] long options with a single '-' are now deprecated. Please use '--' \
          for the following options: %s@."
         (String.concat ~sep:", " !bad));
  a
