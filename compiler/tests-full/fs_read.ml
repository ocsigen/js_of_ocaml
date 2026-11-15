(* Js_of_ocaml tests
 * http://www.ocsigen.org/js_of_ocaml/
 * Copyright (C) 2026 Ocsigen team
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

(* Reads back the files embedded by [build-fs], for [fs_check.js]. *)

open Js_of_ocaml

let read name =
  let ic = open_in_bin name in
  let s = really_input_string ic (in_channel_length ic) in
  close_in ic;
  s

let () =
  Js.Unsafe.set
    Js.Unsafe.global
    "jsooTestReadFile"
    (Js.wrap_callback (fun name -> Js.bytestring (read (Js.to_string name))))
