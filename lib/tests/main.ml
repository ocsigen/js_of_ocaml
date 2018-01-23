(* Js_of_ocaml compiler
 * http://www.ocsigen.org/js_of_ocaml/
 * Copyright (C) 2017 Hugo Heuzard
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
open Js_of_ocaml

module Css_angle = Css_angle
module Css_color = Css_color
module Css_length = Css_length
module Json_convert = Json_convert
module Regexp1 = Regexp1
module Time = Time
module Url1 = Url1

let _ =
  Firebug.console##log(
    Js.string (
      Printf.sprintf "Test results: %d successes out of %d tests"
        !Common.success_count !Common.test_count
    )
  );
  if !Common.success_count <> !Common.test_count
  then exit 1
  else exit 0
