(* Js_of_ocaml tests
 * http://www.ocsigen.org/js_of_ocaml/
 * Copyright (C) 2019 Ty Overby
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2 of the License, or
 * (at your option) any later version.
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

open Util

let%expect_test _ =
  compile_lib [] "empty"
  |> compile_cmo_to_javascript ~sourcemap:true
  |> Filetype.read_js
  |> Filetype.string_of_js_text
  |> print_endline;
  Sys.remove "empty.cma";
  Sys.remove "empty.js";
  [%expect
    {|
      (function(joo_global_object){"use strict";return}(globalThis));

      //# sourceMappingURL=empty.map
    |}]
