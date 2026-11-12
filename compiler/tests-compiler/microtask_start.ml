(* Js_of_ocaml tests
 * http://www.ocsigen.org/js_of_ocaml/
 * Copyright (C) 2026 Hugo Heuzard
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

(* Check when the program runs relative to the code that loads it. *)
let run ?(flags = []) prog harness =
  with_temp_dir ~f:(fun () ->
      let (_ : Filetype.js_file) =
        prog
        |> Filetype.ocaml_text_of_string
        |> Filetype.write_ocaml ~name:"test.ml"
        |> compile_ocaml_to_bc
        |> compile_bc_to_javascript ~flags ~pretty:false ~sourcemap:false
      in
      harness
      |> Filetype.js_text_of_string
      |> Filetype.write_js ~name:"harness.js"
      |> run_javascript
      |> print_string)

let%expect_test
    ("the program starts in a microtask"
     [@when target_engine <> "quickjs" && target_engine <> "bun"]) =
  run
    {| let () = print_endline "program" |}
    {|
      require("./test.js");
      console.log("after require");
      queueMicrotask(() => console.log("next microtask"));
    |};
  [%expect {|
    after require
    program
    next microtask
    |}]

let%expect_test
    ("--disable microtask-start"
     [@when target_engine <> "quickjs" && target_engine <> "bun"]) =
  run
    ~flags:[ "--disable"; "microtask-start" ]
    {| let () = print_endline "program" |}
    {|
      require("./test.js");
      console.log("after require");
    |};
  [%expect {|
    program
    after require
    |}]

let%expect_test
    ("programs with exports start synchronously"
     [@when target_engine <> "quickjs" && target_engine <> "bun"]) =
  run
    {|
      external pure_js_expr : string -> 'a = "caml_pure_js_expr"
      external set : 'a -> 'b -> 'c -> unit = "caml_js_set"
      let () = print_endline "program"
      let () = set (pure_js_expr "jsoo_exports") (pure_js_expr "'x'") 3 |}
    {|
      console.log(require("./test.js").x);
    |};
  [%expect {|
    program
    3
    |}]
