(* Js_of_ocaml tests
 * http://www.ocsigen.org/js_of_ocaml/
 * Copyright (C) 2026 Hugo Heuzard
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

(* https://github.com/ocsigen/js_of_ocaml/issues/2285 *)

open Util

(* The [if (e) var x = e; else var x = e2 --> var x = e || e2] fusion must not
   fire when the condition has side effects: without debug locations, the
   condition [g ()] and the true-branch [g ()] are structurally equal, and the
   fusion collapses two calls into one. [g] must be called twice below. *)
let%expect_test "if-fusion must not drop an effectful call" =
  compile_and_run
    ~debug:false
    {|
    let count = ref 0

    let g () =
      incr count;
      !count < 10

    let h () =
      count := !count + 100;
      false

    let () =
      let a = g () in
      let x = if a then g () else h () in
      print_int !count;
      print_string (if x then "T" else "F");
      print_newline ()
    |};
  [%expect {| 2T |}]

(* Folding [if (0) { var y; }] must keep the (hoisted) declaration of [y];
   dropping it makes the later [y = 1] an assignment to an undeclared
   variable, a ReferenceError in strict mode. *)
let%expect_test "constant-if folding must not drop hoisted var declarations" =
  with_temp_dir ~f:(fun () ->
      let js_prog =
        {|
"use strict";
if (0) { var y; } else { var z; }
try {
    y = 1;
    z = 2;
    console.log("y =", y, "z =", z);
} catch (e) {
    console.log("caught:", e.name);
}
|}
      in
      let js_file =
        js_prog |> Filetype.js_text_of_string |> Filetype.write_js ~name:"test.js"
      in
      let js_min_file = js_file |> jsoo_minify ~pretty:false in
      print_file (Filetype.path_of_js_file js_min_file);
      run_javascript js_file |> print_endline;
      run_javascript js_min_file |> print_endline;
      [%expect
        {|
        $ cat "test.min.js"
          1: "use strict";var
          2: z,y;try{y=1;z=2;console.log("y =",y,"z =",z)}catch(f){console.log("caught:",f.name)}
        y = 1 z = 2

        y = 1 z = 2
        |}])

(* Uses occurring in catch-parameter destructuring defaults must be recorded
   by the free-variable analysis: [captured] is free in [inner], and the
   short-name allocator must not reuse its name for [inner]'s local. Both runs
   must print GOOD. *)
let%expect_test "catch destructuring defaults are uses for shortvar" =
  with_temp_dir ~f:(fun () ->
      let js_prog =
        {|
function outer() {
    var captured = "GOOD";
    function inner() {
        var local = "BAD";
        try {
            throw {};
        } catch ({ p = captured }) {
            return p;
        }
    }
    return inner();
}
console.log(outer());
|}
      in
      let js_file =
        js_prog |> Filetype.js_text_of_string |> Filetype.write_js ~name:"test.js"
      in
      let js_min_file =
        js_file |> jsoo_minify ~flags:[ "--enable"; "shortvar" ] ~pretty:false
      in
      print_file (Filetype.path_of_js_file js_min_file);
      run_javascript js_file |> print_endline;
      run_javascript js_min_file |> print_endline;
      [%expect
        {|
        $ cat "test.min.js"
          1: function
          2: outer(){var
          3: a="GOOD";function
          4: b(){var
          5: b="BAD";try{throw{}}catch({p:f=a}){return f}}return b()}console.log(outer());
        GOOD

        GOOD
        |}])
