(* Js_of_ocaml tests
 * http://www.ocsigen.org/js_of_ocaml/
 * Copyright (C) 2023 Micah Cantor
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

let to_js_file js_prog =
  js_prog |> Filetype.js_text_of_string |> Filetype.write_js ~name:"test.js"

let%expect_test "if statement to conditional expression" =
  with_temp_dir ~f:(fun () ->
      let js_file = to_js_file "if (e1) e2; else e3;" in
      print_endline (Util.optimize_space js_file);
      [%expect {|
        e1 ? e2 : e3;
        //end|}])

let%expect_test "one branch negated if to || expression" =
  with_temp_dir ~f:(fun () ->
      let js_file = to_js_file "if (!e1) e2;" in
      print_endline (Util.optimize_space js_file);
      [%expect {|
        e1 || e2;
        //end|}])

let%expect_test "one branch if to && expression statement" =
  with_temp_dir ~f:(fun () ->
      let js_file = to_js_file "if (e1) e2;" in
      print_endline (Util.optimize_space js_file);
      [%expect {|
        e1 && e2;
        //end|}])

let%expect_test "expression statement; var declaration" =
  with_temp_dir ~f:(fun () ->
      let js_file = to_js_file "e1; var x = e2;" in
      print_endline (Util.optimize_space js_file);
      [%expect {|
        var x = (e1, e2);
        //end|}])

let%expect_test "expression statement; return statement" =
  with_temp_dir ~f:(fun () ->
      let js_file = to_js_file "e1; return e2;" in
      print_endline (Util.optimize_space js_file);
      [%expect {|
        return e1, e2;
        //end|}])

let%expect_test "expression statement; if statement" =
  with_temp_dir ~f:(fun () ->
      let js_file = to_js_file "e1; if (e2) 0; else 1;" in
      print_endline (Util.optimize_space js_file);
      [%expect {|
        e1, e2 ? 0 : 1;
        //end|}])

let%expect_test "expression statement; if statement with returns" =
  with_temp_dir ~f:(fun () ->
      let js_file = to_js_file "e1; if (e2) return 0; else return 1;" in
      print_endline (Util.optimize_space js_file);
      [%expect {|
        return e1, e2 ? 0 : 1;
        //end|}])

let%expect_test "expression statement; if statement without else" =
  with_temp_dir ~f:(fun () ->
      let js_file = to_js_file "e1; if (e2) 0;" in
      print_endline (Util.optimize_space js_file);
      [%expect {|
        e1, e2 && 0;
        //end|}])

let%expect_test "expression statement; if statement without else with return" =
  with_temp_dir ~f:(fun () ->
      let js_file = to_js_file "e1; if (e2) return 0;" in
      print_endline (Util.optimize_space js_file);
      [%expect {|
        if(e1, e2) return 0;
        //end|}])

let%expect_test "if statement without else; return statement" =
  with_temp_dir ~f:(fun () ->
      let js_file = to_js_file "if (e1) e2; return e3;" in
      print_endline (Util.optimize_space js_file);
      [%expect {|
        return e1 && e2, e3;
        //end|}])

let%expect_test "expression statement; expression statement" =
  with_temp_dir ~f:(fun () ->
      let js_file = to_js_file "e1; e2;" in
      print_endline (Util.optimize_space js_file);
      [%expect {|
        e1, e2;
        //end|}])

let%expect_test "if statement; if statement; return statement" =
  with_temp_dir ~f:(fun () ->
      let js_file = to_js_file "if (e1) e2; if (e3) e4; else e5; return e6;" in
      print_endline (Util.optimize_space js_file);
      [%expect {|
      return e1 && e2, e3 ? e4 : e5, e6;
      //end|}])

let%expect_test "if statement; if statement; var statement" =
  with_temp_dir ~f:(fun () ->
      let js_file = to_js_file "if (e1) e2; if (e3) e4; else e5; var x = e6;" in
      print_endline (Util.optimize_space js_file);
      [%expect {|
        var x = (e1 && e2, e3 ? e4 : e5, e6);
        //end|}])

let%expect_test "\"use strict\"; expression statement" =
  with_temp_dir ~f:(fun () ->
      let js_file = to_js_file "\"use strict\"; e2;" in
      print_endline (Util.optimize_space js_file);
      [%expect {|
        "use strict"; e2;
        //end|}])
