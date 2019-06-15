(* Js_of_ocaml compiler
 * http://www.ocsigen.org/js_of_ocaml/
 * Copyright (C) 2017 Hugo Heuzard
 * Copyright (C) 2019 Ty Overby
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

(* https://github.com/ocsigen/js_of_ocaml/issues/507 *)
(* https://github.com/ocsigen/js_of_ocaml/commit/e2f465dd1ac03da706ae086da37794184db21d31 *)

let%expect_test _ =
  Util.compile_and_run
    {|
    let _ =
      let r = ref 0.0 in
      for _ = 1 to 100 do
        r := !r -. (-1.0 *. !r)
      done;
      ();
    print_endline "Success!"
  |};
  [%expect {| Success! |}]
