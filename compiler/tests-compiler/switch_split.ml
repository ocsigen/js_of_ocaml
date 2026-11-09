(* Js_of_ocaml compiler
 * http://www.ocsigen.org/js_of_ocaml/
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

open Js_of_ocaml_compiler

(* Large switches are split into smaller ones (see [DTree.build_switch]):
   when they have more than [switch_size] cases (256 by default), or when
   their case values span more than 1000, since the JavaScriptCore
   interpreter only uses a jump table below that. *)

let switch_sizes ?flags cases =
  let prog =
    Printf.sprintf
      "let f x = match x with\n%s\n  | _ -> 0\n"
      (String.concat
         "\n"
         (List.map (fun k -> Printf.sprintf "  | %d -> %d" k ((k * 7) + 1)) cases))
  in
  let program = Util.compile_and_parse ?flags prog in
  let sizes = ref [] in
  let o =
    object
      inherit Js_traverse.iter as super

      method! statement s =
        (match s with
        | Javascript.Switch_statement (_, l, _, l') ->
            sizes := (List.length l + List.length l') :: !sizes
        | _ -> ());
        super#statement s
    end
  in
  o#program program;
  print_endline (String.concat " " (List.rev_map string_of_int !sizes))

let range ?(step = 1) n = List.init n (fun i -> i * step)

let%expect_test "dense switch below the limit" =
  switch_sizes (range 200);
  [%expect {| 199 |}]

let%expect_test "dense switch above the limit" =
  switch_sizes (range 300);
  [%expect {| 149 149 |}]

let%expect_test "switch_size parameter" =
  switch_sizes ~flags:[ "--set"; "switch_size=60" ] (range 100);
  [%expect {| 49 49 |}]

let%expect_test "sparse switch spanning more than 1000" =
  switch_sizes (range ~step:5 221);
  [%expect {| 110 111 |}]
