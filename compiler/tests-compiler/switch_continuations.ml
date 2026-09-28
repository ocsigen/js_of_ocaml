(* Js_of_ocaml tests
 * http://www.ocsigen.org/js_of_ocaml/
 * Copyright (C) 2026 Robin Ricard
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
open Js_of_ocaml_compiler.Stdlib
open Util

(* [Specialize.switches] runs first in the driver, when all the continuations
   to a block carry the same arguments. Running it after dead code elimination,
   which bypasses the empty case blocks, checks that it does not assume so. *)

let parse_cmo file =
  let ic = open_in_bin (Filetype.path_of_cmo_file file) in
  Fun.protect
    ~finally:(fun () -> close_in ic)
    (fun () ->
      match Parse_bytecode.from_channel ic with
      | `Cmo unit -> (Parse_bytecode.from_cmo unit ic).code
      | _ -> assert false)

(* Print the switches of the program, showing each argument as the constant it
   is bound to. *)
let print_switches (p : Code.program) =
  let constants = Code.Var.Hashtbl.create 17 in
  Code.Addr.Map.iter
    (fun _ (block : Code.block) ->
      List.iter block.body ~f:(function
        | Code.Let (x, Constant c) -> Code.Var.Hashtbl.replace constants x c
        | _ -> ()))
    p.blocks;
  let arg x =
    match Code.Var.Hashtbl.find_opt constants x with
    | Some c -> Format.asprintf "%a" Code.Print.constant c
    | None -> "?"
  in
  let cont (_, args) = String.concat ~sep:", " (List.map args ~f:arg) in
  Code.Addr.Map.iter
    (fun _ (block : Code.block) ->
      match block.branch with
      | Switch (_, l) when Array.exists l ~f:(fun (_, args) -> not (List.is_empty args))
        -> Array.iteri l ~f:(fun i c -> Printf.printf "case %d: %s\n" i (cont c))
      | Branch c when not (List.is_empty (snd c)) -> Printf.printf "branch: %s\n" (cont c)
      | _ -> ())
    p.blocks

let%expect_test "switch specialization keeps continuation arguments" =
  with_temp_dir ~f:(fun () ->
      Config.set_target `JavaScript;
      Config.set_effects_backend `Disabled;
      let p =
        Filetype.ocaml_text_of_string
          {ocaml|
type choice = A | B | C | D
let f choice =
  let value = match choice with A -> "a" | B -> "b" | C -> "c" | D -> "d" in
  "[" ^ value ^ "]"
|ocaml}
        |> Filetype.write_ocaml ~name:"test.ml"
        |> compile_ocaml_to_cmo
        |> parse_cmo
      in
      let p, _ = Deadcode.f (Pure_fun.f p) p in
      print_switches p;
      [%expect
        {|
        case 0: "a"
        case 1: "b"
        case 2: "c"
        case 3: "d"
        |}];
      print_switches (Specialize.switches p);
      [%expect
        {|
        case 0: "a"
        case 1: "b"
        case 2: "c"
        case 3: "d"
        |}])
