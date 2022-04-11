(* Js_of_ocaml compiler
 * http://www.ocsigen.org/js_of_ocaml/
 * Copyright (C) 2016 Hugo Heuzard
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

let output = ref None

let usage () =
  Format.eprintf "Usage: jsoo_listunits [options] [findlib packages|*.cmi|*.cma] @.";
  Format.eprintf " -verbose@.";
  Format.eprintf " -help\t\t\tDisplay usage@.";
  Format.eprintf " -o [name]\t\tSet output filename@.";
  exit 1

let rec scan_args acc = function
  | ("--verbose" | "-verbose") :: xs ->
      Jsoo_common.verbose := true;
      scan_args acc xs
  | "-o" :: name :: xs ->
      output := Some name;
      scan_args acc xs
  | ("--help" | "-help" | "-h") :: _ -> usage ()
  | x :: xs -> scan_args (x :: acc) xs
  | [] -> List.rev acc

let () =
  let args = List.tl (Array.to_list Sys.argv) in
  let args = scan_args [] args in
  let all = Jsoo_common.cmis args in
  let oc =
    match !output with
    | Some x -> open_out x
    | None -> failwith "-o <name> needed"
  in
  let to_unit s =
    Js_of_ocaml_compiler.Stdlib.String.capitalize_ascii
      (Filename.basename (Filename.chop_suffix s ".cmi"))
  in
  List.iter
    (fun c ->
      output_string oc (to_unit c);
      output_string oc "\n")
    all;
  close_out oc
