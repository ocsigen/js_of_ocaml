(* Js_of_ocaml library
 * http://www.ocsigen.org/js_of_ocaml/
 * Copyright (C) 2013 Jacques-Pascal Deplaix
 * Laboratoire PPS - CNRS Université Paris Diderot
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

open Ocamlbuild_plugin
module Pack = Ocamlbuild_pack

let fold f =
  let l = ref [] in
  (try while true do l @:= [f ()] done with _ -> ());
  !l

let split_comma = Str.split_delim (Str.regexp ",")

let fold_pflag scan =
  List.fold_left
    (fun acc x -> try split_comma (scan x (fun x -> x)) @ acc with _ -> acc)
    []

let ocamlfind cmd f =
  let p = Printf.sprintf in
  let cmd = List.map (p "\"%s\"") cmd in
  let cmd = p "ocamlfind query %s" (String.concat " " cmd) in
  Pack.My_unix.run_and_open cmd (fun ic -> fold (fun () -> f ic))

let link_opts prod =
  let (all_pkgs, predicates) =
    let tags = Tags.elements (tags_of_pathname prod) in
    let pkgs = fold_pflag (fun x -> Scanf.sscanf x "package(%[^)])") tags in
    let predicates = fold_pflag (fun x -> Scanf.sscanf x "predicate(%[^)])") tags in
    ("js_of_ocaml" :: pkgs, predicates)
  in

  (* Findlib usualy set pkg_* predicate for all selected packages *)
  (* It doesn't do it with 'query' command, we have to it manualy. *)
  let cmd = "-format" :: "pkg_%p" :: "-r" :: all_pkgs in
  let predicates_pkgs = ocamlfind cmd (fun ic -> input_line ic) in

  let all_predicates = String.concat "," ("javascript" :: predicates @ predicates_pkgs) in

  (* query findlib for linking option *)
  let cmd = "-o-format" :: "-r" :: "-predicates" :: all_predicates :: all_pkgs in
  ocamlfind cmd (fun ic -> A (input_line ic))

let init () =
  let dep = "%.byte" in
  let prod = "%.js" in
  let f env _ =
    let dep = env dep in
    let prod = env prod in
    let link_opts = link_opts prod in
    let tags = tags_of_pathname prod ++ "js_of_ocaml" in
    Cmd (S [A "js_of_ocaml"; A "-noruntime"; T tags; S link_opts; P dep; A "-o"; Px prod])
  in
  rule "js_of_ocaml: .byte -> .js" ~dep ~prod f;
  flag ["js_of_ocaml"; "debug"] (S [A "-pretty"; A "-debuginfo"; A "-sourcemap"]);
  flag ["js_of_ocaml"; "pretty"] (A "-pretty");
  flag ["js_of_ocaml"; "debuginfo"] (A "-debuginfo");
  flag ["js_of_ocaml"; "noinline"] (A "-noinline");
  flag ["js_of_ocaml"; "sourcemap"] (A "-sourcemap");
  pflag ["js_of_ocaml"] "tailcall" (fun x -> S [A "-tc"; A x]);
  pflag ["js_of_ocaml"] "opt" (fun n -> S [A "-opt"; A n]);
  pflag ["js_of_ocaml"] "global" (fun n -> S [A "-with-global"; A n])

let oasis_support ~executables =
  let aux x =
    if List.mem x executables then
      Pathname.update_extension "js" x
    else
      x
  in
  Options.targets := List.map aux !Options.targets

let dispatcher ?(oasis_executables=[]) = function
  | After_rules -> init ()
  | After_options -> oasis_support ~executables:oasis_executables
  | _ -> ()
