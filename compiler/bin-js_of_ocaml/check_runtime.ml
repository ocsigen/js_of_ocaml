(* Js_of_ocaml compiler
 * http://www.ocsigen.org/js_of_ocaml/
 * Copyright (C) 2021 Hugo Heuzard
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

open! Js_of_ocaml_compiler.Stdlib
open Js_of_ocaml_compiler

let group_by_snd l =
  l
  |> List.sort_uniq ~compare:(fun (n1, l1) (n2, l2) ->
         match Poly.compare l1 l2 with
         | 0 -> String.compare n1 n2
         | c -> c)
  |> List.group ~f:(fun (_, g1) (_, g2) -> Poly.equal g1 g2)

let print_groups output l =
  List.iter l ~f:(fun group ->
      match group with
      | [] -> assert false
      | (_, loc) :: _ ->
          (match loc with
          | [] -> ()
          | loc ->
              output_string
                output
                (Printf.sprintf "\nFrom %s:\n" (String.concat ~sep:"," loc)));
          List.iter group ~f:(fun (name, _) ->
              output_string output (Printf.sprintf "%s\n" name)))

let f (runtime_files, bytecode, target_env) =
  Generate.init ();
  let runtime_files, builtin =
    List.partition_map runtime_files ~f:(fun name ->
        match Builtins.find name with
        | Some t -> `Snd t
        | None -> `Fst name)
  in
  let builtin =
    if false then builtin else Js_of_ocaml_compiler_runtime_files.runtime @ builtin
  in
  List.iter builtin ~f:(fun t ->
      let filename = Builtins.File.name t in
      let runtimes = Linker.Fragment.parse_builtin t in
      Linker.load_fragments ~target_env ~filename runtimes);
  Linker.load_files ~target_env runtime_files;
  Linker.check_deps ();
  let all_prims =
    List.concat_map bytecode ~f:(fun f ->
        let ic = open_in_bin f in
        let prims =
          match Parse_bytecode.from_channel ic with
          | `Cmo x -> x.Cmo_format.cu_primitives
          | `Cma x ->
              List.concat_map
                ~f:(fun x -> x.Cmo_format.cu_primitives)
                x.Cmo_format.lib_units
          | `Exe ->
              let toc = Parse_bytecode.Toc.read ic in
              Parse_bytecode.read_primitives toc ic
        in
        close_in ic;
        List.map ~f:(fun p -> p, f) prims)
  in
  let _percent_prim, needed =
    List.partition all_prims ~f:(fun (x, _) -> Char.equal (String.get x 0) '%')
  in
  let origin =
    List.fold_left
      ~f:(fun acc (x, y) ->
        let l = try StringMap.find x acc with Not_found -> [] in
        StringMap.add x (y :: l) acc)
      ~init:StringMap.empty
      needed
  in
  let needed = StringSet.of_list (List.map ~f:fst needed) in
  let from_runtime1 = Linker.get_provided () in
  let from_runtime2 = Primitive.get_external () in
  (* [from_runtime2] is a superset of [from_runtime1].
     Extra primitives are registered on the ocaml side (e.g. generate.ml) *)
  assert (StringSet.is_empty (StringSet.diff from_runtime1 from_runtime2));
  let missing' = StringSet.diff needed from_runtime1 in
  let all_used, missing =
    let state = Linker.init () in
    let state, missing = Linker.resolve_deps state needed in
    StringSet.of_list (Linker.all state), missing
  in
  assert (StringSet.equal missing missing');
  let extra =
    StringSet.diff from_runtime1 all_used
    |> StringSet.elements
    |> List.map ~f:(fun name ->
           ( name
           , match Linker.origin ~name with
             | None -> []
             | Some x -> [ x ] ))
    |> group_by_snd
  in

  let missing_for_real =
    StringSet.diff missing from_runtime2
    |> StringSet.elements
    |> List.map ~f:(fun x -> x, StringMap.find x origin)
    |> group_by_snd
  in

  let output = stdout in
  set_binary_mode_out output true;
  output_string output "Missing\n";
  output_string output "-------\n";
  print_groups output missing_for_real;
  output_string output "\n";
  output_string output "Unused\n";
  output_string output "-------\n";
  print_groups output extra;
  output_string output "\n";
  ()

let options =
  let open Cmdliner in
  (* TODO: add flags to only display missing or extra primitives *)
  let files =
    let doc = "Bytecode and JavaScript files [$(docv)]. " in
    Arg.(value & pos_all string [] & info [] ~docv:"FILES" ~doc)
  in
  let build_t files target_env =
    let runtime_files, bc_files =
      List.partition files ~f:(fun file -> Filename.check_suffix file ".js")
    in
    `Ok (runtime_files, bc_files, target_env)
  in
  let target_env =
    let doc = "Runtime compile target." in
    let options = List.map ~f:(fun env -> Target_env.to_string env, env) Target_env.all in
    let docv = Printf.sprintf "{%s}" (String.concat ~sep:"," (List.map ~f:fst options)) in
    Arg.(
      value & opt (enum options) Target_env.Isomorphic & info [ "target-env" ] ~docv ~doc)
  in
  let t = Term.(const build_t $ files $ target_env) in
  Term.ret t

let info =
  Info.make
    ~name:"check-runtime"
    ~doc:"Check runtime"
    ~description:"js_of_ocaml-check-runtime checks the runtime."

let command =
  let t = Cmdliner.Term.(const f $ options) in
  Cmdliner.Cmd.v info t
