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

(* Split a runtime file written as an ES module into linkable pieces.

   Exports become provided primitives; imports become dependency edges on
   the imported names, so the linker's usual dead code elimination applies:
   only the statements needed for the primitives actually used are linked.

   The module is decomposed at statement granularity:
   - the statement defining an exported name yields a piece providing that
     name;
   - statements defining module-local helpers yield pieces with synthetic
     names (helpers are renamed to fresh variables so they cannot collide
     with other files);
   - statements with side effects are grouped, in source order, into a
     single "initialization" piece that every other piece of the module
     depends on: importing a module evaluates it, so its side effects must
     run whenever any of its exports is used;
   - mutually dependent statements (e.g. mutually recursive functions) are
     merged into a single piece; additional exported names defined by a
     merged piece are provided through zero-code forwarding pieces. *)

open! Stdlib
open Javascript

type piece =
  { exported : bool
  ; name : string
  ; requires : string list
  ; code : statement_list
  ; parse_info : Parse_info.t
  }

let error s = Format.ksprintf (fun s -> failwith s) s

module IntSCC = Strongly_connected_components.Make (struct
  type t = int

  module Map = IntMap
  module Set = IntSet
end)

class subst_idents subst =
  object
    inherit Js_traverse.map

    method! ident i =
      match i with
      | V v -> (
          match Code.Var.Map.find_opt v subst with
          | Some target -> target
          | None -> i)
      | S _ -> i
  end

let s_ident name = Javascript.ident (Utf8_string.of_string_exn name)

(* Substitute import bindings with the imported (global) names. Only named
   imports make sense for runtime files, where linking is name-based. *)
let import_substitutions ~filename (imports : Esm.import_entry list) :
    ident Code.Var.Map.t * StringSet.t =
  List.fold_left imports ~init:(Code.Var.Map.empty, StringSet.empty) ~f:(fun acc import ->
      List.fold_left import.Esm.bindings ~init:acc ~f:(fun (subst, names) binding ->
          match binding with
          | Esm.ImportNamed (orig, V v) ->
              Code.Var.Map.add v (s_ident orig) subst, StringSet.add orig names
          | Esm.ImportNamed (_, S _) -> subst, names
          | Esm.ImportDefault _ ->
              error "%s: default imports are not supported in runtime modules" filename
          | Esm.ImportNamespace _ ->
              error "%s: namespace imports are not supported in runtime modules" filename
          | Esm.ImportSideEffect ->
              error
                "%s: side-effect imports are not supported in runtime modules"
                filename))

(* Substitute exported bindings with their exported (global) names. A value
   exported under several names keeps the first name; the other names are
   provided by forwarding pieces. Returns the substitution and the forwarders
   as (extra name, primary name) pairs. *)
let export_substitutions ~filename (exports : Esm.export_entry StringMap.t) :
    ident Code.Var.Map.t * (string * string) list =
  let groups =
    StringMap.fold
      (fun name (e : Esm.export_entry) acc ->
        (match e.kind with
        | Esm.Export_reexport _ ->
            error "%s: re-exports are not supported in runtime modules" filename
        | Esm.Export_var | Esm.Export_fun | Esm.Export_class -> ());
        if String.equal name "default"
        then error "%s: default exports are not supported in runtime modules" filename;
        match e.local_ident with
        | S _ -> error "%s: exported name %s is not declared in this module" filename name
        | V v ->
            Code.Var.Map.update
              v
              (function
                | None -> Some [ name ]
                | Some l -> Some (name :: l))
              acc)
      exports
      Code.Var.Map.empty
  in
  Code.Var.Map.fold
    (fun v names (subst, forwarders) ->
      match List.sort names ~cmp:String.compare with
      | [] -> assert false
      | primary :: extras ->
          ( Code.Var.Map.add v (s_ident primary) subst
          , List.map extras ~f:(fun extra -> extra, primary) @ forwarders ))
    groups
    (Code.Var.Map.empty, [])

let parse_info_of_stmts ~filename (stmts : statement_list) =
  match stmts with
  | (_, Pi pi) :: _ -> pi
  | (_, (N | U)) :: _ | [] ->
      { Parse_info.zero with Parse_info.name = Some filename; src = Some filename }

let split ~filename (program : program) :
    [ `Pieces of piece list | `No_exports of statement_list ] =
  let resolve spec = Esm.ModuleId.of_path spec in
  let m = Esm.analyze_module ~resolve (Esm.ModuleId.of_path filename) program in
  let import_subst, imported_names = import_substitutions ~filename m.imports in
  if not (List.is_empty m.star_exports)
  then error "%s: [export * from] is not supported in runtime modules" filename;
  if StringMap.is_empty m.exports
  then
    (* A module without exports cannot be pulled in through dependencies:
       treat the whole module as always-included code. *)
    `No_exports ((new subst_idents import_subst)#program m.body)
  else
    let export_subst, forwarders = export_substitutions ~filename m.exports in
    let subst = Code.Var.Map.union (fun _ i _ -> Some i) import_subst export_subst in
    let body = (new subst_idents subst)#program m.body in
    let exported_names =
      StringMap.fold (fun name _ s -> StringSet.add name s) m.exports StringSet.empty
    in
    let stmts = Array.of_list body in
    let infos = Array.map stmts ~f:Esm_tree_shake.statement_info in
    let n = Array.length stmts in
    (* Map each defined binding to the statement defining it *)
    let def_var = ref Code.Var.Map.empty in
    let def_str = ref StringMap.empty in
    Array.iteri infos ~f:(fun i (defines, _, _) ->
        IdentSet.iter
          (fun id ->
            match id with
            | V v -> def_var := Code.Var.Map.add v i !def_var
            | S { name = Utf8 s; _ } -> def_str := StringMap.add s i !def_str)
          defines);
    let def_of id =
      match id with
      | V v -> Code.Var.Map.find_opt v !def_var
      | S { name = Utf8 s; _ } -> StringMap.find_opt s !def_str
    in
    (* Group all side-effecting statements, together with their transitive
       intra-module dependencies, into a virtual "init" node [n]. Merging the
       dependencies avoids a cycle between the init piece and the pieces it
       uses (every piece of the module depends on the init piece); it does
       not reduce dead code elimination since the init piece pulls its
       dependencies in whenever the module is used anyway. It also ensures
       that pieces whose evaluation reads state set up by the init code (e.g.
       [export const x = table.length]) are emitted after it. *)
    let in_init = Array.map infos ~f:(fun (_, _, side_effects) -> side_effects) in
    let rec extend_init () =
      let changed = ref false in
      Array.iteri infos ~f:(fun i (_, uses, _) ->
          if in_init.(i)
          then
            IdentSet.iter
              (fun id ->
                match def_of id with
                | Some j when not in_init.(j) ->
                    in_init.(j) <- true;
                    changed := true
                | Some _ | None -> ())
              uses);
      if !changed then extend_init ()
    in
    extend_init ();
    let node_of i = if in_init.(i) then n else i in
    let has_init = Array.exists in_init ~f:Fun.id in
    (* Dependency graph between nodes *)
    let deps = Array.make (n + 1) IntSet.empty in
    Array.iteri infos ~f:(fun i (_, uses, _) ->
        let src = node_of i in
        IdentSet.iter
          (fun id ->
            match def_of id with
            | Some j when node_of j <> src ->
                deps.(src) <- IntSet.add (node_of j) deps.(src)
            | Some _ | None -> ())
          uses);
    let graph =
      let nodes =
        List.init ~len:n ~f:Fun.id
        |> List.filter ~f:(fun i -> node_of i = i)
        |> fun l -> if has_init then n :: l else l
      in
      List.fold_left nodes ~init:IntMap.empty ~f:(fun g nd -> IntMap.add nd deps.(nd) g)
    in
    let components = IntSCC.connected_components_sorted_from_roots_to_leaf graph in
    (* Statement indices of a component, in source order *)
    let members component =
      let nodes =
        match (component : IntSCC.component) with
        | IntSCC.No_loop id -> [ id ]
        | IntSCC.Has_loop ids -> ids
      in
      List.concat_map nodes ~f:(fun nd ->
          if nd = n
          then List.init ~len:n ~f:Fun.id |> List.filter ~f:(fun i -> in_init.(i))
          else [ nd ])
      |> List.sort ~cmp:compare
    in
    (* First pass: name each component *)
    let synthetic_count = ref 0 in
    let base = Filename.basename filename in
    let named_components =
      Array.map components ~f:(fun component ->
          let idxs = members component in
          let exported =
            List.concat_map idxs ~f:(fun i ->
                let defines, _, _ = infos.(i) in
                IdentSet.fold
                  (fun id acc ->
                    match id with
                    | S { name = Utf8 s; _ } when StringSet.mem s exported_names ->
                        s :: acc
                    | S _ | V _ -> acc)
                  defines
                  [])
            |> List.sort ~cmp:String.compare
          in
          let name, extra_names =
            match exported with
            | name :: extras -> name, extras
            | [] ->
                incr synthetic_count;
                Printf.sprintf "%s.%d" base !synthetic_count, []
          in
          component, idxs, name, extra_names)
    in
    (* Node -> piece name *)
    let piece_of_node =
      Array.fold_left
        named_components
        ~init:IntMap.empty
        ~f:(fun acc (component, _, name, _) ->
          let nodes =
            match (component : IntSCC.component) with
            | IntSCC.No_loop id -> [ id ]
            | IntSCC.Has_loop ids -> ids
          in
          List.fold_left nodes ~init:acc ~f:(fun acc nd -> IntMap.add nd name acc))
    in
    let init_name = if has_init then IntMap.find_opt n piece_of_node else None in
    (* Second pass: build the pieces *)
    let pieces =
      Array.to_list named_components
      |> List.map ~f:(fun (_, idxs, name, extra_names) ->
          let code = List.map idxs ~f:(fun i -> stmts.(i)) in
          let requires =
            List.fold_left idxs ~init:StringSet.empty ~f:(fun acc i ->
                let _, uses, _ = infos.(i) in
                IdentSet.fold
                  (fun id acc ->
                    match def_of id with
                    | Some j ->
                        let dep = IntMap.find (node_of j) piece_of_node in
                        if String.equal dep name then acc else StringSet.add dep acc
                    | None -> (
                        match id with
                        | S { name = Utf8 s; _ } when StringSet.mem s imported_names ->
                            StringSet.add s acc
                        | S _ | V _ -> acc))
                  uses
                  acc)
          in
          let requires =
            match init_name with
            | Some init when not (String.equal init name) -> StringSet.add init requires
            | Some _ | None -> requires
          in
          { exported = StringSet.mem name exported_names
          ; name
          ; requires = StringSet.elements requires
          ; code
          ; parse_info = parse_info_of_stmts ~filename code
          }
          :: List.map extra_names ~f:(fun extra ->
              { exported = true
              ; name = extra
              ; requires = [ name ]
              ; code = []
              ; parse_info = parse_info_of_stmts ~filename code
              }))
      |> List.concat
    in
    let pieces =
      pieces
      @ List.map forwarders ~f:(fun (extra, primary) ->
          (* A value exported under several names: define the extra names
               as aliases of the primary one, since generated code references
               each primitive by its own name. *)
          { exported = true
          ; name = extra
          ; requires = [ primary ]
          ; code =
              [ ( Variable_statement
                    (Var, [ DeclIdent (s_ident extra, Some (EVar (s_ident primary), N)) ])
                , N )
              ]
          ; parse_info = { Parse_info.zero with Parse_info.name = Some filename }
          })
    in
    `Pieces pieces
