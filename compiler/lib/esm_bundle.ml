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

open! Stdlib
open Javascript

(* Build a substitution map from import identifiers to their resolved source identifiers.
   This allows us to directly replace uses of import bindings with the actual source
   variables, avoiding intermediate const bindings. *)
let build_import_substitutions
    (m : Esm.esm_module)
    (all_modules : Esm.esm_module Esm.ModuleId.Map.t) : ident Code.Var.Map.t =
  List.fold_left m.imports ~init:Code.Var.Map.empty ~f:(fun acc import ->
      let source_module =
        match Esm.ModuleId.Map.find_opt import.Esm.source all_modules with
        | Some m -> m
        | None -> failwith ("Module not found: " ^ Esm.ModuleId.to_path import.Esm.source)
      in
      List.fold_left import.Esm.bindings ~init:acc ~f:(fun acc binding ->
          match binding with
          | Esm.ImportSideEffect -> acc
          | Esm.ImportDefault local_id -> (
              let export =
                match StringMap.find_opt "default" source_module.exports with
                | Some e -> e
                | None ->
                    failwith
                      ("No default export in " ^ Esm.ModuleId.to_path import.Esm.source)
              in
              let source_ident = Esm.resolve_reexport all_modules export in
              match local_id with
              | V v -> Code.Var.Map.add v source_ident acc
              | S _ -> acc)
          | Esm.ImportNamed (orig_name, local_id) -> (
              let export =
                match StringMap.find_opt orig_name source_module.exports with
                | Some e -> e
                | None ->
                    failwith
                      (Printf.sprintf
                         "No export '%s' in %s"
                         orig_name
                         (Esm.ModuleId.to_path import.Esm.source))
              in
              let source_ident = Esm.resolve_reexport all_modules export in
              match local_id with
              | V v -> Code.Var.Map.add v source_ident acc
              | S _ -> acc)
          | Esm.ImportNamespace _ ->
              (* Namespace imports still need object creation, handled separately *)
              acc))

(* ========== Namespace Import Optimization ========== *)

(* Analyze namespace usage to find which fields are accessed.
   Returns None if the namespace is used in a way that prevents optimization
   (e.g., passed to a function, used as a value itself).
   Returns Some field_set if only static field accesses are found. *)

type namespace_usage =
  | Only_fields of StringSet.t (* Only static field accesses *)
  | Cannot_optimize (* Namespace used in non-field-access way *)

class collect_namespace_accesses (namespace_vars : Code.Var.Set.t) =
  object (self)
    inherit Js_traverse.iter as super

    val mutable usage : namespace_usage Code.Var.Map.t = Code.Var.Map.empty

    method get_usage = usage

    method private mark_cannot_optimize v =
      usage <- Code.Var.Map.add v Cannot_optimize usage

    method private add_field_access v field =
      let current =
        match Code.Var.Map.find_opt v usage with
        | None -> Only_fields StringSet.empty
        | Some x -> x
      in
      match current with
      | Cannot_optimize -> ()
      | Only_fields fields ->
          usage <- Code.Var.Map.add v (Only_fields (StringSet.add field fields)) usage

    method! expression e =
      match e with
      (* ns.field - static field access via dot notation *)
      | EDot (EVar (V v), _, Utf8 field) when Code.Var.Set.mem v namespace_vars ->
          self#add_field_access v field
      (* ns["field"] - static field access via bracket notation with string literal *)
      | EAccess (EVar (V v), _, EStr (Utf8 field)) when Code.Var.Set.mem v namespace_vars
        -> self#add_field_access v field
      (* ns[expr] - dynamic access, cannot optimize *)
      | EAccess (EVar (V v), _, _) when Code.Var.Set.mem v namespace_vars ->
          self#mark_cannot_optimize v
      (* EVar ns - namespace used as a value (not a field access) *)
      | EVar (V v) when Code.Var.Set.mem v namespace_vars -> self#mark_cannot_optimize v
      | _ -> super#expression e
  end

(* Transform namespace imports into named imports based on field usage analysis.
   Returns updated module with transformed imports and body. *)
let optimize_namespace_imports (m : Esm.esm_module) : Esm.esm_module =
  (* Collect all namespace import identifiers *)
  let namespace_vars =
    List.fold_left m.imports ~init:Code.Var.Map.empty ~f:(fun acc import ->
        List.fold_left import.Esm.bindings ~init:acc ~f:(fun acc binding ->
            match binding with
            | Esm.ImportNamespace (V v) -> Code.Var.Map.add v import.Esm.source acc
            | Esm.ImportNamespace (S _)
            | Esm.ImportNamed _ | Esm.ImportDefault _ | Esm.ImportSideEffect -> acc))
  in
  if Code.Var.Map.is_empty namespace_vars
  then m
  else
    let namespace_var_set =
      Code.Var.Map.fold
        (fun v _ acc -> Code.Var.Set.add v acc)
        namespace_vars
        Code.Var.Set.empty
    in
    (* Analyze field accesses *)
    let collector = new collect_namespace_accesses namespace_var_set in
    collector#program m.body;
    let usage = collector#get_usage in
    (* Build a map from (namespace_var, field) -> fresh_ident for optimizable namespaces *)
    let field_idents : ident StringMap.t Code.Var.Map.t =
      Code.Var.Map.fold
        (fun v _source acc ->
          match Code.Var.Map.find_opt v usage with
          | Some (Only_fields fields) ->
              let field_map =
                StringSet.fold
                  (fun field fmap ->
                    let fresh_id = V (Code.Var.fresh_n field) in
                    StringMap.add field fresh_id fmap)
                  fields
                  StringMap.empty
              in
              Code.Var.Map.add v field_map acc
          | Some Cannot_optimize | None -> acc)
        namespace_vars
        Code.Var.Map.empty
    in
    (* Transform imports: replace optimizable ImportNamespace with ImportNamed *)
    let imports =
      List.map m.imports ~f:(fun import ->
          let bindings =
            List.concat_map import.Esm.bindings ~f:(fun binding ->
                match binding with
                | Esm.ImportNamespace (V v) -> (
                    match Code.Var.Map.find_opt v field_idents with
                    | Some field_map ->
                        (* Replace with named imports *)
                        StringMap.fold
                          (fun field local_id acc ->
                            Esm.ImportNamed (field, local_id) :: acc)
                          field_map
                          []
                    | None ->
                        (* Keep as namespace import *)
                        [ binding ])
                | Esm.ImportNamespace (S _)
                | Esm.ImportNamed _ | Esm.ImportDefault _ | Esm.ImportSideEffect ->
                    [ binding ])
          in
          { import with bindings })
    in
    (* Transform body: replace ns.field with the fresh identifier *)
    let body =
      let replacer =
        object
          inherit Js_traverse.map as super

          method! expression e =
            match e with
            | EDot (EVar (V v), _, Utf8 field) -> (
                match Code.Var.Map.find_opt v field_idents with
                | Some field_map -> (
                    match StringMap.find_opt field field_map with
                    | Some local_id -> EVar local_id
                    | None -> super#expression e)
                | None -> super#expression e)
            | EAccess (EVar (V v), _, EStr (Utf8 field)) -> (
                match Code.Var.Map.find_opt v field_idents with
                | Some field_map -> (
                    match StringMap.find_opt field field_map with
                    | Some local_id -> EVar local_id
                    | None -> super#expression e)
                | None -> super#expression e)
            | _ -> super#expression e
        end
      in
      replacer#program m.body
    in
    { m with imports; body }

(* Generate bindings only for namespace imports, which require object creation *)
let generate_namespace_bindings
    (m : Esm.esm_module)
    (all_modules : Esm.esm_module Esm.ModuleId.Map.t) : statement_list =
  List.concat_map m.imports ~f:(fun import ->
      let source_module =
        match Esm.ModuleId.Map.find_opt import.Esm.source all_modules with
        | Some m -> m
        | None -> failwith ("Module not found: " ^ Esm.ModuleId.to_path import.Esm.source)
      in
      List.filter_map import.Esm.bindings ~f:(fun binding ->
          match binding with
          | Esm.ImportNamespace local_id ->
              (* Create object with all exports *)
              let props =
                StringMap.fold
                  (fun name export acc ->
                    let source_ident = Esm.resolve_reexport all_modules export in
                    let name_utf8 = Utf8_string.of_string_exn name in
                    let pn = if is_ident name then PNI name_utf8 else PNS name_utf8 in
                    Property (pn, EVar source_ident) :: acc)
                  source_module.exports
                  []
              in
              Some
                ( Variable_statement
                    (Const, [ DeclIdent (local_id, Some (EObj props, N)) ])
                , N )
          | Esm.ImportNamed _ | Esm.ImportDefault _ | Esm.ImportSideEffect -> None))

(* Substitution traversal: replace import identifiers with their source identifiers *)
class substitute_imports (subst : ident Code.Var.Map.t) =
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

let apply_import_substitutions subst stmts =
  if Code.Var.Map.is_empty subst
  then stmts
  else (new substitute_imports subst)#program stmts

let bundle (graph : Esm.module_graph) ~(entry_points : Esm.ModuleId.t list) : program =
  let sorted = Esm.topological_sort graph in
  let body_stmts =
    List.concat_map sorted ~f:(fun id ->
        match Esm.ModuleId.Map.find_opt id graph.modules with
        | None -> []
        | Some m ->
            (* Optimize namespace imports: convert `ns.field` to named imports *)
            let m = optimize_namespace_imports m in
            (* Build substitution map and apply to body *)
            let subst = build_import_substitutions m graph.modules in
            let body = apply_import_substitutions subst m.body in
            (* Generate namespace bindings (only for non-optimized namespaces) *)
            let namespace_stmts = generate_namespace_bindings m graph.modules in
            namespace_stmts @ body)
  in
  (* Generate export statements for entry point modules *)
  let export_stmts =
    List.concat_map entry_points ~f:(fun entry_id ->
        match Esm.ModuleId.Map.find_opt entry_id graph.modules with
        | None -> []
        | Some m ->
            StringMap.fold
              (fun _export_name export acc ->
                (* Get the source identifier - resolve re-exports *)
                let source_ident = Esm.resolve_reexport graph.modules export in
                let exported_name = Utf8_string.of_string_exn export.exported_name in
                (Export (ExportNames [ source_ident, exported_name ], Parse_info.zero), N)
                :: acc)
              m.exports
              [])
  in
  body_stmts @ export_stmts

(* ========== Convenience Function ========== *)

let bundle_modules ~parse ~resolve ~entry_points ~tree_shake:do_tree_shake : program =
  let graph = Esm.build_graph ~parse ~resolve ~entry_points in
  let entry_ids = List.map entry_points ~f:Esm.ModuleId.of_path in
  let graph =
    if do_tree_shake
    then
      let entry_exports =
        List.fold_left entry_ids ~init:Esm.ModuleId.Map.empty ~f:(fun acc id ->
            Esm.ModuleId.Map.add id StringSet.empty acc)
      in
      Esm_tree_shake.run graph ~entry_exports
    else graph
  in
  bundle graph ~entry_points:entry_ids

(* ========== Module Merging ========== *)

(* Convert an Esm.import_entry back to a Javascript.Import statement *)
let generate_import_statement (import : Esm.import_entry) : statement =
  let from = Utf8_string.of_string_exn (Esm.ModuleId.to_path import.Esm.source) in
  (* Collect bindings by type *)
  let default_opt, named, namespace_opt, has_side_effect =
    List.fold_left
      import.Esm.bindings
      ~init:(None, [], None, false)
      ~f:(fun (def, named, ns, side) binding ->
        match binding with
        | Esm.ImportDefault id -> Some id, named, ns, side
        | Esm.ImportNamed (orig_name, local_id) ->
            def, (Utf8_string.of_string_exn orig_name, local_id) :: named, ns, side
        | Esm.ImportNamespace id -> def, named, Some id, side
        | Esm.ImportSideEffect -> def, named, ns, true)
  in
  let kind =
    match default_opt, List.rev named, namespace_opt, has_side_effect with
    | None, [], None, true -> SideEffect
    | Some id, [], None, _ -> Default id
    | def_opt, named, None, _ when not (List.is_empty named) -> Named (def_opt, named)
    | def_opt, [], Some ns_id, _ -> Namespace (def_opt, ns_id)
    | _ -> SideEffect (* fallback *)
  in
  Import ({ from; kind; withClause = None }, Parse_info.zero)

let merge_modules ~dest (modules : Esm.esm_module list) : program =
  let dest_id = Esm.ModuleId.of_path dest in
  (* Build a combined map of all exports across all modules.
     When an import from dest is found, we look up the export by name here. *)
  let all_exports =
    List.fold_left modules ~init:StringMap.empty ~f:(fun acc m ->
        StringMap.fold
          (fun name export acc ->
            (* First module's export wins for conflicting names *)
            if StringMap.mem name acc then acc else StringMap.add name export acc)
          m.Esm.exports
          acc)
  in
  (* Collect external imports (not from dest) and generate import statements *)
  let import_stmts =
    List.concat_map modules ~f:(fun m ->
        List.filter_map m.Esm.imports ~f:(fun import ->
            if Esm.ModuleId.equal import.Esm.source dest_id
            then None (* Internal import - will be substituted *)
            else Some (generate_import_statement import, N)))
  in
  (* Process each module: filter imports and apply substitutions *)
  let body_stmts =
    List.concat_map modules ~f:(fun m ->
        (* Filter out imports from the destination module *)
        let internal_imports =
          List.filter m.Esm.imports ~f:(fun import ->
              Esm.ModuleId.equal import.Esm.source dest_id)
        in
        (* Build substitution for internal imports *)
        let subst =
          List.fold_left internal_imports ~init:Code.Var.Map.empty ~f:(fun acc import ->
              List.fold_left import.Esm.bindings ~init:acc ~f:(fun acc binding ->
                  match binding with
                  | Esm.ImportNamed (orig_name, V v) -> (
                      match StringMap.find_opt orig_name all_exports with
                      | Some export -> Code.Var.Map.add v export.Esm.local_ident acc
                      | None -> acc)
                  | Esm.ImportDefault (V v) -> (
                      match StringMap.find_opt "default" all_exports with
                      | Some export -> Code.Var.Map.add v export.Esm.local_ident acc
                      | None -> acc)
                  | _ -> acc))
        in
        apply_import_substitutions subst m.Esm.body)
  in
  (* Collect all exports from all modules *)
  let export_stmts =
    List.concat_map modules ~f:(fun m ->
        StringMap.fold
          (fun _name export acc ->
            let exported_name = Utf8_string.of_string_exn export.Esm.exported_name in
            ( Export (ExportNames [ export.Esm.local_ident, exported_name ], Parse_info.zero)
            , N )
            :: acc)
          m.Esm.exports
          [])
  in
  import_stmts @ body_stmts @ export_stmts
