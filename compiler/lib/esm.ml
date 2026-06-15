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

(* ========== Module Identifiers ========== *)

module ModuleId = struct
  type t = Id of string [@@unboxed]

  let of_path path = Id path

  let to_path (Id t) = t

  let compare (Id a) (Id b) = String.compare a b

  let equal (Id a) (Id b) = String.equal a b

  module Set = Set.Make (struct
    type nonrec t = t

    let compare = compare
  end)

  module Map = Map.Make (struct
    type nonrec t = t

    let compare = compare
  end)
end

module ModuleSCC = Strongly_connected_components.Make (struct
  type t = ModuleId.t

  module Map = ModuleId.Map
  module Set = ModuleId.Set
end)

(* ========== Module Representation ========== *)

type export_kind =
  | Export_var
  | Export_fun
  | Export_class
  | Export_reexport of ModuleId.t * string

type export_entry =
  { exported_name : string
  ; local_ident : ident
  ; kind : export_kind
  }

type import_binding =
  | ImportNamed of string * ident
  | ImportDefault of ident
  | ImportNamespace of ident
  | ImportSideEffect

type import_entry =
  { source : ModuleId.t
  ; bindings : import_binding list
  }

type esm_module =
  { id : ModuleId.t
  ; imports : import_entry list
  ; exports : export_entry StringMap.t
  ; star_exports : ModuleId.t list (* export * from sources *)
  ; body : statement_list
  ; has_default_export : bool
  }

type module_graph =
  { modules : esm_module ModuleId.Map.t
  ; deps : ModuleId.Set.t ModuleId.Map.t
  }

(* ========== Identifier Helpers ========== *)

let ident_to_utf8 id =
  match id with
  | S { name; _ } -> name
  | V v ->
      let name =
        match Code.Var.get_name v with
        | Some n -> n
        | None -> Printf.sprintf "v%d" (Code.Var.idx v)
      in
      Utf8_string.of_string_exn name

let fresh_ident name =
  let v = Code.Var.fresh_n name in
  V v

(* Normalize export declarations: convert ExportVar/ExportFun/ExportClass
   and ExportDefault* to declaration + ExportNames. This allows declared_names
   to find all names and simplifies extract_exports and extract_body. *)
let normalize_exports (stmts : statement_list) : statement_list =
  let default_utf8 = Utf8_string.of_string_exn "default" in
  List.concat_map stmts ~f:(fun (stmt, loc) ->
      match stmt with
      | Export (k, pi) -> begin
          match k with
          | ExportVar (k, decls) ->
              let ids =
                List.concat_map decls ~f:(fun decl ->
                    bound_idents_of_variable_declaration decl)
              in
              let export_names = List.map ids ~f:(fun id -> id, ident_to_utf8 id) in
              [ Variable_statement (k, decls), loc
              ; Export (ExportNames export_names, pi), N
              ]
          | ExportFun (id, decl) ->
              [ Function_declaration (id, decl), loc
              ; Export (ExportNames [ id, ident_to_utf8 id ], pi), N
              ]
          | ExportClass (id, decl) ->
              [ Class_declaration (id, decl), loc
              ; Export (ExportNames [ id, ident_to_utf8 id ], pi), N
              ]
          | ExportDefaultFun (id_opt, decl) ->
              let id = Option.value id_opt ~default:(fresh_ident "default") in
              [ Function_declaration (id, decl), loc
              ; Export (ExportNames [ id, default_utf8 ], pi), N
              ]
          | ExportDefaultClass (id_opt, decl) ->
              let id = Option.value id_opt ~default:(fresh_ident "default") in
              [ Class_declaration (id, decl), loc
              ; Export (ExportNames [ id, default_utf8 ], pi), N
              ]
          | ExportDefaultExpression e ->
              let id = fresh_ident "default" in
              [ Variable_statement (Const, [ DeclIdent (id, Some (e, N)) ]), loc
              ; Export (ExportNames [ id, default_utf8 ], pi), N
              ]
          | CoverExportFrom _ -> assert false
          (* These export forms pass through unchanged *)
          | ExportNames _ | ExportFrom _ -> [ stmt, loc ]
        end
      | _ -> [ stmt, loc ])

(* Rename all declarations to fresh V identifiers using Js_traverse.rename_variable.
   This includes all variables, functions, classes, imports, and exports at all scopes. *)
let rename_module_declarations (stmts : statement_list) : statement_list =
  let stmts = normalize_exports stmts in
  let renamer = new Js_traverse.rename_variable ~esm:true in
  renamer#program stmts

(* ========== Module Analysis ========== *)

(* Extract imports from a module *)
let extract_imports ~resolve (stmts : statement_list) : import_entry list =
  List.filter_map stmts ~f:(fun (stmt, _loc) ->
      match stmt with
      | Import ({ from = Utf8_string.Utf8 from_path; kind; _ }, _) ->
          let source = resolve from_path in
          let bindings =
            match kind with
            | SideEffect -> [ ImportSideEffect ]
            | Default id -> [ ImportDefault id ]
            | DeferNamespace id | Namespace (None, id) -> [ ImportNamespace id ]
            | Namespace (Some default_id, ns_id) ->
                [ ImportDefault default_id; ImportNamespace ns_id ]
            | Named (default_opt, named) -> (
                let bindings =
                  List.map named ~f:(fun (Utf8_string.Utf8 orig, local) ->
                      ImportNamed (orig, local))
                in
                match default_opt with
                | None -> bindings
                | Some id -> ImportDefault id :: bindings)
          in
          Some { source; bindings }
      | _ -> None)

(* Extract exports from a module (called after normalize_exports and renaming) *)
let extract_exports ~resolve (stmts : statement_list) :
    export_entry StringMap.t * ModuleId.t list * bool =
  let exports = ref StringMap.empty in
  let star_exports = ref [] in
  let has_default = ref false in
  let add_export name entry = exports := StringMap.add name entry !exports in
  List.iter stmts ~f:(fun (stmt, _loc) ->
      match stmt with
      | Export (export, _) -> (
          match export with
          (* After normalize_exports, all direct exports become ExportNames *)
          | ExportNames named_exports ->
              List.iter named_exports ~f:(fun (id, Utf8_string.Utf8 exported_name) ->
                  if String.equal exported_name "default" then has_default := true;
                  add_export
                    exported_name
                    { exported_name; local_ident = id; kind = Export_var })
          | ExportFrom { kind = Export_all None; from = Utf8_string.Utf8 from_path; _ } ->
              (* export * from 'module' - track source for later resolution *)
              let source = resolve from_path in
              star_exports := source :: !star_exports
          | ExportFrom
              { kind = Export_all (Some (Utf8_string.Utf8 exported_name))
              ; from = Utf8_string.Utf8 from_path
              ; _
              } ->
              (* export * as name from 'module' *)
              let source = resolve from_path in
              let local_ident = fresh_ident exported_name in
              add_export
                exported_name
                { exported_name; local_ident; kind = Export_reexport (source, "*") }
          | ExportFrom { kind = Export_names named; from = Utf8_string.Utf8 from_path; _ }
            ->
              let source = resolve from_path in
              List.iter
                named
                ~f:(fun (Utf8_string.Utf8 orig_name, Utf8_string.Utf8 exported_name) ->
                  let local_ident = fresh_ident exported_name in
                  add_export
                    exported_name
                    { exported_name
                    ; local_ident
                    ; kind = Export_reexport (source, orig_name)
                    })
          | ExportVar _
          | ExportFun _
          | ExportClass _
          | ExportDefaultFun _
          | ExportDefaultClass _
          | ExportDefaultExpression _ ->
              (* These should have been normalized to ExportNames *)
              assert false
          | CoverExportFrom _ -> assert false)
      | _ -> ());
  !exports, List.rev !star_exports, !has_default

let analyze_module ~resolve id (program : program) : esm_module =
  (* Rename first - this also normalizes exports *)
  let renamed_program = rename_module_declarations program in
  (* Extract from renamed program - identifiers are already V variants *)
  let exports, star_exports, has_default_export =
    extract_exports ~resolve renamed_program
  in
  let imports = extract_imports ~resolve renamed_program in
  (* Extract body: filter out Import and Export statements *)
  let body =
    List.filter renamed_program ~f:(fun (stmt, _loc) ->
        match stmt with
        | Import _ | Export _ -> false
        | _ -> true)
  in
  { id; imports; exports; star_exports; body; has_default_export }

(* ========== Graph Construction ========== *)

let rec build_graph ~parse ~resolve ~entry_points : module_graph =
  let modules = ref ModuleId.Map.empty in
  let deps = ref ModuleId.Map.empty in
  let worklist = Queue.create () in
  (* Initialize worklist with entry points *)
  List.iter entry_points ~f:(fun path ->
      let id = ModuleId.of_path path in
      Queue.push id worklist);
  (* Process modules until worklist is empty *)
  while not (Queue.is_empty worklist) do
    let id = Queue.pop worklist in
    if not (ModuleId.Map.mem id !modules)
    then begin
      let path = ModuleId.to_path id in
      let program = parse path in
      let resolve_for_module specifier =
        match resolve ~from:path specifier with
        | Some resolved_path -> ModuleId.of_path resolved_path
        | None -> failwith (Printf.sprintf "Cannot resolve '%s' from '%s'" specifier path)
      in
      let esm = analyze_module ~resolve:resolve_for_module id program in
      modules := ModuleId.Map.add id esm !modules;
      (* Collect dependencies *)
      let module_deps =
        List.fold_left esm.imports ~init:ModuleId.Set.empty ~f:(fun acc import ->
            ModuleId.Set.add import.source acc)
      in
      (* Add re-export dependencies *)
      let module_deps =
        StringMap.fold
          (fun _ export acc ->
            match export.kind with
            | Export_reexport (source, _) -> ModuleId.Set.add source acc
            | Export_var | Export_fun | Export_class -> acc)
          esm.exports
          module_deps
      in
      (* Add star export dependencies *)
      let module_deps =
        List.fold_left esm.star_exports ~init:module_deps ~f:(fun acc source ->
            ModuleId.Set.add source acc)
      in
      deps := ModuleId.Map.add id module_deps !deps;
      (* Add dependencies to worklist *)
      ModuleId.Set.iter (fun dep -> Queue.push dep worklist) module_deps
    end
  done;
  let graph = { modules = !modules; deps = !deps } in
  (* Resolve star exports *)
  resolve_star_exports graph

(* Resolve export * from by copying exports from source modules.

   Per the ES spec:
   - The 'default' export is never re-exported by 'export *'
   - A module's own explicit exports take precedence over 'export *'
   - If the same name comes from multiple 'export *' sources, it should be
     an ambiguous export error at link time

   We use "first wins" semantics for conflicting star exports instead of
   tracking ambiguous exports, which is a pragmatic simplification. *)
and resolve_star_exports (graph : module_graph) : module_graph =
  (* Get modules in topological order (dependencies first) *)
  let sorted = topological_sort_for_star_exports graph in
  (* Process modules and accumulate resolved exports *)
  let resolved_modules =
    List.fold_left sorted ~init:graph.modules ~f:(fun modules id ->
        match ModuleId.Map.find_opt id modules with
        | None -> modules
        | Some m ->
            if List.is_empty m.star_exports
            then modules
            else
              (* Collect exports from all star export sources *)
              let additional_exports =
                List.fold_left
                  m.star_exports
                  ~init:StringMap.empty
                  ~f:(fun acc source_id ->
                    match ModuleId.Map.find_opt source_id modules with
                    | None -> acc
                    | Some source_module ->
                        (* Add all exports from source, except default *)
                        StringMap.fold
                          (fun name _export acc ->
                            if String.equal name "default"
                            then acc
                            else if StringMap.mem name acc || StringMap.mem name m.exports
                            then acc
                            else
                              (* Create a re-export entry *)
                              let local_ident = fresh_ident name in
                              StringMap.add
                                name
                                { exported_name = name
                                ; local_ident
                                ; kind = Export_reexport (source_id, name)
                                }
                                acc)
                          source_module.exports
                          acc)
              in
              (* Merge additional exports into module *)
              let exports =
                StringMap.union (fun _ a _ -> Some a) m.exports additional_exports
              in
              ModuleId.Map.add id { m with exports } modules)
  in
  { graph with modules = resolved_modules }

(* Simple topological sort for star export resolution *)
and topological_sort_for_star_exports (graph : module_graph) : ModuleId.t list =
  let components = ModuleSCC.connected_components_sorted_from_roots_to_leaf graph.deps in
  Array.fold_right components ~init:[] ~f:(fun component acc ->
      match component with
      | ModuleSCC.No_loop id -> id :: acc
      | ModuleSCC.Has_loop ids -> List.rev_append ids acc)
  |> List.rev

(* Topological sort of modules *)
let topological_sort (graph : module_graph) : ModuleId.t list =
  let components = ModuleSCC.connected_components_sorted_from_roots_to_leaf graph.deps in
  (* Components are sorted from roots to leaves, we want leaves first *)
  let sorted =
    Array.fold_right components ~init:[] ~f:(fun component acc ->
        match component with
        | ModuleSCC.No_loop id -> id :: acc
        | ModuleSCC.Has_loop ids ->
            (* For cycles, add all modules in cycle order *)
            List.rev_append ids acc)
  in
  (* Reverse to get dependency order (dependencies before dependents) *)
  List.rev sorted

(* Resolve re-exports to find the actual source identifier *)
let rec resolve_reexport all_modules exp =
  match exp.kind with
  | Export_reexport (reexport_source, reexport_name) ->
      let reexport_module =
        match ModuleId.Map.find_opt reexport_source all_modules with
        | Some m -> m
        | None -> failwith ("Module not found: " ^ ModuleId.to_path reexport_source)
      in
      let reexport_exp =
        match StringMap.find_opt reexport_name reexport_module.exports with
        | Some e -> e
        | None -> exp (* fallback - use original export *)
      in
      resolve_reexport all_modules reexport_exp
  | Export_var | Export_fun | Export_class ->
      (* Found the actual export - local_ident is already a V identifier *)
      exp.local_ident
