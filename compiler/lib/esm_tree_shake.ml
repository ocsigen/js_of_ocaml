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

(* Collect free variables (unbound identifiers) in a statement *)
let collect_free_vars stmt =
  let free = new Js_traverse.free in
  let _ = free#statement stmt in
  free#get_free

let collect_free_vars_expr expr =
  let free = new Js_traverse.free in
  let _ = free#expression expr in
  free#get_free

let collect_free_vars_fun_decl decl =
  let free = new Js_traverse.free in
  let _ = free#fun_decl decl in
  free#get_free

let collect_free_vars_class_decl decl =
  let free = new Js_traverse.free in
  let _ = free#class_decl decl in
  free#get_free

(* Check if an expression might have side effects *)
let option_has ~f = function
  | None -> false
  | Some x -> f x

let rec expr_has_side_effects expr =
  match expr with
  | EVar _ | EStr _ | EBool _ | ENum _ | ERegexp _ | EPrivName _ -> false
  | EFun _ | EArrow _ | EClass _ -> false
  | EArr l ->
      List.exists l ~f:(function
        | ElementHole -> false
        | Element e | ElementSpread e -> expr_has_side_effects e)
  | EObj l ->
      List.exists l ~f:(function
        | Property (pn, e) ->
            property_name_has_side_effects pn || expr_has_side_effects e
        | PropertySpread e -> expr_has_side_effects e
        | PropertyMethod (pn, _) -> property_name_has_side_effects pn
        | CoverInitializedName _ -> false)
  | ESeq (e1, e2) -> expr_has_side_effects e1 || expr_has_side_effects e2
  | ECond (e1, e2, e3) ->
      expr_has_side_effects e1 || expr_has_side_effects e2 || expr_has_side_effects e3
  | EBin (op, e1, e2) -> (
      match op with
      (* Assignment operators have side effects *)
      | Eq | StarEq | SlashEq | ModEq | PlusEq | MinusEq | LslEq | AsrEq | LsrEq | BandEq
      | BxorEq | BorEq | OrEq | AndEq | ExpEq | CoalesceEq ->
          true
      (* Non-assignment operators: check operands *)
      | Or | And | Bor | Bxor | Band | EqEq | NotEq | EqEqEq | NotEqEq | Lt | Le | Gt | Ge
      | LtInt | LeInt | GtInt | GeInt | InstanceOf | In | Lsl | Lsr | Asr | Plus | Minus
      | Mul | Div | Mod | Exp | Coalesce ->
          expr_has_side_effects e1 || expr_has_side_effects e2)
  | EUn (op, e) -> (
      match op with
      (* These operators have side effects *)
      | Delete | IncrA | DecrA | IncrB | DecrB | Await -> true
      (* These operators don't have side effects themselves *)
      | Not | Neg | Pl | Typeof | Void | Bnot -> expr_has_side_effects e)
  | ECall _ | ECallTemplate _ | ENew _ -> true (* calls might have side effects *)
  | EAccess (e1, _, e2) -> expr_has_side_effects e1 || expr_has_side_effects e2
  | EDot (e, _, _) | EDotPrivate (e, _, _) -> expr_has_side_effects e
  | EAssignTarget _ -> true
  | ETemplate parts ->
      List.exists parts ~f:(function
        | TStr _ -> false
        | TExp e -> expr_has_side_effects e)
  | EYield _ -> true
  | CoverParenthesizedExpressionAndArrowParameterList _
  | CoverCallExpressionAndAsyncArrowHead _ -> false

and property_name_has_side_effects pn =
  match pn with
  | PNI _ | PNS _ | PNN _ -> false
  | PComputed e -> expr_has_side_effects e

let rec stmt_has_side_effects stmt =
  match stmt with
  | Empty_statement | Debugger_statement -> false
  | Block l -> List.exists l ~f:(fun (s, _) -> stmt_has_side_effects s)
  | Variable_statement (_, decls) ->
      List.exists decls ~f:(function
        | DeclIdent (_, None) -> false
        | DeclIdent (_, Some (e, _)) -> expr_has_side_effects e
        | DeclPattern (_, (e, _)) -> expr_has_side_effects e)
  | Function_declaration _ -> false
  | Class_declaration (_, decl) -> class_decl_has_side_effects decl
  | Expression_statement e -> expr_has_side_effects e
  | If_statement (e, (s1, _), s2_opt) ->
      expr_has_side_effects e
      || stmt_has_side_effects s1
      || option_has s2_opt ~f:(fun (s2, _) -> stmt_has_side_effects s2)
  | Do_while_statement ((s, _), e) -> stmt_has_side_effects s || expr_has_side_effects e
  | While_statement (e, (s, _)) -> expr_has_side_effects e || stmt_has_side_effects s
  | For_statement (init, cond, incr, (body, _)) ->
      (match init with
      | Left None -> false
      | Left (Some e) -> expr_has_side_effects e
      | Right (_, decls) ->
          List.exists decls ~f:(function
            | DeclIdent (_, None) -> false
            | DeclIdent (_, Some (e, _)) -> expr_has_side_effects e
            | DeclPattern (_, (e, _)) -> expr_has_side_effects e))
      || option_has cond ~f:expr_has_side_effects
      || option_has incr ~f:expr_has_side_effects
      || stmt_has_side_effects body
  | ForIn_statement (_, e, (body, _))
  | ForOf_statement (_, e, (body, _))
  | ForAwaitOf_statement (_, e, (body, _)) ->
      expr_has_side_effects e || stmt_has_side_effects body
  | Continue_statement _ | Break_statement _ -> false
  | Return_statement (e_opt, _) -> option_has e_opt ~f:expr_has_side_effects
  | Labelled_statement (_, (s, _)) -> stmt_has_side_effects s
  | Switch_statement (e, cases1, default, cases2) ->
      expr_has_side_effects e
      || List.exists cases1 ~f:(fun (ce, sl) ->
             expr_has_side_effects ce
             || List.exists sl ~f:(fun (s, _) -> stmt_has_side_effects s))
      || option_has default ~f:(fun sl ->
             List.exists sl ~f:(fun (s, _) -> stmt_has_side_effects s))
      || List.exists cases2 ~f:(fun (ce, sl) ->
             expr_has_side_effects ce
             || List.exists sl ~f:(fun (s, _) -> stmt_has_side_effects s))
  | Throw_statement _ -> true
  | Try_statement (b, catch, finally) ->
      List.exists b ~f:(fun (s, _) -> stmt_has_side_effects s)
      || option_has catch ~f:(fun (_, b) ->
             List.exists b ~f:(fun (s, _) -> stmt_has_side_effects s))
      || option_has finally ~f:(fun b ->
             List.exists b ~f:(fun (s, _) -> stmt_has_side_effects s))
  | With_statement (e, (s, _)) -> expr_has_side_effects e || stmt_has_side_effects s
  | Import _ -> true
  | Export _ -> true

and class_decl_has_side_effects decl =
  option_has decl.extends ~f:expr_has_side_effects
  || List.exists decl.body ~f:(function
       | CEMethod (_, _, name, _) -> class_element_name_has_side_effects name
       | CEField (_, _, name, init) ->
           class_element_name_has_side_effects name
           || option_has init ~f:(fun (e, _) -> expr_has_side_effects e)
       | CEStaticBLock stmts ->
           List.exists stmts ~f:(fun (s, _) -> stmt_has_side_effects s)
       | CEAccessor (_, _, name, init) ->
           class_element_name_has_side_effects name
           || option_has init ~f:(fun (e, _) -> expr_has_side_effects e))

and class_element_name_has_side_effects name =
  match name with
  | PropName pn -> property_name_has_side_effects pn
  | PrivName _ -> false

(* Statement info for tree shaking *)
type stmt_info =
  { idx : int
  ; defines : IdentSet.t
  ; uses : IdentSet.t (* local uses within the module *)
  ; import_uses : (Esm.ModuleId.t * string * Code.Var.t) list
      (* (source, export_name, local_binding_var) *)
  ; has_side_effects : bool
  ; stmt : statement * location
  }

(* Build a map from import binding ident to (source, export_name) *)
let build_import_map (imports : Esm.import_entry list) :
    (Esm.ModuleId.t * string) Code.Var.Map.t =
  List.fold_left imports ~init:Code.Var.Map.empty ~f:(fun acc import ->
      List.fold_left import.Esm.bindings ~init:acc ~f:(fun acc binding ->
          match binding with
          | Esm.ImportNamed (orig, V v) ->
              Code.Var.Map.add v (import.Esm.source, orig) acc
          | Esm.ImportDefault (V v) ->
              Code.Var.Map.add v (import.Esm.source, "default") acc
          | Esm.ImportNamespace (V v) ->
              (* Namespace imports use "*" to indicate all exports *)
              Code.Var.Map.add v (import.Esm.source, "*") acc
          (* S identifiers and side-effect imports don't add to the map *)
          | Esm.ImportNamed (_, S _)
          | Esm.ImportDefault (S _)
          | Esm.ImportNamespace (S _)
          | Esm.ImportSideEffect ->
              acc))

(* Analyze a statement: extract defines, split uses into local vs import *)
let analyze_stmt import_map idx (stmt, loc) : stmt_info =
  let defines, all_uses, has_side_effects =
    match stmt with
    | Variable_statement (_, decls) ->
        let defines =
          List.fold_left decls ~init:IdentSet.empty ~f:(fun acc decl ->
              let ids = bound_idents_of_variable_declaration decl in
              List.fold_left ids ~init:acc ~f:(fun acc id -> IdentSet.add id acc))
        in
        let uses =
          List.fold_left decls ~init:IdentSet.empty ~f:(fun acc decl ->
              match decl with
              | DeclIdent (_, None) -> acc
              | DeclIdent (_, Some (e, _)) -> IdentSet.union acc (collect_free_vars_expr e)
              | DeclPattern (_, (e, _)) -> IdentSet.union acc (collect_free_vars_expr e))
        in
        let side_effects =
          List.exists decls ~f:(function
            | DeclIdent (_, None) -> false
            | DeclIdent (_, Some (e, _)) -> expr_has_side_effects e
            | DeclPattern (_, (e, _)) -> expr_has_side_effects e)
        in
        defines, uses, side_effects
    | Function_declaration (id, decl) ->
        let defines = IdentSet.singleton id in
        let uses = collect_free_vars_fun_decl decl in
        defines, uses, false
    | Class_declaration (id, decl) ->
        let defines = IdentSet.singleton id in
        let uses = collect_free_vars_class_decl decl in
        let side_effects = class_decl_has_side_effects decl in
        defines, uses, side_effects
    (* Statements that don't define module-level bindings *)
    | Block _ | Empty_statement | Expression_statement _ | If_statement _
    | Do_while_statement _ | While_statement _ | For_statement _ | ForIn_statement _
    | ForOf_statement _ | ForAwaitOf_statement _ | Continue_statement _ | Break_statement _
    | Return_statement _ | With_statement _ | Labelled_statement _ | Switch_statement _
    | Throw_statement _ | Try_statement _ | Debugger_statement | Import _ | Export _ ->
        let uses = collect_free_vars stmt in
        IdentSet.empty, uses, stmt_has_side_effects stmt
  in
  (* Split uses into local identifiers and import references.
     import_uses is a list of (source_module, export_name, local_binding_var) *)
  let local_uses, import_uses =
    IdentSet.fold
      (fun id (local_acc, import_acc) ->
        match id with
        | V v -> (
            match Code.Var.Map.find_opt v import_map with
            | Some (source, name) -> local_acc, (source, name, v) :: import_acc
            | None -> IdentSet.add id local_acc, import_acc)
        | S _ -> IdentSet.add id local_acc, import_acc)
      all_uses
      (IdentSet.empty, [])
  in
  { idx; defines; uses = local_uses; import_uses; has_side_effects; stmt = stmt, loc }

(* Work item for the fixpoint *)
type work_item =
  | MarkExport of Esm.ModuleId.t * string
  | MarkIdent of Esm.ModuleId.t * ident
  | MarkModuleReached of Esm.ModuleId.t

(* Set for tracking live (module, export_name) pairs *)
module ExportKey = struct
  type t = Esm.ModuleId.t * string

  let compare (m1, s1) (m2, s2) =
    let c = Esm.ModuleId.compare m1 m2 in
    if c <> 0 then c else String.compare s1 s2
end

module ExportSet = Set.Make (ExportKey)

(* Set for tracking live (module, var) pairs *)
module VarKey = struct
  type t = Esm.ModuleId.t * Code.Var.t

  let compare (m1, v1) (m2, v2) =
    let c = Esm.ModuleId.compare m1 m2 in
    if c <> 0 then c else Code.Var.compare v1 v2
end

module VarSet = Set.Make (VarKey)

let run (graph : Esm.module_graph) ~(entry_exports : StringSet.t Esm.ModuleId.Map.t) :
    Esm.module_graph =
  (* Analyze all statements in all modules, storing as arrays for O(1) access *)
  let module_stmts : stmt_info array Esm.ModuleId.Map.t =
    Esm.ModuleId.Map.map
      (fun m ->
        let import_map = build_import_map m.Esm.imports in
        Array.of_list
          (List.mapi m.Esm.body ~f:(fun idx stmt -> analyze_stmt import_map idx stmt)))
      graph.modules
  in
  (* Build map from (module, ident) -> stmt indices that define it *)
  let def_map : int list Code.Var.Map.t Esm.ModuleId.Map.t =
    Esm.ModuleId.Map.map
      (fun stmts ->
        Array.fold_left stmts ~init:Code.Var.Map.empty ~f:(fun acc info ->
            IdentSet.fold
              (fun id acc ->
                match id with
                | V v ->
                    let existing =
                      match Code.Var.Map.find_opt v acc with
                      | Some l -> l
                      | None -> []
                    in
                    Code.Var.Map.add v (info.idx :: existing) acc
                | S _ -> acc)
              info.defines
              acc))
      module_stmts
  in
  (* Live state - using mutable arrays for statements, immutable sets for exports/idents *)
  let live_stmts : bool array Esm.ModuleId.Map.t =
    Esm.ModuleId.Map.map
      (fun m -> Array.make (List.length m.Esm.body) false)
      graph.modules
  in
  let live_exports = ref ExportSet.empty in
  let live_idents = ref VarSet.empty in
  let reached_modules = ref Esm.ModuleId.Set.empty in
  (* Worklist *)
  let worklist = Queue.create () in
  (* Mark a statement as live and enqueue its dependencies *)
  let mark_stmt_live module_id idx =
    let arr = Esm.ModuleId.Map.find module_id live_stmts in
    if not arr.(idx)
    then begin
      arr.(idx) <- true;
      (* Mark module as reached (for side-effect propagation) *)
      if not (Esm.ModuleId.Set.mem module_id !reached_modules)
      then begin
        reached_modules := Esm.ModuleId.Set.add module_id !reached_modules;
        Queue.push (MarkModuleReached module_id) worklist
      end;
      let stmts = Esm.ModuleId.Map.find module_id module_stmts in
      let info = stmts.(idx) in
      (* Enqueue local dependencies *)
      IdentSet.iter (fun id -> Queue.push (MarkIdent (module_id, id)) worklist) info.uses;
      (* Enqueue import dependencies and mark import bindings as live *)
      List.iter info.import_uses ~f:(fun (source, name, local_var) ->
          (* Mark the local import binding as live *)
          live_idents := VarSet.add (module_id, local_var) !live_idents;
          if String.equal name "*"
          then
            (* Namespace import: need all exports from source *)
            let source_module = Esm.ModuleId.Map.find source graph.modules in
            StringMap.iter
              (fun export_name _ ->
                Queue.push (MarkExport (source, export_name)) worklist)
              source_module.exports
          else Queue.push (MarkExport (source, name)) worklist)
    end
  in
  (* Mark an ident as live and find statements that define it *)
  let mark_ident_live module_id id =
    match id with
    | V v ->
        if not (VarSet.mem (module_id, v) !live_idents)
        then begin
          live_idents := VarSet.add (module_id, v) !live_idents;
          let def_map_for_module = Esm.ModuleId.Map.find module_id def_map in
          match Code.Var.Map.find_opt v def_map_for_module with
          | Some indices -> List.iter indices ~f:(mark_stmt_live module_id)
          | None -> ()
        end
    | S _ -> ()
  in
  (* Resolve an export to its source, following re-export chains *)
  let rec resolve_export module_id export_name =
    let m = Esm.ModuleId.Map.find module_id graph.modules in
    match StringMap.find_opt export_name m.Esm.exports with
    | None -> None
    | Some export -> (
        match export.kind with
        | Esm.Export_reexport (source, orig_name) ->
            let orig = if String.equal orig_name "*" then export_name else orig_name in
            resolve_export source orig
        | Esm.Export_var | Esm.Export_fun | Esm.Export_class ->
            Some (module_id, export.local_ident))
  in
  (* Mark an export as live *)
  let mark_export_live module_id export_name =
    if not (ExportSet.mem (module_id, export_name) !live_exports)
    then begin
      live_exports := ExportSet.add (module_id, export_name) !live_exports;
      match resolve_export module_id export_name with
      | Some (resolved_module, resolved_ident) ->
          Queue.push (MarkIdent (resolved_module, resolved_ident)) worklist
      | None -> ()
    end
  in
  (* Mark a module as reached: mark its side-effecting statements and propagate
     through side-effect imports *)
  let mark_module_reached module_id =
    match Esm.ModuleId.Map.find_opt module_id graph.modules with
    | None -> ()
    | Some m ->
        (* Mark all side-effecting statements in this module *)
        let stmts = Esm.ModuleId.Map.find module_id module_stmts in
        Array.iter stmts ~f:(fun info ->
            if info.has_side_effects then mark_stmt_live module_id info.idx);
        (* Propagate to modules imported for side effects *)
        List.iter m.Esm.imports ~f:(fun import ->
            List.iter import.Esm.bindings ~f:(fun binding ->
                match binding with
                | Esm.ImportSideEffect ->
                    if not (Esm.ModuleId.Set.mem import.Esm.source !reached_modules)
                    then begin
                      reached_modules :=
                        Esm.ModuleId.Set.add import.Esm.source !reached_modules;
                      Queue.push (MarkModuleReached import.Esm.source) worklist
                    end
                | Esm.ImportNamed _ | Esm.ImportDefault _ | Esm.ImportNamespace _ -> ()))
  in
  (* Initialize: mark entry exports as live *)
  Esm.ModuleId.Map.iter
    (fun module_id exports ->
      if StringSet.is_empty exports
      then
        (* Empty set = all exports of this entry module *)
        let m = Esm.ModuleId.Map.find module_id graph.modules in
        StringMap.iter
          (fun name _ -> Queue.push (MarkExport (module_id, name)) worklist)
          m.Esm.exports
      else
        StringSet.iter
          (fun name -> Queue.push (MarkExport (module_id, name)) worklist)
          exports)
    entry_exports;
  (* Fixpoint: process worklist until empty *)
  while not (Queue.is_empty worklist) do
    match Queue.pop worklist with
    | MarkExport (module_id, name) -> mark_export_live module_id name
    | MarkIdent (module_id, id) -> mark_ident_live module_id id
    | MarkModuleReached module_id -> mark_module_reached module_id
  done;
  (* Build result: filter each module to keep only live statements *)
  let modules =
    Esm.ModuleId.Map.mapi
      (fun module_id m ->
        let arr = Esm.ModuleId.Map.find module_id live_stmts in
        (* Keep module only if it has live statements *)
        if not (Array.exists ~f:(fun x -> x) arr)
        then None
        else
          let stmts = Esm.ModuleId.Map.find module_id module_stmts in
          let body =
            Array.fold_right stmts ~init:[] ~f:(fun info acc ->
                if arr.(info.idx) then info.stmt :: acc else acc)
          in
          (* Filter exports to only those that are live *)
          let exports =
            StringMap.filter
              (fun name _ -> ExportSet.mem (module_id, name) !live_exports)
              m.Esm.exports
          in
          (* Filter imports to only those with live bindings *)
          let imports =
            List.filter_map m.Esm.imports ~f:(fun import ->
                let bindings =
                  List.filter import.Esm.bindings ~f:(fun binding ->
                      match binding with
                      | Esm.ImportNamed (_, V v)
                      | Esm.ImportDefault (V v)
                      | Esm.ImportNamespace (V v) ->
                          VarSet.mem (module_id, v) !live_idents
                      | Esm.ImportSideEffect -> true
                      (* S identifiers are not tracked in live_idents *)
                      | Esm.ImportNamed (_, S _)
                      | Esm.ImportDefault (S _)
                      | Esm.ImportNamespace (S _) ->
                          false)
                in
                if List.is_empty bindings
                then None
                else Some { import with bindings })
          in
          Some { m with body; exports; imports })
      graph.modules
  in
  let modules =
    Esm.ModuleId.Map.fold
      (fun id m_opt acc ->
        match m_opt with
        | Some m -> Esm.ModuleId.Map.add id m acc
        | None -> acc)
      modules
      Esm.ModuleId.Map.empty
  in
  { graph with modules }
