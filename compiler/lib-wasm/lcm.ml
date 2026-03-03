(* Lazy Code Motion (LCM) for boxing/unboxing and tagging/untagging conversions.

   In the wasm_of_ocaml backend, the type analysis (typing.ml) assigns unboxed or
   untagged types to variables when profitable. The code generator then inserts
   conversion operations (box/unbox/tag/untag) at every point where a representation
   mismatch occurs: function call boundaries, branches to blocks with differently-typed
   parameters, returns, stores, etc.

   Many of these conversions are redundant or could be hoisted out of loops. For example,
   a loop that repeatedly unboxes a float from an invariant variable will emit the same
   unbox on every iteration. LCM eliminates this redundancy.

   The pass works in two phases:

   1. **Lowering** ([lower_conversions]): Materialises implicit representation mismatches
      as explicit IR primitives (Wasm_box_*, Wasm_unbox_*, Wasm_tag_int, Wasm_untag_int).
      After this phase, every conversion is a visible instruction that can be analysed.

   2. **LCM optimisation** ([process_function]): Applies the classical Knoop-Ruthing-Steffen
      Lazy Code Motion algorithm to these conversion instructions. LCM finds the optimal
      placement: as early as necessary (to eliminate redundancy) but as late as possible
      (to avoid lengthening lifetimes or executing speculatively). The five dataflow
      analyses are:
        - Anticipatability: can the conversion be moved to this point?
        - Availability: has the conversion already been computed on all paths?
        - Earliest = anticipated but not yet available
        - Delayability: can we push earliest placements further down?
        - Latest = last point where a delayed insertion is still correct
        - Isolated: is the conversion used only once after insertion?
      After computing optimal placements, the rewrite pass inserts conversions at
      [latest \ isolated] points and substitutes redundant occurrences with the
      hoisted result.

   3. **Peephole cleanup** ([optimize_peephole_conversions]): Eliminates inverse conversion
      pairs (e.g. box(unbox(x)) -> x) that may arise from the LCM rewrite.

   The analysis runs independently on each function to avoid cross-function variable
   references in inserted instructions.

   Reference: J. Knoop, O. Ruthing, B. Steffen, "Lazy Code Motion", PLDI 1992. *)

open! Stdlib
open Code
module VarSet = Var.Set

let debug = Debug.find "lcm"

type conversion_kind =
  | Unbox_i32
  | Unbox_i64
  | Unbox_f64
  | Box_i32
  | Box_i64
  | Box_f64
  | Untag_int
  | Tag_int

module Conv = struct
  type t = conversion_kind * Var.t

  let compare (k1, v1) (k2, v2) =
    let c = Poly.compare k1 k2 in
    if c <> 0 then c else Var.compare v1 v2
end

module ConvSet = Set.Make (Conv)
module ConvMap = Map.Make (Conv)

let prim_of_kind = function
  | Unbox_i32 -> Wasm_unbox_i32
  | Unbox_i64 -> Wasm_unbox_i64
  | Unbox_f64 -> Wasm_unbox_f64
  | Box_i32 -> Wasm_box_i32
  | Box_i64 -> Wasm_box_i64
  | Box_f64 -> Wasm_box_f64
  | Untag_int -> Wasm_untag_int
  | Tag_int -> Wasm_tag_int

let kind_of_prim = function
  | Wasm_unbox_i32 -> Some Unbox_i32
  | Wasm_unbox_i64 -> Some Unbox_i64
  | Wasm_unbox_f64 -> Some Unbox_f64
  | Wasm_box_i32 -> Some Box_i32
  | Wasm_box_i64 -> Some Box_i64
  | Wasm_box_f64 -> Some Box_f64
  | Wasm_untag_int -> Some Untag_int
  | Wasm_tag_int -> Some Tag_int
  | _ -> None

let inverse_kind = function
  | Unbox_i32 -> Some Box_i32
  | Unbox_i64 -> Some Box_i64
  | Unbox_f64 -> Some Box_f64
  | Box_i32 -> Some Unbox_i32
  | Box_i64 -> Some Unbox_i64
  | Box_f64 -> Some Unbox_f64
  | Untag_int -> Some Tag_int
  | Tag_int -> Some Untag_int

let type_of_kind = function
  | Unbox_i32 -> Typing.Number (Typing.Int32, Typing.Unboxed)
  | Unbox_i64 -> Typing.Number (Typing.Int64, Typing.Unboxed)
  | Unbox_f64 -> Typing.Number (Typing.Float, Typing.Unboxed)
  | Box_i32 -> Typing.Number (Typing.Int32, Typing.Boxed)
  | Box_i64 -> Typing.Number (Typing.Int64, Typing.Boxed)
  | Box_f64 -> Typing.Number (Typing.Float, Typing.Boxed)
  | Untag_int -> Typing.Int Typing.Integer.Normalized
  | Tag_int -> Typing.Int Typing.Integer.Ref

(* Check whether a conversion is safe given the operand's type. A conversion
   is safe when the operand's type is known to match the expected input
   representation — e.g. Unbox_f64 on a value typed as Number(Float, Boxed).
   Box/Tag operations are inherently safe since their operands have distinct
   Wasm types. Unsafe conversions operate on Top-typed operands where the
   runtime representation may not match (e.g. with GADTs). *)
let is_safe_input kind typ =
  match kind with
  | Unbox_i32 -> Poly.equal typ (Typing.Number (Typing.Int32, Typing.Boxed))
  | Unbox_i64 -> Poly.equal typ (Typing.Number (Typing.Int64, Typing.Boxed))
  | Unbox_f64 -> Poly.equal typ (Typing.Number (Typing.Float, Typing.Boxed))
  | Box_i32 | Box_i64 | Box_f64 | Tag_int -> true
  | Untag_int -> (
      match typ with
      | Typing.Int (Typing.Integer.Ref | Typing.Integer.Normalized | Typing.Integer.Unnormalized)
        -> true
      | _ -> false)

(* Determine which conversion operation, if any, is needed to go from one
   representation to another. Returns [None] when the representations match
   or no conversion is applicable. *)
let number_conversion_kind ~(from : Typing.typ) ~(into : Typing.typ) =
  match from, into with
  | ( Typing.Number (Typing.Int32, Typing.Unboxed)
    , Typing.Number (Typing.Int32, Typing.Unboxed) )
  | ( Typing.Number (Typing.Int64, Typing.Unboxed)
    , Typing.Number (Typing.Int64, Typing.Unboxed) )
  | ( Typing.Number (Typing.Float, Typing.Unboxed)
    , Typing.Number (Typing.Float, Typing.Unboxed) )
  | Int (Normalized | Unnormalized), Int (Normalized | Unnormalized) -> None
  | _, Typing.Int (Typing.Integer.Normalized | Typing.Integer.Unnormalized) ->
      Some Untag_int
  | ( Typing.Int (Typing.Integer.Normalized | Typing.Integer.Unnormalized)
    , Typing.Int Typing.Integer.Ref ) -> Some Tag_int
  | Typing.Int _, _ | _, Typing.Int _ -> None
  | Typing.Number (_, Typing.Unboxed), Typing.Number (_, Typing.Unboxed) -> None
  | _, Typing.Number (Typing.Int32, Typing.Unboxed) -> Some Unbox_i32
  | _, Typing.Number (Typing.Int64, Typing.Unboxed) -> Some Unbox_i64
  | _, Typing.Number (Typing.Float, Typing.Unboxed) -> Some Unbox_f64
  | Typing.Number (Typing.Int32, Typing.Unboxed), _ -> Some Box_i32
  | Typing.Number (Typing.Int64, Typing.Unboxed), _ -> Some Box_i64
  | Typing.Number (Typing.Float, Typing.Unboxed), _ -> Some Box_f64
  | _ -> None

let lower_var_conversion ~types ~(from : Typing.typ) ~(into : Typing.typ) x =
  match number_conversion_kind ~from ~into with
  | None -> [], x
  | Some kind ->
      let tmp = Var.fresh () in
      Typing.set_var_type types tmp into;
      [ Let (tmp, Prim (prim_of_kind kind, [ Pv x ])) ], tmp

(* Phase 1: Lowering.

   Walk every instruction and branch, and insert explicit conversion primitives
   wherever the type analysis indicates a representation mismatch. For example,
   if a function parameter expects an unboxed float but the argument is boxed, an
   [Wasm_unbox_f64] instruction is inserted before the call. Similarly, if a
   primitive produces an unboxed result but the variable is typed as boxed, a
   boxing instruction is inserted after the definition.

   For branches with multiple targets (Cond, Switch, Pushtrap), conversions
   needed for different targets are placed on split edges (fresh intermediate
   blocks) so they don't execute on the wrong path. *)
let lower_conversions
    (blocks : block Addr.Map.t)
    (types : Typing.t)
    (global_flow_info : Global_flow.info)
    (return_type : Typing.typ)
    (free_pc : int ref) =
  let lower_apply x ~f ~args ~exact =
    let closure =
      if exact
      then
        match Global_flow.get_unique_closure global_flow_info f with
        | Some (g, params)
          when List.compare_length_with args ~len:(List.length params) = 0 ->
            Some (g, params)
        | Some _ | None -> None
      else None
    in
    let target_types =
      match closure with
      | Some (_, params) -> List.map params ~f:(Typing.var_type types)
      | None -> List.map args ~f:(fun _ -> Typing.Top)
    in
    let lowered_args_rev = ref [] in
    let args' =
      List.map2
        ~f:(fun arg into ->
          let from = Typing.var_type types arg in
          let lowered, arg' = lower_var_conversion ~types ~from ~into arg in
          lowered_args_rev := List.rev_append lowered !lowered_args_rev;
          arg')
        args
        target_types
    in
    let lowered_args = List.rev !lowered_args_rev in
    match closure with
    | Some (g, _) -> (
        let from = Typing.return_type types g in
        let into = Typing.var_type types x in
        match number_conversion_kind ~from ~into with
        | None -> lowered_args @ [ Let (x, Apply { f; args = args'; exact }) ]
        | Some kind ->
            let tmp = Var.fresh () in
            Typing.set_var_type types tmp from;
            lowered_args
            @ [ Let (tmp, Apply { f; args = args'; exact })
              ; Let (x, Prim (prim_of_kind kind, [ Pv tmp ]))
              ])
    | None -> lowered_args @ [ Let (x, Apply { f; args = args'; exact }) ]
  in
  let lower_prim x p args =
    let target_types_opt =
      let top = Typing.Top in
      let int_n = Typing.Int Typing.Integer.Normalized in
      match p with
      | Extern nm -> fst (Typing.prim_sig nm)
      | Array_get -> Some [ top; int_n ]
      | Lt | Le | Ult -> Some [ int_n; int_n ]
      | _ -> None
    in
    let args', lowered_args =
      match target_types_opt with
      | Some target_types
        when List.compare_length_with args ~len:(List.length target_types) = 0 ->
          let lowered_args_rev = ref [] in
          let args' =
            List.map2
              ~f:(fun arg into ->
                match arg with
                | Pv v ->
                    let from = Typing.var_type types v in
                    let lowered, v' = lower_var_conversion ~types ~from ~into v in
                    lowered_args_rev := List.rev_append lowered !lowered_args_rev;
                    Pv v'
                | Pc _ -> arg)
              args
              target_types
          in
          args', List.rev !lowered_args_rev
      | _ -> args, []
    in
    lowered_args @ [ Let (x, Prim (p, args')) ]
  in
  let get_from e =
    match e with
    | Constant c -> Typing.constant_type c
    | Field (_, _, Float) -> Typing.Number (Typing.Float, Typing.Unboxed)
    | Prim (p, _) -> (
        match p with
        | Wasm_unbox_f64 -> Typing.Number (Typing.Float, Typing.Unboxed)
        | Wasm_unbox_i32 -> Typing.Number (Typing.Int32, Typing.Unboxed)
        | Wasm_unbox_i64 -> Typing.Number (Typing.Int64, Typing.Unboxed)
        | Wasm_untag_int | Lt | Le | Ult | IsInt | Eq | Neq | Not | Vectlength ->
            Typing.Int Typing.Integer.Normalized
        | Extern nm -> snd (Typing.prim_sig nm)
        | _ -> Typing.Top)
    | Apply { f; _ } -> Typing.return_type types f
    | _ -> Typing.Top
  in
  let replace_assigned x tmp i =
    match i with
    | Let (v, e) when Var.equal v x -> Let (tmp, e)
    | _ -> i
  in
  let lower_instr = function
    | Set_field (x, n, Non_float, y) ->
        let lowered, y' =
          lower_var_conversion ~types ~from:(Typing.var_type types y) ~into:Typing.Top y
        in
        lowered @ [ Set_field (x, n, Non_float, y') ]
    | Array_set (x, y, z) ->
        let int_n = Typing.Int Typing.Integer.Normalized in
        let lowered1, y' =
          lower_var_conversion ~types ~from:(Typing.var_type types y) ~into:int_n y
        in
        let lowered2, z' =
          lower_var_conversion ~types ~from:(Typing.var_type types z) ~into:Typing.Top z
        in
        lowered1 @ lowered2 @ [ Array_set (x, y', z') ]
    | Assign (x, y) ->
        let from = Typing.var_type types y in
        let into = Typing.var_type types x in
        let lowered, y' = lower_var_conversion ~types ~from ~into y in
        lowered @ [ Assign (x, y') ]
    | Let (x, Apply { f; args; exact }) -> lower_apply x ~f ~args ~exact
    | Let (x, e) ->
        let lowered_e =
          match e with
          | Prim (p, args) -> lower_prim x p args
          | _ -> [ Let (x, e) ]
        in
        let from = get_from e in
        let into = Typing.var_type types x in
        let lowered, _ =
          match number_conversion_kind ~from ~into with
          | Some kind ->
              let tmp = Var.fresh () in
              Typing.set_var_type types tmp from;
              let lowered_e =
                let last = List.hd (List.rev lowered_e) in
                let rest = List.rev (List.tl (List.rev lowered_e)) in
                rest @ [ replace_assigned x tmp last ]
              in
              lowered_e @ [ Let (x, Prim (prim_of_kind kind, [ Pv tmp ])) ], tmp
          | None -> lowered_e, x
        in
        lowered
    | i -> [ i ]
  in
  let new_blocks_map = ref Addr.Map.empty in
  let split_edge lowered (pc, args) =
    if List.is_empty lowered
    then pc, args
    else
      let new_pc = !free_pc in
      free_pc := new_pc + 1;
      new_blocks_map :=
        Addr.Map.add
          new_pc
          { params = []; body = lowered; branch = Branch (pc, args) }
          !new_blocks_map;
      new_pc, []
  in
  let lower_branch branch =
    let lower_cont (pc, args) =
      let target_block = Addr.Map.find pc blocks in
      let target_types = List.map ~f:(Typing.var_type types) target_block.params in
      let lowered_args_rev = ref [] in
      let args' =
        List.map2
          ~f:(fun arg into ->
            let from = Typing.var_type types arg in
            let lowered, arg' = lower_var_conversion ~types ~from ~into arg in
            lowered_args_rev := List.rev_append lowered !lowered_args_rev;
            arg')
          args
          target_types
      in
      List.rev !lowered_args_rev, (pc, args')
    in
    let int_n = Typing.Int Typing.Integer.Normalized in
    match branch with
    | Return y ->
        let from = Typing.var_type types y in
        let lowered, y' = lower_var_conversion ~types ~from ~into:return_type y in
        lowered, Return y'
    | Raise (y, l) ->
        let from = Typing.var_type types y in
        let lowered, y' = lower_var_conversion ~types ~from ~into:Typing.Top y in
        lowered, Raise (y', l)
    | Branch cont ->
        let lowered, cont' = lower_cont cont in
        lowered, Branch cont'
    | Cond (v, cont1, cont2) ->
        let lowered1, cont1' = lower_cont cont1 in
        let lowered2, cont2' = lower_cont cont2 in
        let cont1'' = split_edge lowered1 cont1' in
        let cont2'' = split_edge lowered2 cont2' in
        [], Cond (v, cont1'', cont2'')
    | Switch (v, conts) ->
        let lowered_v, v' =
          lower_var_conversion ~types ~from:(Typing.var_type types v) ~into:int_n v
        in
        let conts' =
          Array.map
            ~f:(fun cont ->
              let lowered, cont' = lower_cont cont in
              split_edge lowered cont')
            conts
        in
        lowered_v, Switch (v', conts')
    | Pushtrap (cont1, v, cont2) ->
        let lowered1, cont1' = lower_cont cont1 in
        let lowered2, cont2' = lower_cont cont2 in
        let cont1'' = split_edge lowered1 cont1' in
        let cont2'' = split_edge lowered2 cont2' in
        [], Pushtrap (cont1'', v, cont2'')
    | Poptrap cont ->
        let lowered, cont' = lower_cont cont in
        lowered, Poptrap cont'
    | Stop -> [], Stop
  in
  let blocks =
    Addr.Map.map
      (fun block ->
        let body_lowered = List.concat_map ~f:lower_instr block.body in
        let branch_lowered, branch' = lower_branch block.branch in
        { block with body = body_lowered @ branch_lowered; branch = branch' })
      blocks
  in
  Addr.Map.union (fun _ a _ -> Some a) blocks !new_blocks_map

(* Collect the universe of all conversions that appear in the function,
   along with the join of their result types across all occurrences. *)
let get_all_conversions blocks types =
  let all_convs = ref ConvSet.empty in
  let conv_types = ref ConvMap.empty in
  Addr.Map.iter
    (fun _ block ->
      List.iter
        ~f:(function
          | Let (x, Prim (p, [ Pv v ])) -> (
              match kind_of_prim p with
              | Some kind ->
                  let conv = kind, v in
                  all_convs := ConvSet.add conv !all_convs;
                  let typ = Typing.var_type types x in
                  let typ =
                    match ConvMap.find_opt conv !conv_types with
                    | Some current -> Typing.join current typ
                    | None -> typ
                  in
                  conv_types := ConvMap.add conv typ !conv_types
              | None -> ())
          | _ -> ())
        block.body)
    blocks;
  !all_convs, !conv_types

(* Local properties of a basic block, computed for the LCM dataflow analyses.

   A conversion (kind, v) is identified by its operation and its operand variable.
   It is "killed" in a block if v is redefined (by Let or Assign) in that block.

   - [transp]: conversions whose operand is never killed in this block (transparent).
   - [comp]: conversions that are computed (appear) in this block and are not
     subsequently killed by a later redefinition of their operand.
     (Downward-exposed computations.)
   - [antloc]: conversions that are computed in this block and whose operand was
     not killed before the computation. (Locally anticipatable: the operand's
     value at block entry reaches the conversion.) *)
type block_props =
  { transp : ConvSet.t
  ; transp_ant : ConvSet.t
  ; comp : ConvSet.t
  ; antloc : ConvSet.t
  }

let remove_conversions_of_var convs v =
  ConvSet.filter (fun (_, arg) -> not (Var.equal arg v)) convs

let remove_killed_mappings map v =
  ConvMap.filter
    (fun (_, arg) mapped -> (not (Var.equal arg v)) && not (Var.equal mapped v))
    map

let compute_local_props all_convs is_safe block =
  let killed_vars = ref VarSet.empty in
  (* Block parameters are definitions. They kill conversions involving them. *)
  List.iter ~f:(fun v -> killed_vars := VarSet.add v !killed_vars) block.params;
  List.iter
    ~f:(function
      | Let (v, _) | Assign (v, _) -> killed_vars := VarSet.add v !killed_vars
      | Set_field _ | Offset_ref _ | Array_set _ | Event _ -> ())
    block.body;
  let transp = ConvSet.filter (fun (_, v) -> not (VarSet.mem v !killed_vars)) all_convs in
  (* transp_ant: restricted transparency for the anticipatability analysis.
     If the block contains any effectful instruction (Apply), unsafe conversions
     cannot be hoisted through it — they might execute on a path where the
     operand's runtime type doesn't match (e.g. with GADTs). *)
  let has_effects =
    List.exists ~f:(function Let (_, Apply _) -> true | _ -> false) block.body
  in
  let transp_ant =
    if has_effects then ConvSet.filter (fun c -> is_safe c) transp else transp
  in
  let comp = ref ConvSet.empty in
  let antloc = ref ConvSet.empty in
  let current_killed = ref VarSet.empty in
  let seen_effect = ref false in
  List.iter ~f:(fun v -> current_killed := VarSet.add v !current_killed) block.params;
  let kill_var v =
    current_killed := VarSet.add v !current_killed;
    comp := remove_conversions_of_var !comp v
  in
  List.iter
    ~f:(function
      | Let (v, Prim (p, [ Pv arg ])) -> (
          match kind_of_prim p with
          | Some kind ->
              let conv = kind, arg in
              if (not (VarSet.mem arg !current_killed))
                 && (is_safe conv || not !seen_effect)
              then antloc := ConvSet.add conv !antloc;
              comp := ConvSet.add conv !comp;
              kill_var v
          | None -> kill_var v)
      | Let (v, Apply _) ->
          seen_effect := true;
          kill_var v
      | Let (v, _) | Assign (v, _) -> kill_var v
      | Set_field _ | Offset_ref _ | Array_set _ | Event _ -> ())
    block.body;
  { transp; transp_ant; comp = !comp; antloc = !antloc }

module CFG = struct
  let successors blocks pc =
    let b = Addr.Map.find pc blocks in
    match b.branch with
    | Return _ | Raise _ | Stop -> []
    | Branch (pc', _) | Poptrap (pc', _) -> [ pc' ]
    | Pushtrap ((pc', _), _, (pc_h, _)) -> [ pc'; pc_h ]
    | Cond (_, (pc1, _), (pc2, _)) -> [ pc1; pc2 ]
    | Switch (_, targets) ->
        Array.to_list (ArrayLabels.map ~f:(fun (pc, _) -> pc) targets)

  let predecessors blocks =
    let preds = ref Addr.Map.empty in
    Addr.Map.iter
      (fun pc _ ->
        let succs = successors blocks pc in
        List.iter
          ~f:(fun succ ->
            let p = Addr.Map.find_opt succ !preds |> Option.value ~default:[] in
            preds := Addr.Map.add succ (pc :: p) !preds)
          succs)
      blocks;
    !preds
end

let split_critical_edges blocks free_pc =
  let preds = CFG.predecessors blocks in
  let has_multiple_preds pc =
    match Addr.Map.find_opt pc preds with
    | Some ps ->
        let unique = List.fold_left ~f:(fun s p -> Addr.Set.add p s) ~init:Addr.Set.empty ps in
        Addr.Set.cardinal unique >= 2
    | None -> false
  in
  let new_blocks = ref Addr.Map.empty in
  let make_split_cont needs_split ((target_pc, _args) as cont) =
    if needs_split target_pc
    then (
      let new_pc = !free_pc in
      free_pc := new_pc + 1;
      new_blocks :=
        Addr.Map.add new_pc { params = []; body = []; branch = Branch cont } !new_blocks;
      (new_pc, []))
    else cont
  in
  let duplicate_targets targets =
    let rec collect seen dups = function
      | [] -> dups
      | pc :: rest ->
          if Addr.Set.mem pc seen
          then collect seen (Addr.Set.add pc dups) rest
          else collect (Addr.Set.add pc seen) dups rest
    in
    collect Addr.Set.empty Addr.Set.empty targets
  in
  let needs_split targets pc =
    has_multiple_preds pc || Addr.Set.mem pc (duplicate_targets targets)
  in
  let split_branch = function
    | Cond (v, ((pc1, _) as cont1), ((pc2, _) as cont2)) ->
        let needs = needs_split [ pc1; pc2 ] in
        Cond (v, make_split_cont needs cont1, make_split_cont needs cont2)
    | Switch (v, conts) ->
        let targets = Array.to_list (Array.map ~f:fst conts) in
        let needs = needs_split targets in
        Switch (v, Array.map ~f:(make_split_cont needs) conts)
    | Pushtrap (((pc1, _) as cont1), v, ((pc2, _) as cont2)) ->
        let needs = needs_split [ pc1; pc2 ] in
        Pushtrap (make_split_cont needs cont1, v, make_split_cont needs cont2)
    | (Branch _ | Poptrap _ | Return _ | Raise _ | Stop) as b -> b
  in
  let blocks =
    Addr.Map.map (fun block -> { block with branch = split_branch block.branch }) blocks
  in
  Addr.Map.union (fun _pc _a b -> Some b) !new_blocks blocks

let apply_subst subst x =
  let rec loop visited subst x =
    match Var.Map.find_opt x subst with
    | Some y ->
        if List.exists ~f:(fun v -> Var.equal v y) visited then failwith "subst cycle";
        loop (x :: visited) subst y
    | None -> x
  in
  loop [] subst x

let reachable_blocks all_blocks entry =
  let visited = ref Addr.Set.empty in
  let rec visit pc =
    if not (Addr.Set.mem pc !visited)
    then (
      visited := Addr.Set.add pc !visited;
      List.iter ~f:visit (CFG.successors all_blocks pc))
  in
  visit entry;
  !visited

(* Phase 3: Peephole cleanup.

   After LCM rewriting, inverse conversion pairs may appear:
     let y = box_f64(x)     -- inserted by LCM or lowering
     let z = unbox_f64(y)   -- original use
   This pass detects such pairs and substitutes z with x directly,
   removing the dead intermediate definitions. *)
let optimize_peephole_conversions blocks =
  let defs = Var.Tbl.make () None in
  let subst = ref Var.Map.empty in
  Addr.Map.iter
    (fun _ block ->
      List.iter
        ~f:(function
          | Let (x, Prim (p, [ Pv y ])) -> (
              match kind_of_prim p with
              | Some k -> Var.Tbl.set defs x (Some (k, y))
              | None -> ())
          | _ -> ())
        block.body)
    blocks;
  Var.Tbl.iter
    (fun x opt ->
      match opt with
      | Some (k1, y) -> (
          match Var.Tbl.get defs y with
          | Some (k2, z) when Poly.equal (inverse_kind k1) (Some k2) ->
              subst := Var.Map.add x z !subst
          | _ -> ())
      | None -> ())
    defs;
  if Var.Map.is_empty !subst
  then blocks
  else
    let subst_var v = apply_subst !subst v in
    let subst_arg = function
      | Pv v -> Pv (subst_var v)
      | Pc c -> Pc c
    in
    Addr.Map.map
      (fun block ->
        let body =
          List.filter_map
            ~f:(function
              | Let (x, _) when Var.Map.mem x !subst -> None
              | Let (x, Apply { f; args; exact }) ->
                  Some
                    (Let
                       ( x
                       , Apply
                           { f = subst_var f; args = List.map ~f:subst_var args; exact }
                       ))
              | Let (x, Block (idx, arr, aon, mut)) ->
                  Some (Let (x, Block (idx, Array.map ~f:subst_var arr, aon, mut)))
              | Let (x, Closure (lst1, (pc, lst2), cc)) ->
                  Some
                    (Let
                       ( x
                       , Closure
                           ( List.map ~f:subst_var lst1
                           , (pc, List.map ~f:subst_var lst2)
                           , cc ) ))
              | Let (x, Field (y, n, k)) -> Some (Let (x, Field (subst_var y, n, k)))
              | Let (x, Prim (p, args)) ->
                  Some (Let (x, Prim (p, List.map ~f:subst_arg args)))
              | Assign (x, y) -> Some (Assign (x, subst_var y))
              | Array_set (x, y, z) ->
                  Some (Array_set (subst_var x, subst_var y, subst_var z))
              | Set_field (x, n, k, y) ->
                  Some (Set_field (subst_var x, n, k, subst_var y))
              | Offset_ref (x, n) -> Some (Offset_ref (subst_var x, n))
              | Event loc -> Some (Event loc)
              | i -> Some i)
            block.body
        in
        let branch =
          match block.branch with
          | Return y -> Return (subst_var y)
          | Raise (y, l) -> Raise (subst_var y, l)
          | Branch (pc, args) -> Branch (pc, List.map ~f:subst_var args)
          | Cond (v, (pc1, args1), (pc2, args2)) ->
              Cond
                ( subst_var v
                , (pc1, List.map ~f:subst_var args1)
                , (pc2, List.map ~f:subst_var args2) )
          | Switch (v, targets) ->
              Switch
                ( subst_var v
                , Array.map ~f:(fun (pc, args) -> pc, List.map ~f:subst_var args) targets
                )
          | Pushtrap ((pc1, args1), v, (pc2, args2)) ->
              Pushtrap
                ((pc1, List.map ~f:subst_var args1), v, (pc2, List.map ~f:subst_var args2))
          | Poptrap (pc, args) -> Poptrap (pc, List.map ~f:subst_var args)
          | Stop -> Stop
        in
        { block with body; branch })
      blocks

(* Phase 4: Partial Dead Code Elimination (PDE) for conversions.

   After LCM and peephole cleanup, a conversion placed at a branch point may be
   partially dead — used on some successor paths but not others. PDE sinks such
   conversions to the paths where they are actually needed.

   This is the dual of PRE: PRE hoists to eliminate redundancy, PDE sinks to
   eliminate partial deadness. Only the simple case is handled: sink when the
   result is live on exactly one successor edge, or remove when live on none. *)
let sink_partially_dead_conversions blocks all_blocks entry =
  (* Collect conversion result variables *)
  let conv_vars = ref VarSet.empty in
  Addr.Map.iter
    (fun _ block ->
      List.iter
        ~f:(function
          | Let (v, Prim (p, [ Pv _ ])) -> (
              match kind_of_prim p with
              | Some _ -> conv_vars := VarSet.add v !conv_vars
              | None -> ())
          | _ -> ())
        block.body)
    blocks;
  let conv_vars = !conv_vars in
  if VarSet.is_empty conv_vars
  then blocks
  else
    (* Collect conv result vars that are free in nested closure bodies.
       These must not be removed or sunk, since their uses in the closure
       body are invisible to the function-scoped liveness analysis. *)
    let escaping_vars =
      let escaped = ref VarSet.empty in
      Addr.Map.iter
        (fun _ block ->
          List.iter
            ~f:(function
              | Let (_, Closure (_, (pc, _), _)) ->
                  let closure_pcs = reachable_blocks all_blocks pc in
                  Addr.Set.iter
                    (fun cpc ->
                      let cblock = Addr.Map.find cpc all_blocks in
                      Freevars.iter_block_free_vars
                        (fun x ->
                          if VarSet.mem x conv_vars
                          then escaped := VarSet.add x !escaped)
                        cblock)
                    closure_pcs
              | _ -> ())
            block.body)
        blocks;
      !escaped
    in
    let is_conv v = VarSet.mem v conv_vars in
    let add_cv acc v = if is_conv v then VarSet.add v acc else acc in
    let add_pv acc = function Pv v -> add_cv acc v | Pc _ -> acc in
    (* Collect conv result vars referenced in an instruction's operands *)
    let used_in_instr i =
      match i with
      | Let (_, Prim (_, args)) -> List.fold_left ~f:add_pv ~init:VarSet.empty args
      | Let (_, Apply { f; args; _ }) ->
          List.fold_left ~f:add_cv ~init:(add_cv VarSet.empty f) args
      | Let (_, Field (x, _, _)) -> add_cv VarSet.empty x
      | Let (_, Block (_, arr, _, _)) ->
          Array.fold_left ~f:add_cv ~init:VarSet.empty arr
      | Let (_, Closure _) -> VarSet.empty
      | Let (_, (Constant _ | Special _)) -> VarSet.empty
      | Assign (_, y) -> add_cv VarSet.empty y
      | Set_field (x, _, _, y) -> add_cv (add_cv VarSet.empty x) y
      | Array_set (x, y, z) -> add_cv (add_cv (add_cv VarSet.empty x) y) z
      | Offset_ref (x, _) -> add_cv VarSet.empty x
      | Event _ -> VarSet.empty
    in
    let used_in_branch br =
      let add_args acc args = List.fold_left ~f:add_cv ~init:acc args in
      match br with
      | Return y -> add_cv VarSet.empty y
      | Raise (y, _) -> add_cv VarSet.empty y
      | Branch (_, args) -> add_args VarSet.empty args
      | Cond (v, (_, a1), (_, a2)) ->
          add_args (add_args (add_cv VarSet.empty v) a1) a2
      | Switch (v, targets) ->
          Array.fold_left
            ~f:(fun acc (_, args) -> add_args acc args)
            ~init:(add_cv VarSet.empty v)
            targets
      | Pushtrap ((_, a1), _, (_, a2)) -> add_args (add_args VarSet.empty a1) a2
      | Poptrap (_, args) -> add_args VarSet.empty args
      | Stop -> VarSet.empty
    in
    (* Compute USE/DEF per block for backward liveness of conv result vars.
       USE(B) = upward-exposed uses (referenced before defined in B).
       DEF(B) = conv result vars defined in B. *)
    let block_use_def =
      Addr.Map.map
        (fun block ->
          let use = ref VarSet.empty in
          let def = ref VarSet.empty in
          List.iter
            ~f:(fun i ->
              let u = used_in_instr i in
              use := VarSet.union !use (VarSet.diff u !def);
              match i with
              | Let (v, _) | Assign (v, _) ->
                  if is_conv v then def := VarSet.add v !def
              | _ -> ())
            block.body;
          let bu = used_in_branch block.branch in
          use := VarSet.union !use (VarSet.diff bu !def);
          !use, !def)
        blocks
    in
    let preds = CFG.predecessors blocks in
    (* Backward liveness fixpoint:
       LIVE_OUT(B) = ∪ { LIVE_IN(S) | S ∈ successors(B) }
       LIVE_IN(B)  = USE(B) ∪ (LIVE_OUT(B) \ DEF(B)) *)
    let live_in = ref (Addr.Map.map (fun _ -> VarSet.empty) blocks) in
    let wl =
      ref
        (Addr.Map.fold (fun pc _ acc -> Addr.Set.add pc acc) blocks Addr.Set.empty)
    in
    while not (Addr.Set.is_empty !wl) do
      let pc = Addr.Set.max_elt !wl in
      wl := Addr.Set.remove pc !wl;
      let use_b, def_b = Addr.Map.find pc block_use_def in
      let succs = CFG.successors blocks pc in
      let live_out =
        List.fold_left
          ~f:(fun acc s -> VarSet.union acc (Addr.Map.find s !live_in))
          ~init:VarSet.empty
          succs
      in
      let new_li = VarSet.union use_b (VarSet.diff live_out def_b) in
      if not (VarSet.equal (Addr.Map.find pc !live_in) new_li)
      then (
        live_in := Addr.Map.add pc new_li !live_in;
        let ps = Addr.Map.find_opt pc preds |> Option.value ~default:[] in
        List.iter ~f:(fun p -> wl := Addr.Set.add p !wl) ps)
    done;
    (* Sinking pass: RPO traversal (predecessors before successors). *)
    let rpo =
      Structure.blocks_in_reverse_post_order (Structure.build_graph blocks entry)
    in
    let result = ref blocks in
    List.iter
      ~f:(fun pc ->
        let block = Addr.Map.find pc !result in
        let succs = CFG.successors blocks pc in
        if List.length succs >= 2
        then (
          let to_sink = ref [] in
          let to_remove = ref VarSet.empty in
          let body = block.body in
          let body_arr = Array.of_list body in
          let body_len = Array.length body_arr in
          for idx = 0 to body_len - 1 do
            match body_arr.(idx) with
            | Let (v, Prim (p, [ Pv arg ]))
              when Option.is_some (kind_of_prim p)
                   && not (VarSet.mem v escaping_vars) ->
                (* Condition 1: v not used in B after the conversion *)
                let used_after = ref false in
                for j = idx + 1 to body_len - 1 do
                  if VarSet.mem v (used_in_instr body_arr.(j)) then used_after := true
                done;
                if VarSet.mem v (used_in_branch block.branch) then used_after := true;
                if not !used_after
                then (
                  (* Condition 3: v live on a strict subset of successor edges *)
                  let succs_live =
                    List.filter
                      ~f:(fun s -> VarSet.mem v (Addr.Map.find s !live_in))
                      succs
                  in
                  match succs_live with
                  | [] ->
                      (* Dead on all paths: remove *)
                      to_remove := VarSet.add v !to_remove
                  | [ target ] ->
                      let target_block = Addr.Map.find target !result in
                      (* Condition 4: arg ∉ target.params *)
                      let arg_in_params =
                        List.mem ~eq:Var.equal arg target_block.params
                      in
                      (* Condition 5: arg not killed after conversion in B *)
                      let arg_killed = ref false in
                      for j = idx + 1 to body_len - 1 do
                        (match body_arr.(j) with
                        | Let (w, _) | Assign (w, _) when Var.equal w arg ->
                            arg_killed := true
                        | _ -> ())
                      done;
                      if (not arg_in_params) && not !arg_killed
                      then to_sink := (body_arr.(idx), target) :: !to_sink
                  | _ -> (* live on multiple successors, don't sink *) ())
            | _ -> ()
          done;
          if (not (List.is_empty !to_sink)) || not (VarSet.is_empty !to_remove)
          then (
            let sunk_vars =
              List.fold_left
                ~f:(fun acc (i, _) ->
                  match i with Let (v, _) -> VarSet.add v acc | _ -> acc)
                ~init:VarSet.empty
                !to_sink
            in
            let remove_set = VarSet.union sunk_vars !to_remove in
            let new_body =
              List.filter
                ~f:(function
                  | Let (v, _) -> not (VarSet.mem v remove_set)
                  | _ -> true)
                body
            in
            result := Addr.Map.add pc { block with body = new_body } !result;
            (* Insert sunk instructions at the top of their target blocks.
               to_sink is in reverse body order, so List.iter prepends them
               back into the correct (original) order. *)
            List.iter
              ~f:(fun (instr, target_pc) ->
                let tb = Addr.Map.find target_pc !result in
                result :=
                  Addr.Map.add target_pc { tb with body = instr :: tb.body } !result)
              !to_sink)))
      rpo;
    !result

(* Phase 5: Conversion elimination through parameter widening.

   When a block converts a block parameter (e.g., unbox_f64(p) where p is a
   param), we can eliminate the conversion by passing the converted value as an
   additional parameter. This is profitable when all predecessors already have the
   converted value available — either as the operand of an inverse conversion
   (box/unbox pair across a block boundary) or as an already-computed result.

   When a predecessor merely passes a block parameter through (passthrough), we
   recursively check its predecessors too, accumulating "shadow parameter" slots
   along the chain. Cycles (loop back-edges) are handled optimistically: if we
   encounter a block already on the DFS stack, we assume it will get a shadow.

   Example: a loop header receives a boxed float and immediately unboxes it for
   arithmetic. The loop tail boxes the result and passes it back. By adding an
   unboxed parameter to both the header and the tail, the box-unbox pair across
   the iteration boundary is eliminated. *)
let eliminate_param_conversions blocks types =
  let result = ref blocks in
  let did_change = ref true in
  while !did_change do
    did_change := false;
    let preds = CFG.predecessors !result in
    (* Build conversion definition and computed-conversion tables *)
    let conv_defs = Var.Tbl.make () None in
    let block_computed = ref Addr.Map.empty in
    Addr.Map.iter
      (fun pc block ->
        let computed = ref ConvMap.empty in
        List.iter
          ~f:(function
            | Let (x, Prim (p, [ Pv y ])) -> (
                match kind_of_prim p with
                | Some k ->
                    Var.Tbl.set conv_defs x (Some (k, y));
                    computed := ConvMap.add (k, y) x !computed
                | None -> ())
            | _ -> ())
          block.body;
        block_computed := Addr.Map.add pc !computed !block_computed)
      !result;
    (* Check if a predecessor can directly supply kind(q):
       - Inverse pair: q = inv_kind(u) → supply u
       - Already computed: kind(q) computed in pred block *)
    let find_direct_value pred_pc q kind =
      (match inverse_kind kind with
      | Some inv_k -> (
          match Var.Tbl.get conv_defs q with
          | Some (k, u) when Poly.equal k inv_k -> Some u
          | _ -> None)
      | None -> None)
      |> function
      | Some _ as r -> r
      | None -> (
          match Addr.Map.find_opt pred_pc !block_computed with
          | Some m -> ConvMap.find_opt (kind, q) m
          | None -> None)
    in
    let find_param_idx x params =
      let rec loop i = function
        | [] -> None
        | p :: _ when Var.equal p x -> Some i
        | _ :: rest -> loop (i + 1) rest
      in
      loop 0 params
    in
    (* Collect all continuations from a block's branch that target a given pc *)
    let conts_to_target branch target_pc =
      let acc = ref [] in
      (match branch with
      | Branch (tpc, args) -> if tpc = target_pc then acc := args :: !acc
      | Cond (_, (t1, a1), (t2, a2)) ->
          if t1 = target_pc then acc := a1 :: !acc;
          if t2 = target_pc then acc := a2 :: !acc
      | Switch (_, cs) ->
          Array.iter ~f:(fun (t, a) -> if t = target_pc then acc := a :: !acc) cs
      | Pushtrap ((t1, a1), _, (t2, a2)) ->
          if t1 = target_pc then acc := a1 :: !acc;
          if t2 = target_pc then acc := a2 :: !acc
      | Poptrap (t, a) -> if t = target_pc then acc := a :: !acc
      | Return _ | Raise _ | Stop -> ());
      !acc
    in
    (* DFS: check whether all predecessors of block bpc can supply kind(arg)
       where arg is what they pass at param index bpi. Discovers passthrough
       blocks that need shadow parameters along the way.

       visiting: set of (bpc, bpi) on the DFS stack for cycle detection.
       Returns Some shadows_needed or None on failure.
       shadows_needed: list of (block_pc, param_idx) that need shadow params
       (includes the initial (bpc, bpi) passed to the top-level call). *)
    let rec can_supply_all bpc bpi kind visiting =
      let pred_pcs =
        Addr.Map.find_opt bpc preds
        |> Option.value ~default:[]
        |> List.sort_uniq ~cmp:compare
      in
      if List.is_empty pred_pcs
      then None
      else
        let rec check_preds ps acc_shadows =
          match ps with
          | [] -> Some acc_shadows
          | pred_pc :: rest_preds ->
              let pred_block = Addr.Map.find pred_pc !result in
              let conts = conts_to_target pred_block.branch bpc in
              let rec check_conts cs acc =
                match cs with
                | [] -> Some acc
                | args :: rest_conts ->
                    let q = List.nth args bpi in
                    (match find_direct_value pred_pc q kind with
                    | Some _ -> check_conts rest_conts acc
                    | None ->
                        (* q not directly available; check if it's a param
                           of pred that we can recurse through *)
                        let q_idx = find_param_idx q pred_block.params in
                        (match q_idx with
                        | None -> None (* can't supply *)
                        | Some qi ->
                            let key = pred_pc, qi in
                            if List.exists
                                 ~f:(fun (a, b) -> a = pred_pc && b = qi)
                                 visiting
                            then
                              (* Cycle: assume shadow will be there *)
                              let acc' =
                                if List.exists
                                     ~f:(fun (a, b) -> a = pred_pc && b = qi)
                                     acc
                                then acc
                                else key :: acc
                              in
                              check_conts rest_conts acc'
                            else
                              let acc' =
                                if List.exists
                                     ~f:(fun (a, b) -> a = pred_pc && b = qi)
                                     acc
                                then acc
                                else key :: acc
                              in
                              (match
                                 can_supply_all pred_pc qi kind (key :: visiting)
                               with
                              | None -> None
                              | Some more ->
                                  let combined =
                                    List.fold_left
                                      ~f:(fun a ((spc, si) as s) ->
                                        if List.exists
                                             ~f:(fun (a2, b2) ->
                                               a2 = spc && b2 = si)
                                             a
                                        then a
                                        else s :: a)
                                      ~init:acc'
                                      more
                                  in
                                  check_conts rest_conts combined)))
              in
              (match check_conts conts acc_shadows with
              | None -> None
              | Some acc' -> check_preds rest_preds acc')
        in
        check_preds pred_pcs []
    in
    (* Find and apply the first applicable transformation, then restart *)
    let found = ref false in
    Addr.Map.iter
      (fun pc _ ->
        if !found then ()
        else
          let block = Addr.Map.find pc !result in
          if List.is_empty block.params
          then ()
          else
            List.iter
              ~f:(fun instr ->
                if !found
                then ()
                else
                  match instr with
                  | Let (v, Prim (p, [ Pv param_var ])) -> (
                      match kind_of_prim p with
                      | Some kind -> (
                          let param_idx = find_param_idx param_var block.params in
                          match param_idx with
                          | None -> ()
                          | Some param_idx -> (
                              let initial_key = pc, param_idx in
                              match
                                can_supply_all
                                  pc
                                  param_idx
                                  kind
                                  [ initial_key ]
                              with
                              | None -> ()
                              | Some passthrough_shadows ->
                                  (* Success: apply the transformation.
                                     All blocks in initial_key :: passthrough_shadows
                                     get a shadow parameter appended. *)
                                  let all_shadows = initial_key :: passthrough_shadows in
                                  found := true;
                                  did_change := true;
                                  (* 1. Create shadow param variables *)
                                  let shadow_vars =
                                    List.map
                                      ~f:(fun (bpc, bpi) ->
                                        let sv = Var.fresh () in
                                        Typing.set_var_type
                                          types
                                          sv
                                          (type_of_kind kind);
                                        bpc, bpi, sv)
                                      all_shadows
                                  in
                                  let find_shadow_var bpc bpi =
                                    let _, _, sv =
                                      List.find
                                        ~f:(fun (a, b, _) ->
                                          a = bpc && b = bpi)
                                        shadow_vars
                                    in
                                    sv
                                  in
                                  (* 2. Append shadow params to each block *)
                                  List.iter
                                    ~f:(fun (bpc, _bpi, sv) ->
                                      let b = Addr.Map.find bpc !result in
                                      result :=
                                        Addr.Map.add
                                          bpc
                                          { b with params = b.params @ [ sv ] }
                                          !result)
                                    shadow_vars;
                                  (* 3. For the target block, substitute v with
                                     its shadow and remove the conversion *)
                                  let target_sv = find_shadow_var pc param_idx in
                                  let b = Addr.Map.find pc !result in
                                  let subst_v x =
                                    if Var.equal x v then target_sv else x
                                  in
                                  let new_body =
                                    List.filter_map
                                      ~f:(function
                                        | Let (x, _) when Var.equal x v -> None
                                        | i ->
                                            Some
                                              (Subst.Excluding_Binders.instr
                                                 subst_v
                                                 i))
                                      b.body
                                  in
                                  let new_branch =
                                    Subst.Excluding_Binders.last subst_v b.branch
                                  in
                                  result :=
                                    Addr.Map.add
                                      pc
                                      { b with
                                        body = new_body
                                      ; branch = new_branch
                                      }
                                      !result;
                                  (* Update block_computed: the eliminated
                                     conversion's result v is now target_sv *)
                                  (match Addr.Map.find_opt pc !block_computed with
                                  | Some bc ->
                                      let bc' =
                                        ConvMap.map
                                          (fun w ->
                                            if Var.equal w v then target_sv
                                            else w)
                                          bc
                                      in
                                      block_computed :=
                                        Addr.Map.add pc bc' !block_computed
                                  | None -> ());
                                  (* 4. Update predecessor branches for each
                                     shadow block to pass the shadow value *)
                                  List.iter
                                    ~f:(fun (bpc, bpi, _sv) ->
                                      let bpc_preds =
                                        Addr.Map.find_opt bpc preds
                                        |> Option.value ~default:[]
                                        |> List.sort_uniq ~cmp:compare
                                      in
                                      List.iter
                                        ~f:(fun pred_pc ->
                                          let pb =
                                            Addr.Map.find pred_pc !result
                                          in
                                          let ext ((tpc, args) as cont) =
                                            if tpc <> bpc
                                            then cont
                                            else
                                              let q = List.nth args bpi in
                                              let shadow_val =
                                                match
                                                  find_direct_value
                                                    pred_pc
                                                    q
                                                    kind
                                                with
                                                | Some u -> u
                                                | None ->
                                                    (* q is a param of pred
                                                       with a shadow *)
                                                    let pred_block =
                                                      Addr.Map.find
                                                        pred_pc
                                                        !result
                                                    in
                                                    let qi =
                                                      match
                                                        find_param_idx
                                                          q
                                                          pred_block.params
                                                      with
                                                      | Some i -> i
                                                      | None -> assert false
                                                    in
                                                    find_shadow_var pred_pc qi
                                              in
                                              tpc, args @ [ shadow_val ]
                                          in
                                          let br =
                                            match pb.branch with
                                            | Branch c -> Branch (ext c)
                                            | Cond (w, c1, c2) ->
                                                Cond (w, ext c1, ext c2)
                                            | Switch (w, cs) ->
                                                Switch
                                                  (w, Array.map ~f:ext cs)
                                            | Pushtrap (c1, w, c2) ->
                                                Pushtrap (ext c1, w, ext c2)
                                            | Poptrap c -> Poptrap (ext c)
                                            | br -> br
                                          in
                                          result :=
                                            Addr.Map.add
                                              pred_pc
                                              { pb with branch = br }
                                              !result)
                                        bpc_preds)
                                    shadow_vars))
                      | None -> ())
                  | _ -> ())
              block.body)
      !result
  done;
  !result

(* Phase 2: LCM dataflow analysis and rewrite for a single function.

   Given the explicit conversion instructions produced by lowering, compute the
   optimal placement using five dataflow analyses, then rewrite the IR.

   The analysis must be per-function to avoid cross-function variable references
   in the inserted conversion instructions. *)
let process_function types conv_types entry fun_blocks return_type all_blocks =
  let all_convs, local_conv_types = get_all_conversions fun_blocks types in
  ConvMap.iter
    (fun conv typ -> conv_types := ConvMap.add conv typ !conv_types)
    local_conv_types;
  if ConvSet.is_empty all_convs
  then fun_blocks
  else
    let is_safe (kind, var) =
      is_safe_input kind (Typing.var_type types var)
    in
    let props = Addr.Map.map (compute_local_props all_convs is_safe) fun_blocks in
    let preds = CFG.predecessors fun_blocks in
    (* Step 1: Anticipatability (backward dataflow, all-paths).

       ANTIN(b) = set of conversions that will definitely be computed on every
       path from b to the function exit, before their operand is redefined.

       A conversion is anticipatable at a point if it is safe (and useful) to
       move its computation to that point.

       Equation: ANTIN(b) = ANTLOC(b) ∪ (TRANSP(b) ∩ ANTOUT(b))
                 ANTOUT(b) = ∩ { ANTIN(s) | s ∈ successors(b) }

       Initialised to the universal set (all_convs) and iterated to a fixpoint.
       Conversions referencing a successor's block parameters are excluded from
       ANTOUT since those variables are rebound at the block boundary. *)
    let antin = ref (Addr.Map.map (fun _ -> all_convs) fun_blocks) in
    let worklist =
      ref (Addr.Map.fold (fun pc _ acc -> Addr.Set.add pc acc) fun_blocks Addr.Set.empty)
    in
    while not (Addr.Set.is_empty !worklist) do
      let pc = Addr.Set.min_elt !worklist in
      worklist := Addr.Set.remove pc !worklist;
      let b_props = Addr.Map.find pc props in
      let succs = CFG.successors fun_blocks pc in
      let new_antout =
        if List.is_empty succs
        then ConvSet.empty
        else
          List.fold_left
            ~f:(fun acc succ ->
              let succ_antin = Addr.Map.find succ !antin in
              let succ_block = Addr.Map.find succ fun_blocks in
              let valid_succ_antin =
                ConvSet.filter
                  (fun (_, arg) -> not (List.mem ~eq:Var.equal arg succ_block.params))
                  succ_antin
              in
              ConvSet.inter acc valid_succ_antin)
            ~init:all_convs
            succs
      in
      let new_antin =
        ConvSet.union b_props.antloc (ConvSet.inter b_props.transp_ant new_antout)
      in
      if not (ConvSet.equal (Addr.Map.find pc !antin) new_antin)
      then (
        antin := Addr.Map.add pc new_antin !antin;
        let ps = Addr.Map.find_opt pc preds |> Option.value ~default:[] in
        List.iter ~f:(fun p' -> worklist := Addr.Set.add p' !worklist) ps)
    done;
    (* Step 2: Availability (forward dataflow, all-paths).

       AVOUT(b) = set of conversions that have been computed on every path from
       the function entry to the exit of b, without their operand being redefined.

       Equation: AVIN(b)  = ∩ { AVOUT(p) | p ∈ predecessors(b) }
                 AVOUT(b) = COMP(b) ∪ (TRANSP(b) ∩ AVIN(b))

       The entry block is initialised to empty (nothing is available on entry).

       EARLIEST(b) = ANTIN(b) \ AVIN(b)
       A conversion is earliest at b if it is anticipated there but not yet
       available — this is the first point where inserting it is both useful
       and correct. *)
    let avout = ref (Addr.Map.map (fun _ -> all_convs) fun_blocks) in
    avout := Addr.Map.add entry ConvSet.empty !avout;
    worklist :=
      Addr.Map.fold (fun pc _ acc -> Addr.Set.add pc acc) fun_blocks Addr.Set.empty;
    while not (Addr.Set.is_empty !worklist) do
      let pc = Addr.Set.min_elt !worklist in
      worklist := Addr.Set.remove pc !worklist;
      let b_props = Addr.Map.find pc props in
      let ps = Addr.Map.find_opt pc preds |> Option.value ~default:[] in
      let new_avin =
        if pc = entry || List.is_empty ps
        then ConvSet.empty
        else
          List.fold_left
            ~f:(fun acc p' -> ConvSet.inter acc (Addr.Map.find p' !avout))
            ~init:all_convs
            ps
      in
      let new_avout =
        ConvSet.union b_props.comp (ConvSet.inter b_props.transp new_avin)
      in
      if not (ConvSet.equal (Addr.Map.find pc !avout) new_avout)
      then (
        avout := Addr.Map.add pc new_avout !avout;
        let succs = CFG.successors fun_blocks pc in
        List.iter ~f:(fun s -> worklist := Addr.Set.add s !worklist) succs)
    done;
    let avin =
      Addr.Map.mapi
        (fun pc _ ->
          let ps = Addr.Map.find_opt pc preds |> Option.value ~default:[] in
          if pc = entry || List.is_empty ps
          then ConvSet.empty
          else
            List.fold_left
              ~f:(fun acc p' -> ConvSet.inter acc (Addr.Map.find p' !avout))
              ~init:all_convs
              ps)
        fun_blocks
    in
    let earliest =
      Addr.Map.mapi
        (fun pc _ -> ConvSet.diff (Addr.Map.find pc !antin) (Addr.Map.find pc avin))
        fun_blocks
    in
    (* Step 3: Delayability (forward dataflow, all-paths).

       DELAYIN(b) = set of conversions whose earliest placement can be delayed
       from their earliest point down to the entry of b without missing any use.

       Equation: DELAYIN(b) = EARLIEST(b) ∪ (∩ { DELAYOUT(p) | p ∈ preds(b) })
                 DELAYOUT(b) = DELAYIN(b) \ COMP(b)

       A conversion can be delayed past a block as long as the block does not
       use (compute) it. This pushes insertions as late as possible. *)
    let delayin = ref earliest in
    let delayout = ref (Addr.Map.map (fun _ -> all_convs) fun_blocks) in
    worklist :=
      Addr.Map.fold (fun pc _ acc -> Addr.Set.add pc acc) fun_blocks Addr.Set.empty;
    while not (Addr.Set.is_empty !worklist) do
      let pc = Addr.Set.min_elt !worklist in
      worklist := Addr.Set.remove pc !worklist;
      let b_props = Addr.Map.find pc props in
      let ps = Addr.Map.find_opt pc preds |> Option.value ~default:[] in
      let new_delayin =
        if pc = entry || List.is_empty ps
        then Addr.Map.find pc earliest
        else
          ConvSet.union
            (Addr.Map.find pc earliest)
            (List.fold_left
               ~f:(fun acc p' -> ConvSet.inter acc (Addr.Map.find p' !delayout))
               ~init:all_convs
               ps)
      in
      delayin := Addr.Map.add pc new_delayin !delayin;
      let new_delayout = ConvSet.diff new_delayin b_props.comp in
      if not (ConvSet.equal (Addr.Map.find pc !delayout) new_delayout)
      then (
        delayout := Addr.Map.add pc new_delayout !delayout;
        let succs = CFG.successors fun_blocks pc in
        List.iter ~f:(fun s -> worklist := Addr.Set.add s !worklist) succs)
    done;
    (* Step 4: Latest (derived, no iteration needed).

       LATEST(b) = DELAYIN(b) ∩ (COMP(b) ∪ ¬(∩ { DELAYIN(s) | s ∈ succs(b) }))

       A conversion is latest at b if it is delayable to b and either:
       - b uses (computes) the conversion, or
       - some successor cannot accept further delay (the conversion is not
         delayable into all successors).

       This is the optimal insertion point: as late as possible while still
       covering all uses. *)
    let latest =
      Addr.Map.mapi
        (fun pc _block ->
          let succs = CFG.successors fun_blocks pc in
          let delayin_pc = Addr.Map.find pc !delayin in
          let b_props = Addr.Map.find pc props in
          let delayin_succs_intersect =
            if List.is_empty succs
            then ConvSet.empty
            else
              List.fold_left
                ~f:(fun acc s -> ConvSet.inter acc (Addr.Map.find s !delayin))
                ~init:all_convs
                succs
          in
          ConvSet.inter
            delayin_pc
            (ConvSet.union b_props.comp (ConvSet.diff all_convs delayin_succs_intersect)))
        fun_blocks
    in
    (* Step 5: Isolation (forward dataflow, all-paths).

       ISOLATEDIN(b) = LATEST(b) ∪ (ISOLATEDOUT(b) \ COMP(b))
       ISOLATEDOUT(b) = ∩ { ISOLATEDIN(s) | s ∈ successors(b) }

       A conversion is isolated at b if, after being inserted at its latest
       point, it is never used again before the next insertion point. Isolated
       conversions are not worth inserting — they would create a new temporary
       without eliminating any redundancy.

       The final insertion set is LATEST(b) \ ISOLATED(b): conversions placed
       at their optimal point that actually eliminate at least one redundant
       occurrence downstream. *)
    let isolatedout = ref (Addr.Map.map (fun _ -> all_convs) fun_blocks) in
    let isolatedin = ref (Addr.Map.map (fun _ -> ConvSet.empty) fun_blocks) in
    worklist :=
      Addr.Map.fold (fun pc _ acc -> Addr.Set.add pc acc) fun_blocks Addr.Set.empty;
    while not (Addr.Set.is_empty !worklist) do
      let pc = Addr.Set.min_elt !worklist in
      worklist := Addr.Set.remove pc !worklist;
      let b_props = Addr.Map.find pc props in
      let succs = CFG.successors fun_blocks pc in
      let new_isolatedout =
        if List.is_empty succs
        then ConvSet.empty
        else
          List.fold_left
            ~f:(fun acc s ->
              let s_isolatedin = Addr.Map.find s !isolatedin in
              let succ_block = Addr.Map.find s fun_blocks in
              let valid_s_isolatedin =
                ConvSet.filter
                  (fun (_, arg) -> not (List.mem ~eq:Var.equal arg succ_block.params))
                  s_isolatedin
              in
              ConvSet.inter acc valid_s_isolatedin)
            ~init:all_convs
            succs
      in
      isolatedout := Addr.Map.add pc new_isolatedout !isolatedout;
      let new_isolatedin =
        ConvSet.union
          (Addr.Map.find pc latest)
          (ConvSet.diff new_isolatedout b_props.comp)
      in
      if not (ConvSet.equal (Addr.Map.find pc !isolatedin) new_isolatedin)
      then (
        isolatedin := Addr.Map.add pc new_isolatedin !isolatedin;
        let ps = Addr.Map.find_opt pc preds |> Option.value ~default:[] in
        List.iter ~f:(fun p' -> worklist := Addr.Set.add p' !worklist) ps)
    done;

    (* Rewrite phase: walk blocks in reverse post-order and apply the LCM results.

       For each block:
       1. Inherit available conversions from predecessors (conv_in): a mapping
          from (kind, operand) to the variable holding the already-computed result.
          Only conversions that are available (in AVIN) and agree across all
          processed predecessors are inherited.

       2. Insert new conversions at the top of the block for everything in
          LATEST(b) \ ISOLATED(b). Each insertion creates a fresh variable and
          adds it to the conv_to_var mapping.

       3. Walk the block body: for each conversion instruction, check if the
          conv_to_var mapping already has a result for it. If so, record a
          substitution (the original variable maps to the pre-computed one) and
          remove the instruction. Otherwise, keep it and register its result
          in conv_to_var for later uses.

       4. Rewrite the branch: if a branch target expects a converted value and
          the conversion is available in conv_to_var, use the pre-computed result
          directly instead of the unconverted variable. *)
    let rpo =
      Structure.blocks_in_reverse_post_order (Structure.build_graph fun_blocks entry)
    in
    let conv_out_map = ref Addr.Map.empty in
    let phi_info = ref Addr.Map.empty in
    let result = ref fun_blocks in
    List.iter
      ~f:(fun pc ->
        let block = Addr.Map.find pc fun_blocks in
        let preds_pc =
          Addr.Map.find_opt pc preds
          |> Option.value ~default:[]
          |> List.sort_uniq ~cmp:compare
        in
        let processed_preds =
          List.fold_left
            ~f:(fun acc p ->
              match Addr.Map.find_opt p !conv_out_map with
              | Some map -> map :: acc
              | None -> acc)
            ~init:[]
            preds_pc
        in
        let all_preds_processed = List.length processed_preds = List.length preds_pc in
        let conv_in =
          match processed_preds with
          | [] -> ConvMap.empty
          | hd :: tl ->
              List.fold_left
                ~f:(fun acc pred_conv_out ->
                  ConvMap.filter
                    (fun k v ->
                      match ConvMap.find_opt k pred_conv_out with
                      | Some v' when Var.equal v v' -> true
                      | _ -> false)
                    acc)
                ~init:hd
                tl
        in
        let pc_avin = Addr.Map.find pc avin in
        let safe_conv_in = ConvMap.filter (fun k _ -> ConvSet.mem k pc_avin) conv_in in
        (* Handle conversions present in ALL predecessors with different variables:
           create a phi to merge them. Only at fully-processed merge points. *)
        let safe_conv_in =
          if all_preds_processed && List.length preds_pc > 1
          then
            let pred_maps =
              List.filter_map
                ~f:(fun p -> Addr.Map.find_opt p !conv_out_map)
                preds_pc
            in
            match pred_maps with
            | [] | [ _ ] -> safe_conv_in
            | first :: rest ->
                ConvMap.fold
                  (fun conv _var acc ->
                    if ConvMap.mem conv acc
                    then acc (* already in conv_in with same variable *)
                    else if not (ConvSet.mem conv pc_avin)
                    then acc
                    else if
                      (* Check: all preds have it? *)
                      List.for_all ~f:(fun m -> ConvMap.mem conv m) rest
                    then (
                      let kind, _ = conv in
                      let phi_var = Var.fresh () in
                      let typ =
                        ConvMap.find_opt conv !conv_types
                        |> Option.value ~default:(type_of_kind kind)
                      in
                      Typing.set_var_type types phi_var typ;
                      phi_info :=
                        Addr.Map.update
                          pc
                          (function
                            | None -> Some [ conv, phi_var ]
                            | Some l -> Some ((conv, phi_var) :: l))
                          !phi_info;
                      ConvMap.add conv phi_var acc)
                    else acc)
                  first
                  safe_conv_in
          else safe_conv_in
        in
        let to_insert =
          ConvSet.diff (Addr.Map.find pc latest) (Addr.Map.find pc !isolatedout)
        in
        let inserted_rev = ref [] in
        let conv_to_var = ref safe_conv_in in
        (* Partial redundancy elimination: for each conversion in to_insert,
           check if some predecessors already have it. If so, insert only at
           the missing predecessors and create a phi to merge the results.
           This avoids redundant computation on paths that already have it.
           Only at fully-processed non-loop merge points (same guard as the
           all-preds phi insertion above). *)
        let to_insert_remaining = ref ConvSet.empty in
        ConvSet.iter
          (fun ((kind, arg) as conv) ->
            if all_preds_processed && List.length preds_pc > 1
            then (
              let preds_with =
                List.filter
                  ~f:(fun p ->
                    match Addr.Map.find_opt p !conv_out_map with
                    | Some m -> ConvMap.mem conv m
                    | None -> false)
                  preds_pc
              in
              let preds_without =
                List.filter
                  ~f:(fun p ->
                    match Addr.Map.find_opt p !conv_out_map with
                    | Some m -> not (ConvMap.mem conv m)
                    | None -> true)
                  preds_pc
              in
              if (not (List.is_empty preds_with)) && not (List.is_empty preds_without)
              then (
                (* Partial redundancy: insert at missing preds, create phi *)
                let phi_var = Var.fresh () in
                let typ =
                  ConvMap.find_opt conv !conv_types
                  |> Option.value ~default:(type_of_kind kind)
                in
                Typing.set_var_type types phi_var typ;
                List.iter
                  ~f:(fun pred_pc ->
                    let tmp = Var.fresh () in
                    Typing.set_var_type types tmp typ;
                    let instr = Let (tmp, Prim (prim_of_kind kind, [ Pv arg ])) in
                    let pred_block = Addr.Map.find pred_pc !result in
                    result :=
                      Addr.Map.add
                        pred_pc
                        { pred_block with body = pred_block.body @ [ instr ] }
                        !result;
                    let pred_conv_out =
                      Addr.Map.find_opt pred_pc !conv_out_map
                      |> Option.value ~default:ConvMap.empty
                    in
                    conv_out_map :=
                      Addr.Map.add
                        pred_pc
                        (ConvMap.add conv tmp pred_conv_out)
                        !conv_out_map)
                  preds_without;
                phi_info :=
                  Addr.Map.update
                    pc
                    (function
                      | None -> Some [ conv, phi_var ]
                      | Some l -> Some ((conv, phi_var) :: l))
                    !phi_info;
                conv_to_var := ConvMap.add conv phi_var !conv_to_var)
              else to_insert_remaining := ConvSet.add conv !to_insert_remaining)
            else to_insert_remaining := ConvSet.add conv !to_insert_remaining)
          to_insert;
        (* Insert remaining conversions (fully new, no partial redundancy) at block top *)
        ConvSet.iter
          (fun ((kind, arg) as conv) ->
            let tmp = Var.fresh () in
            let typ =
              ConvMap.find_opt conv !conv_types
              |> Option.value ~default:(type_of_kind kind)
            in
            Typing.set_var_type types tmp typ;
            conv_to_var := ConvMap.add conv tmp !conv_to_var;
            inserted_rev :=
              Let (tmp, Prim (prim_of_kind kind, [ Pv arg ])) :: !inserted_rev)
          !to_insert_remaining;
        let subst = ref Var.Map.empty in
        let subst_var x = apply_subst !subst x in
        let body_rev = ref [] in
        List.iter
          ~f:(fun i ->
            let i = Subst.Excluding_Binders.instr subst_var i in
            match i with
            | Let (v, Prim (p, [ Pv arg ])) -> (
                conv_to_var := remove_killed_mappings !conv_to_var v;
                subst := Var.Map.remove v !subst;
                match kind_of_prim p with
                | Some kind -> (
                    let conv = kind, arg in
                    match ConvMap.find_opt conv !conv_to_var with
                    | Some tmp -> subst := Var.Map.add v tmp !subst
                    | None ->
                        body_rev := i :: !body_rev;
                        conv_to_var := ConvMap.add conv v !conv_to_var)
                | None -> body_rev := i :: !body_rev)
            | Let (v, _) ->
                conv_to_var := remove_killed_mappings !conv_to_var v;
                subst := Var.Map.remove v !subst;
                body_rev := i :: !body_rev
            | Assign (v, _) ->
                conv_to_var := remove_killed_mappings !conv_to_var v;
                subst := Var.Map.remove v !subst;
                body_rev := i :: !body_rev
            | Set_field _ | Offset_ref _ | Array_set _ | Event _ ->
                body_rev := i :: !body_rev)
          block.body;
        let branch = Subst.Excluding_Binders.last subst_var block.branch in
        let rewrite_cont (pc', args) =
          let target_block = Addr.Map.find pc' fun_blocks in
          let target_types = List.map ~f:(Typing.var_type types) target_block.params in
          let args' =
            List.map2
              ~f:(fun arg into ->
                let from = Typing.var_type types arg in
                match number_conversion_kind ~from ~into with
                | Some kind -> (
                    let conv = kind, arg in
                    match ConvMap.find_opt conv !conv_to_var with
                    | Some tmp -> tmp
                    | None -> arg)
                | None -> arg)
              args
              target_types
          in
          pc', args'
        in
        let rewrite_branch branch =
          let int_n = Typing.Int Typing.Integer.Normalized in
          match branch with
          | Stop -> branch
          | Return y ->
              let y' =
                match
                  number_conversion_kind ~from:(Typing.var_type types y) ~into:return_type
                with
                | Some kind -> (
                    match ConvMap.find_opt (kind, y) !conv_to_var with
                    | Some tmp -> tmp
                    | None -> y)
                | None -> y
              in
              Return y'
          | Raise (y, l) ->
              let y' =
                match
                  number_conversion_kind ~from:(Typing.var_type types y) ~into:Typing.Top
                with
                | Some kind -> (
                    match ConvMap.find_opt (kind, y) !conv_to_var with
                    | Some tmp -> tmp
                    | None -> y)
                | None -> y
              in
              Raise (y', l)
          | Branch cont -> Branch (rewrite_cont cont)
          | Cond (v, cont1, cont2) ->
              let v' =
                match
                  number_conversion_kind ~from:(Typing.var_type types v) ~into:int_n
                with
                | Some kind -> (
                    match ConvMap.find_opt (kind, v) !conv_to_var with
                    | Some tmp -> tmp
                    | None -> v)
                | None -> v
              in
              Cond (v', rewrite_cont cont1, rewrite_cont cont2)
          | Switch (v, conts) ->
              let v' =
                match
                  number_conversion_kind ~from:(Typing.var_type types v) ~into:int_n
                with
                | Some kind -> (
                    match ConvMap.find_opt (kind, v) !conv_to_var with
                    | Some tmp -> tmp
                    | None -> v)
                | None -> v
              in
              Switch (v', Array.map ~f:rewrite_cont conts)
          | Pushtrap (cont1, v, cont2) ->
              Pushtrap (rewrite_cont cont1, v, rewrite_cont cont2)
          | Poptrap cont -> Poptrap (rewrite_cont cont)
        in
        let branch = rewrite_branch branch in
        let new_block =
          { block with body = List.rev !inserted_rev @ List.rev !body_rev; branch }
        in
        conv_out_map := Addr.Map.add pc !conv_to_var !conv_out_map;
        result := Addr.Map.add pc new_block !result)
      rpo;
    (* Post-pass: patch block params and predecessor branches for phi insertions. *)
    Addr.Map.iter
      (fun target_pc phis ->
        let blk = Addr.Map.find target_pc !result in
        let new_params = blk.params @ List.map ~f:snd phis in
        result := Addr.Map.add target_pc { blk with params = new_params } !result;
        let target_preds =
          Addr.Map.find_opt target_pc preds
          |> Option.value ~default:[]
          |> List.sort_uniq ~cmp:compare
        in
        List.iter
          ~f:(fun pred_pc ->
            let pred_block = Addr.Map.find pred_pc !result in
            let pred_conv_out =
              Addr.Map.find_opt pred_pc !conv_out_map
              |> Option.value ~default:ConvMap.empty
            in
            let extend_cont ((pc', args) as cont) =
              if pc' = target_pc
              then
                let extra =
                  List.map
                    ~f:(fun (conv, _phi_var) ->
                      match ConvMap.find_opt conv pred_conv_out with
                      | Some v -> v
                      | None ->
                          (* Should not happen: conversion is in AVIN so all preds
                             must have it *)
                          let _, arg = conv in
                          arg)
                    phis
                in
                pc', args @ extra
              else cont
            in
            let new_branch =
              match pred_block.branch with
              | Branch cont -> Branch (extend_cont cont)
              | Cond (v, c1, c2) -> Cond (v, extend_cont c1, extend_cont c2)
              | Switch (v, conts) -> Switch (v, Array.map ~f:extend_cont conts)
              | Pushtrap (c1, v, c2) -> Pushtrap (extend_cont c1, v, extend_cont c2)
              | Poptrap cont -> Poptrap (extend_cont cont)
              | (Stop | Return _ | Raise _) as br -> br
            in
            result := Addr.Map.add pred_pc { pred_block with branch = new_branch } !result)
          target_preds)
      !phi_info;
    let result = optimize_peephole_conversions !result in
    let result = sink_partially_dead_conversions result all_blocks entry in
    eliminate_param_conversions result types

(* Entry point. Three steps:
   1. Decide which functions can return unboxed/untagged values for direct calls.
   2. For each function, lower implicit conversions into explicit IR primitives.
   3. Run the LCM analysis and rewrite to eliminate redundant conversions. *)
let f (p : program) (types : Typing.t) ~(global_flow_info : Global_flow.info) ~fun_info =
  (* Decide return-type unboxing for functions whose call sites are all known.
     If a function always returns an unboxed number or a normalised int, record
     that as its return type so callers can avoid re-boxing. *)
  fold_closures
    p
    (fun name_opt _ _ _ () ->
      match name_opt with
      | Some g -> (
          if Typing.can_unbox_parameters fun_info g
          then
            let s = Var.Map.find g global_flow_info.info_return_vals in
            let t =
              Var.Set.fold
                (fun x acc -> Typing.join (Typing.var_type types x) acc)
                s
                Typing.Bot
            in
            match t with
            | Typing.Number (_, Typing.Unboxed) | Typing.Int Typing.Integer.Normalized ->
                Typing.set_return_type types g t
            | Typing.Top
            | Typing.Int Typing.Integer.Ref
            | Typing.Int Typing.Integer.Unnormalized
            | Typing.Number (_, Typing.Boxed)
            | Typing.Tuple _ | Typing.Bigarray _ | Typing.Bot -> ())
      | None -> ())
    ();
  (* Collect function entry points *)
  let fun_entries = ref [ None, p.start ] in
  Addr.Map.iter
    (fun _ block ->
      List.iter
        ~f:(function
          | Let (x, Closure (_, (pc, _), _)) ->
              fun_entries := (Some x, pc) :: !fun_entries
          | _ -> ())
        block.body)
    p.blocks;
  (* Process each function independently to avoid cross-function variable leakage
     in the data flow analysis. *)
  let conv_types = ref ConvMap.empty in
  let blocks = ref p.blocks in
  let free_pc = ref p.free_pc in
  List.iter
    ~f:(fun (name_opt, entry) ->
      let return_type =
        match name_opt with
        | Some f -> Typing.return_type types f
        | None -> Typing.Top
      in
      let fun_block_pcs = reachable_blocks !blocks entry in
      let fun_blocks =
        Addr.Map.filter (fun pc _ -> Addr.Set.mem pc fun_block_pcs) !blocks
      in
      let fun_blocks =
        lower_conversions fun_blocks types global_flow_info return_type free_pc
      in
      let fun_blocks = split_critical_edges fun_blocks free_pc in
      let result =
        process_function types conv_types entry fun_blocks return_type !blocks
      in
      Addr.Map.iter (fun pc block -> blocks := Addr.Map.add pc block !blocks) result)
    !fun_entries;
  let p = { p with blocks = !blocks; free_pc = !free_pc } in
  if debug ()
  then (
    prerr_endline "AFTER";
    Print.program Format.err_formatter (fun _ _ -> "") p);
  p, types
