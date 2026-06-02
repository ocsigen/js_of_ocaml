(* Wasm_of_ocaml compiler
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

(* Lazy Code Motion (LCM) for boxing/unboxing and tagging/untagging conversions.

   In the wasm_of_ocaml backend, the type analysis (typing.ml) assigns unboxed or
   untagged types to variables when profitable. The code generator then inserts
   conversion operations (box/unbox/tag/untag) at every point where a representation
   mismatch occurs: function call boundaries, branches to blocks with differently-typed
   parameters, returns, stores, etc.

   Many of these conversions are redundant or could be hoisted out of loops. For example,
   a loop that repeatedly unboxes a float from an invariant variable will emit the same
   unbox on every iteration. This pass eliminates such redundancy through four phases,
   run independently on each function to avoid cross-function variable references.

   1. **Lowering** ([lower_conversions]): Materialises implicit representation mismatches
      as explicit IR primitives (Wasm_box_*, Wasm_unbox_*, Wasm_tag_int, Wasm_untag_int).
      After this phase, every conversion is a visible instruction that can be analysed.

   2. **LCM dataflow and rewrite** ([process_function]): Applies the classical
      Knoop-Ruthing-Steffen Lazy Code Motion algorithm. LCM finds the optimal placement:
      as early as necessary (to eliminate redundancy) but as late as possible (to avoid
      lengthening lifetimes or executing speculatively). The five dataflow analyses are:
        - Anticipatability: can the conversion be moved to this point?
        - Availability: has the conversion already been computed on all paths?
        - Earliest = anticipated but not yet available
        - Delayability: can we push earliest placements further down?
        - Latest = last point where a delayed insertion is still correct
        - Isolated: is the conversion used only once after insertion?
      The rewrite pass inserts conversions at [latest \ isolated] points and substitutes
      redundant occurrences with the hoisted result.

   3. **Peephole cleanup** ([optimize_peephole_conversions]): Eliminates inverse conversion
      pairs (e.g. box(unbox(x)) → x) that may arise from the LCM rewrite. Also propagates
      conversion info through block parameters so that pairs split across block boundaries
      are detected.

   4. **Parameter widening** ([eliminate_param_conversions]): When a block converts a value
      that arrives as a block parameter, widening threads the already-converted value as an
      additional "shadow" parameter, eliminating the conversion. This handles patterns like
      a loop header that receives a boxed float and immediately unboxes it: by adding an
      unboxed parameter, the box-unbox pair across the loop back-edge is eliminated. A DFS
      checks that all predecessors can supply the converted value (via inverse pairs or
      pre-computed results), accumulating shadow parameter slots along the chain. Cycles
      are handled optimistically.

   The phases are ordered so that each feeds into the next: LCM creates optimal placements
   but may introduce inverse pairs; peephole cleans those up; widening then eliminates the
   conversions of values flowing through block parameters.

   LCM places conversions as late as possible, so no conversion is partially dead
   afterwards: there is no need for a partial dead code elimination phase.

   Reference: J. Knoop, O. Ruthing, B. Steffen, "Lazy Code Motion", PLDI 1992. *)

open! Stdlib
open Code
module VarSet = Var.Set

let debug = Debug.find "lcm"

let times = Debug.find "times"

let stats = Debug.find "stats"

type lcm_stats =
  { mutable functions_processed : int
  ; mutable functions_with_conversions : int
  ; mutable conversions_lowered : int
  ; mutable conversions_tracked : int
  ; mutable ant_iterations : int
  ; mutable avail_iterations : int
  ; mutable delay_iterations : int
  ; mutable isol_iterations : int
  ; mutable conversions_inserted : int
  ; mutable conversions_eliminated : int
  ; mutable peephole_eliminated : int
  ; mutable params_widened : int
  ; mutable time_lowering : float
  ; mutable time_dataflow : float
  ; mutable time_rewrite : float
  ; mutable time_peephole : float
  ; mutable time_widening : float
  ; mutable time_setup : float
  }

let make_stats () =
  { functions_processed = 0
  ; functions_with_conversions = 0
  ; conversions_lowered = 0
  ; conversions_tracked = 0
  ; ant_iterations = 0
  ; avail_iterations = 0
  ; delay_iterations = 0
  ; isol_iterations = 0
  ; conversions_inserted = 0
  ; conversions_eliminated = 0
  ; peephole_eliminated = 0
  ; params_widened = 0
  ; time_lowering = 0.
  ; time_dataflow = 0.
  ; time_rewrite = 0.
  ; time_peephole = 0.
  ; time_widening = 0.
  ; time_setup = 0.
  }

let tick () = Sys.time ()

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
  | Untag_int -> Typing.Int Typing.Integer.Small_normalized
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
      | Typing.Int _ -> true
      | _ -> false)

(* Whether the result of a conversion should not be kept live across a
   call. Untagging a 31-bit integer is a single shift: recomputing it after
   the call is cheaper than keeping the result in a register, which the
   call forces to spill. This does not hold with portable integers, where
   untagging must also handle boxed large integers. *)
let not_live_across_calls (kind, _) =
  match kind with
  | Untag_int -> not (Config.Flag.portable_int ())
  | Unbox_i32 | Unbox_i64 | Unbox_f64 | Box_i32 | Box_i64 | Box_f64 | Tag_int -> false

(* Determine which conversion operation, if any, is needed to go from one
   representation to another. Returns [None] when the representations match
   or no conversion is applicable. The representation lattice itself lives in
   [Typing.conversion_prim]; here we just map the resulting primitive to the
   local [conversion_kind]. *)
let number_conversion_kind ~(from : Typing.typ) ~(into : Typing.typ) =
  match Typing.conversion_prim ~from ~into with
  | Some p -> kind_of_prim p
  | None -> None

let lower_var_conversion ~types ~(from : Typing.typ) ~(into : Typing.typ) x =
  match number_conversion_kind ~from ~into with
  | None -> [], x
  | Some kind ->
      let tmp = Var.fresh () in
      (* An untagged integer is always normalized, even when the use accepts
         an unnormalized one. Otherwise, as the types of the occurrences of a
         conversion are joined when they are merged, a single use accepting
         unnormalized integers would force the other ones to normalize the
         value again. *)
      Typing.set_var_type
        types
        tmp
        (match kind with
        | Untag_int -> type_of_kind kind
        | Unbox_i32 | Unbox_i64 | Unbox_f64 | Box_i32 | Box_i64 | Box_f64 | Tag_int ->
            into);
      [ Let (tmp, Prim (prim_of_kind kind, [ Pv x ])) ], tmp

(* The constant a variable is bound to, if any, according to the global flow
   analysis. Variables created by this pass are not known to it. *)
let constant_of (global_flow_info : Global_flow.info) x =
  let idx = Var.idx x in
  if idx < Array.length global_flow_info.info_defs
  then
    match global_flow_info.info_defs.(idx) with
    | Expr (Constant c) -> Some c
    | Expr _ | Phi _ -> None
  else None

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
    (free_pc : int ref)
    ~(st : lcm_stats) =
  (* A conversion of a number constant is replaced by the constant in the
     target representation: a boxed constant is a static value and an
     unboxed one an immediate. *)
  let lower_var_conversion ~types ~from ~into x =
    let constant =
      match constant_of global_flow_info x with
      | Some (Float _ | Int32 _ | Int64 _ | NativeInt _) as c -> c
      | ((Some (Float32 _) as c) [@if oxcaml]) -> c
      | Some _ | None -> None
    in
    match constant with
    | Some c when Option.is_some (number_conversion_kind ~from ~into) ->
        let tmp = Var.fresh () in
        Typing.set_var_type types tmp into;
        [ Let (tmp, Constant c) ], tmp
    | Some _ | None -> lower_var_conversion ~types ~from ~into x
  in
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
      let int_n = Typing.Int Typing.Integer.Small_normalized in
      (* Integers compared as the code generator does: as 64-bit integers
         with portable integers *)
      let int_cmp =
        if Config.Flag.portable_int ()
        then Typing.Int Typing.Integer.Large_normalized
        else int_n
      in
      match p with
      | Extern (nm, _) -> fst (Typing.prim_sig nm)
      | Array_get -> Some [ top; int_n ]
      | Lt | Le | Ult -> Some [ int_cmp; int_cmp ]
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
  let get_from ~into e =
    match e with
    | Constant _ ->
        (* The code generator emits a constant directly in the representation
           of the variable (a boxed constant is a static value), so there is
           no need to box it at run time. *)
        into
    | Field (_, _, Float) -> Typing.Number (Typing.Float, Typing.Unboxed)
    | Prim (p, _) -> (
        match p with
        | Wasm_unbox_f64 -> Typing.Number (Typing.Float, Typing.Unboxed)
        | Wasm_unbox_i32 -> Typing.Number (Typing.Int32, Typing.Unboxed)
        | Wasm_unbox_i64 -> Typing.Number (Typing.Int64, Typing.Unboxed)
        | Wasm_untag_int | Lt | Le | Ult | IsInt | Eq | Neq | Not | Vectlength _ ->
            Typing.Int Typing.Integer.Small_normalized
        | Extern (nm, _) ->
            let t = snd (Typing.prim_sig nm) in
            (* For context-dependent prims (e.g. caml_ba_get_N), prim_sig
               returns Top because the return type depends on argument types.
               In that case, fall back to the variable's type from the typing
               pass, which was computed with full context. This avoids
               inserting spurious conversions when generate.ml produces
               optimised code matching the variable's type directly. *)
            if Poly.equal t Typing.Top then into else t
        | _ -> Typing.Top)
    | _ -> Typing.Top
    (* [Apply] is handled earlier by [lower_apply] and never reaches here. *)
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
        let int_n = Typing.Int Typing.Integer.Small_normalized in
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
        let into = Typing.var_type types x in
        let from = get_from ~into e in
        let lowered, _ =
          match number_conversion_kind ~from ~into with
          | Some kind ->
              let tmp = Var.fresh () in
              Typing.set_var_type types tmp from;
              let lowered_e =
                match List.rev lowered_e with
                | [] -> assert false
                | last :: rest -> List.rev (replace_assigned x tmp last :: rest)
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
    let int_n = Typing.Int Typing.Integer.Small_normalized in
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
  let result =
    Addr.Map.fold
      (fun pc block acc -> if Addr.Map.mem pc acc then acc else Addr.Map.add pc block acc)
      !new_blocks_map
      blocks
  in
  Addr.Map.iter
    (fun _ block ->
      List.iter
        ~f:(function
          | Let (_, Prim (p, [ Pv _ ])) when Option.is_some (kind_of_prim p) ->
              st.conversions_lowered <- st.conversions_lowered + 1
          | _ -> ())
        block.body)
    result;
  result

(* Variables that are the target of an [Assign]. The parser only assigns
   the variables holding the context of an exception handler, from within
   the body of the corresponding [try], and the handler reads them. They are
   not in SSA form, and the edges from the body to the handler are not
   visible in the CFG (the handler's only predecessor is the [Pushtrap]
   block). Conversions whose operand is one of these variables are thus
   never moved, merged or threaded through block parameters: a converted
   value computed before an assignment would be stale afterwards. *)
let assigned_vars blocks =
  Addr.Map.fold
    (fun _ block acc ->
      List.fold_left
        ~f:(fun acc i ->
          match i with
          | Assign (x, _) -> VarSet.add x acc
          | Let _ | Set_field _ | Offset_ref _ | Array_set _ | Event _ -> acc)
        ~init:acc
        block.body)
    blocks
    VarSet.empty

(* Collect the universe of all conversions that appear in the function,
   along with the join of their result types across all occurrences.
   Conversions of assigned variables are left out, so that LCM does not
   move them. *)
let get_all_conversions blocks types ~assigned =
  let all_convs = ref ConvSet.empty in
  let conv_types = ref ConvMap.empty in
  Addr.Map.iter
    (fun _ block ->
      List.iter
        ~f:(function
          | Let (x, Prim (p, [ Pv v ])) when not (VarSet.mem v assigned) -> (
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

(* A ConvMap with a reverse index from variables to the conv keys they appear in,
   enabling O(log n) removal when a variable is killed instead of O(n) filtering. *)
module ConvTracker : sig
  type t

  val of_map : Var.t ConvMap.t -> t

  val to_map : t -> Var.t ConvMap.t

  val find_opt : Conv.t -> t -> Var.t option

  val add : Conv.t -> Var.t -> t -> t

  val kill_var : Var.t -> t -> t

  val filter : (Conv.t -> bool) -> t -> t
end = struct
  type t =
    { fwd : Var.t ConvMap.t
    ; rev : ConvSet.t Var.Map.t (* var -> set of conv keys mentioning it *)
    }

  let rev_add v conv rev =
    Var.Map.update
      v
      (function
        | None -> Some (ConvSet.singleton conv)
        | Some s -> Some (ConvSet.add conv s))
      rev

  let of_map fwd =
    let rev =
      ConvMap.fold
        (fun ((_, arg) as conv) mapped acc -> rev_add arg conv (rev_add mapped conv acc))
        fwd
        Var.Map.empty
    in
    { fwd; rev }

  let to_map t = t.fwd

  let find_opt conv t = ConvMap.find_opt conv t.fwd

  let add conv mapped t =
    let _, arg = conv in
    let fwd = ConvMap.add conv mapped t.fwd in
    let rev = rev_add arg conv (rev_add mapped conv t.rev) in
    { fwd; rev }

  let kill_var v t =
    match Var.Map.find_opt v t.rev with
    | None -> t
    | Some convs ->
        let fwd =
          ConvSet.fold
            (fun conv fwd ->
              match ConvMap.find_opt conv fwd with
              | Some mapped ->
                  let _, arg = conv in
                  if Var.equal arg v || Var.equal mapped v
                  then ConvMap.remove conv fwd
                  else fwd
              | None -> fwd)
            convs
            t.fwd
        in
        let rev = Var.Map.remove v t.rev in
        { fwd; rev }

  let filter f t = of_map (ConvMap.filter (fun conv _ -> f conv) t.fwd)
end

let compute_local_props all_convs is_safe block =
  let killed_vars = ref VarSet.empty in
  (* Block parameters are definitions. They kill conversions involving them. *)
  List.iter ~f:(fun v -> killed_vars := VarSet.add v !killed_vars) block.params;
  List.iter
    ~f:(function
      | Let (v, _) | Assign (v, _) -> killed_vars := VarSet.add v !killed_vars
      | Set_field _ | Offset_ref _ | Array_set _ | Event _ -> ())
    block.body;
  (* transp_ant: restricted transparency for the anticipatability analysis.
     If the block contains any effectful instruction (Apply), unsafe conversions
     cannot be hoisted through it — they might execute on a path where the
     operand's runtime type doesn't match (e.g. with GADTs). *)
  let has_effects =
    List.exists
      ~f:(function
        | Let (_, Apply _) -> true
        | _ -> false)
      block.body
  in
  (* A call also ends the availability of the conversions whose result
     should not be kept live across calls *)
  let transp =
    ConvSet.filter
      (fun ((_, v) as conv) ->
        (not (VarSet.mem v !killed_vars))
        && not (has_effects && not_live_across_calls conv))
      all_convs
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
              if
                (not (VarSet.mem arg !current_killed))
                && (is_safe conv || not !seen_effect)
                && not (not_live_across_calls conv && !seen_effect)
              then antloc := ConvSet.add conv !antloc;
              comp := ConvSet.add conv !comp;
              kill_var v
          | None -> kill_var v)
      | Let (v, Apply _) ->
          seen_effect := true;
          comp := ConvSet.filter (fun conv -> not (not_live_across_calls conv)) !comp;
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
        let unique =
          List.fold_left ~f:(fun s p -> Addr.Set.add p s) ~init:Addr.Set.empty ps
        in
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
      new_pc, [])
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
  let needs_split targets =
    let dups = duplicate_targets targets in
    fun pc -> has_multiple_preds pc || Addr.Set.mem pc dups
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
  Addr.Map.fold
    (fun pc block acc -> if Addr.Map.mem pc acc then acc else Addr.Map.add pc block acc)
    !new_blocks
    blocks

let apply_subst subst x =
  let rec loop visited subst x =
    match Var.Map.find_opt x subst with
    | Some y ->
        if List.exists ~f:(fun v -> Var.equal v y) visited then failwith "subst cycle";
        loop (x :: visited) subst y
    | None -> x
  in
  loop [] subst x

(* Phase 3: Peephole cleanup.

   After LCM rewriting, inverse conversion pairs may appear:
     let y = box_f64(x)     -- inserted by LCM or lowering
     let z = unbox_f64(y)   -- original use
   This pass detects such pairs and substitutes z with x directly,
   removing the dead intermediate definitions. *)
let optimize_peephole_conversions blocks preds ~assigned ~(st : lcm_stats) =
  let defs = Var.Hashtbl.create 16 in
  let subst = ref Var.Map.empty in
  Addr.Map.iter
    (fun _ block ->
      List.iter
        ~f:(function
          | Let (x, Prim (p, [ Pv y ])) when not (VarSet.mem y assigned) -> (
              match kind_of_prim p with
              | Some k -> Var.Hashtbl.replace defs x (k, y)
              | None -> ())
          | _ -> ())
        block.body)
    blocks;
  (* Propagate conversion info through block parameters: if all predecessors
     pass the same conversion result for a parameter, record that parameter
     as having the conversion's (kind, arg) in defs. This lets the inverse-pair
     detection below catch box/unbox pairs split across block boundaries. *)
  Addr.Map.iter
    (fun pc block ->
      if not (List.is_empty block.params)
      then
        let pred_pcs =
          Addr.Map.find_opt pc preds
          |> Option.value ~default:[]
          |> List.sort_uniq ~cmp:compare
        in
        if not (List.is_empty pred_pcs)
        then (
          (* For each parameter index, collect the argument passed by each
             predecessor. Predecessors may branch to this block multiple
             times (e.g. Cond with both arms targeting the same block). *)
          let param_count = List.length block.params in
          let args_per_param = Array.make param_count [] in
          List.iter
            ~f:(fun pred_pc ->
              let pred_block = Addr.Map.find pred_pc blocks in
              let collect_args (tpc, args) =
                if tpc = pc
                then
                  List.iteri
                    ~f:(fun i arg ->
                      if i < param_count
                      then args_per_param.(i) <- arg :: args_per_param.(i))
                    args
              in
              match pred_block.branch with
              | Branch cont -> collect_args cont
              | Cond (_, c1, c2) ->
                  collect_args c1;
                  collect_args c2
              | Switch (_, cs) -> Array.iter ~f:collect_args cs
              | Pushtrap (c1, _, c2) ->
                  collect_args c1;
                  collect_args c2
              | Poptrap c -> collect_args c
              | Return _ | Raise _ | Stop -> ())
            pred_pcs;
          List.iteri
            ~f:(fun i param ->
              match args_per_param.(i) with
              | _ when VarSet.mem param assigned -> ()
              | [ single_arg ] -> (
                  (* Exactly one incoming edge for this parameter *)
                  match Var.Hashtbl.find_opt defs single_arg with
                  | Some def -> Var.Hashtbl.replace defs param def
                  | None -> ())
              | first :: rest when List.for_all ~f:(fun a -> Var.equal a first) rest -> (
                  (* All edges pass the same variable *)
                  match Var.Hashtbl.find_opt defs first with
                  | Some def -> Var.Hashtbl.replace defs param def
                  | None -> ())
              | _ -> ())
            block.params))
    blocks;
  Var.Hashtbl.iter
    (fun x (k1, y) ->
      match Var.Hashtbl.find_opt defs y with
      | Some (k2, z) when Poly.equal (inverse_kind k1) (Some k2) ->
          subst := Var.Map.add x z !subst
      | _ -> ())
    defs;
  st.peephole_eliminated <- st.peephole_eliminated + Var.Map.cardinal !subst;
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

(* How a conversion's operand enters a block: as the i-th block parameter
   or as a free variable visible from all predecessors. *)
type origin =
  | Param of int
  | Var of Var.t

let equal_origin a b =
  match a, b with
  | Param i, Param j -> i = j
  | Var v1, Var v2 -> Var.equal v1 v2
  | Param _, Var _ | Var _, Param _ -> false

let compare_origin a b =
  match a, b with
  | Param i, Param j -> compare i j
  | Var v1, Var v2 -> Var.compare v1 v2
  | Param _, Var _ -> -1
  | Var _, Param _ -> 1

module AddrOriginSet = Set.Make (struct
  type t = Addr.t * origin

  let compare (a1, o1) (a2, o2) =
    let c = compare a1 a2 in
    if c <> 0 then c else compare_origin o1 o2
end)

(* Phase 4: Conversion elimination through parameter widening.

   When a block converts a value that flows in from predecessors — either as a
   block parameter or as a free variable — we can eliminate the conversion by
   threading the already-converted value as an additional parameter. This is
   profitable when all predecessors already have the converted value available,
   either as the operand of an inverse conversion (box/unbox pair across a block
   boundary) or as an already-computed result.

   The traced variable's [origin] in each block is either [Param i] (it arrives
   as the i-th block parameter) or [Var v] (it is a free variable, the same
   [v] in every predecessor). When a predecessor cannot directly supply the
   converted value, we recursively check its predecessors, accumulating "shadow
   parameter" slots along the chain. Cycles (loop back-edges) are handled
   optimistically: if we encounter a block already on the DFS stack, we assume
   it will get a shadow.

   Example 1 (parameter): a loop header receives a boxed float and immediately
   unboxes it for arithmetic. The loop tail boxes the result and passes it back.
   By adding an unboxed parameter to both the header and the tail, the box-unbox
   pair across the iteration boundary is eliminated.

   Example 2 (free variable): a block boxes a float v0, then branches to two
   successors that each immediately unbox it. The predecessors have v0 available,
   so widening threads v0 as a shadow parameter, eliminating the unbox. *)
let eliminate_param_conversions
    blocks
    types
    preds
    ~assigned
    ~global_flow_info
    ~(st : lcm_stats) =
  let result = ref blocks in
  (* Build conversion definition table, computed-conversion table,
     constant table and candidate list in a single pass over the blocks. *)
  let conv_defs = Var.Hashtbl.create 16 in
  let constants = Var.Hashtbl.create 16 in
  let block_computed = ref Addr.Map.empty in
  let candidates = ref [] in
  Addr.Map.iter
    (fun pc block ->
      let computed = ref ConvMap.empty in
      List.iter
        ~f:(function
          | Let (x, Constant c) -> Var.Hashtbl.replace constants x c
          | Let (x, Prim (p, [ Pv y ])) when not (VarSet.mem y assigned) -> (
              match kind_of_prim p with
              | Some k ->
                  Var.Hashtbl.replace conv_defs x (k, y);
                  computed := ConvMap.add (k, y) x !computed;
                  candidates := (pc, x, k, y) :: !candidates
              | None -> ())
          | Let (_, Apply _) ->
              computed :=
                ConvMap.filter (fun conv _ -> not (not_live_across_calls conv)) !computed
          | _ -> ())
        block.body;
      if not (ConvMap.is_empty !computed)
      then block_computed := Addr.Map.add pc !computed !block_computed)
    !result;
  let candidates = !candidates in
  if List.is_empty candidates
  then blocks
  else
    (* Check if a predecessor can directly supply kind(q):
     - Inverse pair: q = inv_kind(u) → supply u
     - Already computed: kind(q) computed in pred block *)
    let find_direct_value pred_pc q kind =
      (match inverse_kind kind with
        | Some inv_k -> (
            match Var.Hashtbl.find_opt conv_defs q with
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
    (* A predecessor can also supply kind(q) when q is a constant: the
       constant is then materialised in the target representation. *)
    let constant_supply q kind =
      let c =
        match Var.Hashtbl.find_opt constants q with
        | Some _ as c -> c
        | None -> constant_of global_flow_info q
      in
      match c, kind with
      | Some (Float _ as c), (Unbox_f64 | Box_f64)
      | Some (Int32 _ as c), (Unbox_i32 | Box_i32)
      | Some (Int64 _ as c), (Unbox_i64 | Box_i64)
      | Some (Int _ as c), Untag_int -> Some c
      | Some _, _ | None, _ -> None
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
    (* DFS: check whether all predecessors of block bpc can supply kind(q)
     where q is determined by origin:
     - Param bpi: q is what predecessors pass at param index bpi
     - Var v: q is the free variable v (same in all predecessors)

     visited: set of (bpc, origin) already reached during this query. A
     block reached again, either through a cycle or through another path,
     is not explored again: a failure makes the whole query fail, so when
     the query succeeds, the shadows needed by an explored block have been
     collected where it was first reached. This keeps the walk linear
     rather than exponential in the number of join points.
     Returns Some shadows_needed or None on failure.
     shadows_needed: list of (block_pc, origin) that need shadow params
     (includes the initial (bpc, origin) passed to the top-level call). *)
    (* Pre-sort and deduplicate predecessor lists once *)
    let sorted_preds = Addr.Map.map (fun ps -> List.sort_uniq ~cmp:compare ps) preds in
    let rec can_supply bpc origin kind visited =
      let pred_pcs = Addr.Map.find_opt bpc sorted_preds |> Option.value ~default:[] in
      if List.is_empty pred_pcs
      then None
      else
        let rec check_preds ps acc_shadows =
          match ps with
          | [] -> Some acc_shadows
          | pred_pc :: rest_preds -> (
              let pred_block = Addr.Map.find pred_pc !result in
              let conts = conts_to_target pred_block.branch bpc in
              let rec check_conts cs acc =
                match cs with
                | [] -> Some acc
                | args :: rest_conts -> (
                    let q =
                      match origin with
                      | Param bpi -> List.nth args bpi
                      | Var v -> v
                    in
                    match find_direct_value pred_pc q kind with
                    | _ when VarSet.mem q assigned ->
                        (* A shadow of [q] would not follow its assignments *)
                        None
                    | Some _ -> check_conts rest_conts acc
                    | None when Option.is_some (constant_supply q kind) ->
                        check_conts rest_conts acc
                    | None ->
                        (* q not directly available; trace how it enters pred *)
                        let pred_origin =
                          match find_param_idx q pred_block.params with
                          | Some qi -> Param qi
                          | None -> Var q
                        in
                        let key = pred_pc, pred_origin in
                        let acc' = AddrOriginSet.add key acc in
                        if AddrOriginSet.mem key !visited
                        then
                          (* Already reached (possibly a cycle): optimistically
                             assume the shadow will be supplied *)
                          check_conts rest_conts acc'
                        else (
                          visited := AddrOriginSet.add key !visited;
                          match can_supply pred_pc pred_origin kind visited with
                          | None -> None
                          | Some more ->
                              check_conts rest_conts (AddrOriginSet.union acc' more)))
              in
              match check_conts conts acc_shadows with
              | None -> None
              | Some acc' -> check_preds rest_preds acc')
        in
        check_preds pred_pcs AddrOriginSet.empty
    in
    (* Process each candidate. Apply shadow params and branch extensions
     immediately (cheap, targeted), but collect v -> target_sv substitutions
     for a single batched pass at the end. *)
    let eliminated = ref VarSet.empty in
    let all_substs = ref Var.Map.empty in
    List.iter
      ~f:(fun (pc, v, kind, _param_var) ->
        if VarSet.mem v !eliminated
        then ()
        else
          (* Re-read the block and find the conversion (it may have been
           modified by a prior shadow param addition). *)
          let block = Addr.Map.find pc !result in
          let still_present =
            List.find_opt
              ~f:(function
                | Let (x, Prim (p, [ Pv _ ])) ->
                    Var.equal x v && Option.is_some (kind_of_prim p)
                | _ -> false)
              block.body
          in
          match still_present with
          | None -> ()
          | Some (Let (_, Prim (p, [ Pv param_var ]))) -> (
              let kind =
                match kind_of_prim p with
                | Some k -> k
                | None -> kind
              in
              let initial_origin =
                match find_param_idx param_var block.params with
                | Some pi -> Param pi
                | None -> Var param_var
              in
              let initial_key = pc, initial_origin in
              (* Skip Var-origin candidates: when the conversion's operand is
               a free variable (not a block parameter), all predecessors see
               the same variable, so can_supply would need kind(v) to already
               exist in every predecessor — which is rare (0 successes on
               ocamlc) and expensive to check (deep DFS over 3000+ candidates).
               Param-origin candidates may still recurse through Var sub-origins
               internally when tracing the value across blocks. *)
              match initial_origin with
              | Var _ -> ()
              | Param _ -> (
                  match
                    can_supply
                      pc
                      initial_origin
                      kind
                      (ref (AddrOriginSet.singleton initial_key))
                  with
                  | None -> ()
                  | Some passthrough_shadows ->
                      let all_shadows =
                        AddrOriginSet.elements
                          (AddrOriginSet.add initial_key passthrough_shadows)
                      in
                      eliminated := VarSet.add v !eliminated;
                      st.params_widened <- st.params_widened + List.length all_shadows;
                      (* 1. Create shadow param variables *)
                      let shadow_vars =
                        List.map
                          ~f:(fun (bpc, orig) ->
                            let sv = Var.fresh () in
                            Typing.set_var_type types sv (type_of_kind kind);
                            bpc, orig, sv)
                          all_shadows
                      in
                      let find_shadow_var bpc orig =
                        let _, _, sv =
                          List.find
                            ~f:(fun (a, b, _) -> a = bpc && equal_origin b orig)
                            shadow_vars
                        in
                        sv
                      in
                      (* 2. Append shadow params to each block *)
                      List.iter
                        ~f:(fun (bpc, _orig, sv) ->
                          let b = Addr.Map.find bpc !result in
                          result :=
                            Addr.Map.add bpc { b with params = b.params @ [ sv ] } !result)
                        shadow_vars;
                      (* 3. Record substitution v -> target_sv and remove the
                   conversion from the target block *)
                      let target_sv = find_shadow_var pc initial_origin in
                      all_substs := Var.Map.add v target_sv !all_substs;
                      let b = Addr.Map.find pc !result in
                      let new_body =
                        List.filter
                          ~f:(function
                            | Let (x, _) when Var.equal x v -> false
                            | _ -> true)
                          b.body
                      in
                      result := Addr.Map.add pc { b with body = new_body } !result;
                      (* Update tables *)
                      Var.Hashtbl.remove conv_defs v;
                      (match Addr.Map.find_opt pc !block_computed with
                      | Some bc ->
                          let bc' =
                            ConvMap.map
                              (fun w -> if Var.equal w v then target_sv else w)
                              bc
                          in
                          block_computed := Addr.Map.add pc bc' !block_computed
                      | None -> ());
                      (* 4. Update predecessor branches for each
                   shadow block to pass the shadow value *)
                      List.iter
                        ~f:(fun (bpc, origin, _sv) ->
                          let bpc_preds =
                            Addr.Map.find_opt bpc sorted_preds |> Option.value ~default:[]
                          in
                          List.iter
                            ~f:(fun pred_pc ->
                              let pb = Addr.Map.find pred_pc !result in
                              let materialised = ref [] in
                              let ext ((tpc, args) as cont) =
                                if tpc <> bpc
                                then cont
                                else
                                  let q =
                                    match origin with
                                    | Param bpi -> List.nth args bpi
                                    | Var v -> v
                                  in
                                  let shadow_val =
                                    match find_direct_value pred_pc q kind with
                                    | Some u -> u
                                    | None when Option.is_some (constant_supply q kind) ->
                                        let c = Option.get (constant_supply q kind) in
                                        let u = Var.fresh () in
                                        Typing.set_var_type types u (type_of_kind kind);
                                        materialised :=
                                          Let (u, Constant c) :: !materialised;
                                        u
                                    | None ->
                                        let pred_block = Addr.Map.find pred_pc !result in
                                        let pred_origin =
                                          match find_param_idx q pred_block.params with
                                          | Some qi -> Param qi
                                          | None -> Var q
                                        in
                                        find_shadow_var pred_pc pred_origin
                                  in
                                  tpc, args @ [ shadow_val ]
                              in
                              let br =
                                match pb.branch with
                                | Branch c -> Branch (ext c)
                                | Cond (w, c1, c2) -> Cond (w, ext c1, ext c2)
                                | Switch (w, cs) -> Switch (w, Array.map ~f:ext cs)
                                | Pushtrap (c1, w, c2) -> Pushtrap (ext c1, w, ext c2)
                                | Poptrap c -> Poptrap (ext c)
                                | br -> br
                              in
                              result :=
                                Addr.Map.add
                                  pred_pc
                                  { pb with
                                    body = pb.body @ List.rev !materialised
                                  ; branch = br
                                  }
                                  !result)
                            bpc_preds)
                        shadow_vars))
          | _ -> ())
      candidates;
    (* Apply all substitutions in a single pass *)
    if Var.Map.is_empty !all_substs
    then !result
    else
      let subst_var x =
        match Var.Map.find_opt x !all_substs with
        | Some sv -> sv
        | None -> x
      in
      Addr.Map.map
        (fun b ->
          let new_body = List.map ~f:(Subst.Excluding_Binders.instr subst_var) b.body in
          let new_branch = Subst.Excluding_Binders.last subst_var b.branch in
          { b with body = new_body; branch = new_branch })
        !result

(* Phase 2: LCM dataflow analysis and rewrite for a single function.

   Given the explicit conversion instructions produced by lowering, compute the
   optimal placement using five dataflow analyses, then rewrite the IR.

   The analysis must be per-function to avoid cross-function variable references
   in the inserted conversion instructions. *)
let process_function
    types
    conv_types
    entry
    fun_blocks
    return_type
    ~assigned
    ~global_flow_info
    ~(st : lcm_stats) =
  let all_convs, local_conv_types = get_all_conversions fun_blocks types ~assigned in
  ConvMap.iter
    (fun conv typ -> conv_types := ConvMap.add conv typ !conv_types)
    local_conv_types;
  st.functions_processed <- st.functions_processed + 1;
  if ConvSet.is_empty all_convs
  then fun_blocks
  else (
    st.functions_with_conversions <- st.functions_with_conversions + 1;
    st.conversions_tracked <- st.conversions_tracked + ConvSet.cardinal all_convs;
    let t0 = tick () in
    let is_safe (kind, var) = is_safe_input kind (Typing.var_type types var) in
    let props = Addr.Map.map (compute_local_props all_convs is_safe) fun_blocks in
    let block_param_sets =
      Addr.Map.map
        (fun block ->
          List.fold_left ~f:(fun s v -> VarSet.add v s) ~init:VarSet.empty block.params)
        fun_blocks
    in
    let preds = CFG.predecessors fun_blocks in
    (* Compute reverse post-order (RPO) via DFS from entry.
       Forward analyses process in RPO (index 0..n-1).
       Backward analyses process in reverse RPO = post-order (index n-1..0). *)
    let rpo_order =
      let order = ref [] in
      let visited = ref Addr.Set.empty in
      let rec visit pc =
        if not (Addr.Set.mem pc !visited)
        then (
          visited := Addr.Set.add pc !visited;
          List.iter ~f:visit (CFG.successors fun_blocks pc);
          order := pc :: !order)
      in
      visit entry;
      Array.of_list !order
    in
    let n_blocks = Array.length rpo_order in
    let rpo_index = Addr.Hashtbl.create n_blocks in
    Array.iteri ~f:(fun i pc -> Addr.Hashtbl.add rpo_index pc i) rpo_order;
    (* Precompute per-block successor and predecessor lists as arrays for
       fast indexed access during the iterative sweeps. *)
    let succs_of =
      Array.init n_blocks ~f:(fun i -> CFG.successors fun_blocks rpo_order.(i))
    in
    let preds_of =
      Array.init n_blocks ~f:(fun i ->
          Addr.Map.find_opt rpo_order.(i) preds |> Option.value ~default:[])
    in
    let props_of = Array.init n_blocks ~f:(fun i -> Addr.Map.find rpo_order.(i) props) in
    let param_sets_of =
      Array.init n_blocks ~f:(fun i -> Addr.Map.find rpo_order.(i) block_param_sets)
    in
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
    let antin = Array.make n_blocks all_convs in
    let changed = ref true in
    while !changed do
      changed := false;
      (* Backward: sweep in post-order = reverse RPO *)
      for ri = n_blocks - 1 downto 0 do
        st.ant_iterations <- st.ant_iterations + 1;
        let b_props = props_of.(ri) in
        let succs = succs_of.(ri) in
        let new_antout =
          if List.is_empty succs
          then ConvSet.empty
          else
            List.fold_left
              ~f:(fun acc succ ->
                let si = Addr.Hashtbl.find rpo_index succ in
                let succ_antin = antin.(si) in
                let succ_param_set = param_sets_of.(si) in
                let valid_succ_antin =
                  ConvSet.filter
                    (fun (_, arg) -> not (VarSet.mem arg succ_param_set))
                    succ_antin
                in
                ConvSet.inter acc valid_succ_antin)
              ~init:all_convs
              succs
        in
        let new_antin =
          ConvSet.union b_props.antloc (ConvSet.inter b_props.transp_ant new_antout)
        in
        if not (ConvSet.equal antin.(ri) new_antin)
        then (
          antin.(ri) <- new_antin;
          changed := true)
      done
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
    let avout = Array.make n_blocks all_convs in
    avout.(0) <- ConvSet.empty;
    changed := true;
    while !changed do
      changed := false;
      (* Forward: sweep in RPO *)
      for ri = 0 to n_blocks - 1 do
        st.avail_iterations <- st.avail_iterations + 1;
        let pc = rpo_order.(ri) in
        let b_props = props_of.(ri) in
        let ps = preds_of.(ri) in
        let new_avin =
          if pc = entry || List.is_empty ps
          then ConvSet.empty
          else
            List.fold_left
              ~f:(fun acc p' -> ConvSet.inter acc avout.(Addr.Hashtbl.find rpo_index p'))
              ~init:all_convs
              ps
        in
        let new_avout =
          ConvSet.union b_props.comp (ConvSet.inter b_props.transp new_avin)
        in
        if not (ConvSet.equal avout.(ri) new_avout)
        then (
          avout.(ri) <- new_avout;
          changed := true)
      done
    done;
    let avin =
      Array.init n_blocks ~f:(fun ri ->
          let pc = rpo_order.(ri) in
          let ps = preds_of.(ri) in
          if pc = entry || List.is_empty ps
          then ConvSet.empty
          else
            List.fold_left
              ~f:(fun acc p' -> ConvSet.inter acc avout.(Addr.Hashtbl.find rpo_index p'))
              ~init:all_convs
              ps)
    in
    let earliest = Array.init n_blocks ~f:(fun ri -> ConvSet.diff antin.(ri) avin.(ri)) in
    (* Step 3: Delayability (forward dataflow, all-paths).

       DELAYIN(b) = set of conversions whose earliest placement can be delayed
       from their earliest point down to the entry of b without missing any use.

       Equation: DELAYIN(b) = EARLIEST(b) ∪ (∩ { DELAYOUT(p) | p ∈ preds(b) })
                 DELAYOUT(b) = DELAYIN(b) \ ANTLOC(b)

       A conversion can be delayed past a block as long as the block does not
       use it (an upward-exposed computation). This pushes insertions as late
       as possible. *)
    let delayin = Array.copy earliest in
    let delayout = Array.make n_blocks all_convs in
    changed := true;
    while !changed do
      changed := false;
      (* Forward: sweep in RPO *)
      for ri = 0 to n_blocks - 1 do
        st.delay_iterations <- st.delay_iterations + 1;
        let pc = rpo_order.(ri) in
        let b_props = props_of.(ri) in
        let ps = preds_of.(ri) in
        let new_delayin =
          if pc = entry || List.is_empty ps
          then earliest.(ri)
          else
            ConvSet.union
              earliest.(ri)
              (List.fold_left
                 ~f:(fun acc p' ->
                   ConvSet.inter acc delayout.(Addr.Hashtbl.find rpo_index p'))
                 ~init:all_convs
                 ps)
        in
        delayin.(ri) <- new_delayin;
        let new_delayout = ConvSet.diff new_delayin b_props.antloc in
        if not (ConvSet.equal delayout.(ri) new_delayout)
        then (
          delayout.(ri) <- new_delayout;
          changed := true)
      done
    done;
    (* Step 4: Latest (derived, no iteration needed).

       LATEST(b) = DELAYIN(b) ∩ (ANTLOC(b) ∪ ¬(∩ { DELAYIN(s) | s ∈ succs(b) }))

       A conversion is latest at b if it is delayable to b and either:
       - b uses the conversion (upward-exposed), or
       - some successor cannot accept further delay (the conversion is not
         delayable into all successors).

       This is the optimal insertion point: as late as possible while still
       covering all uses. *)
    let latest =
      Array.init n_blocks ~f:(fun ri ->
          let succs = succs_of.(ri) in
          let delayin_pc = delayin.(ri) in
          let b_props = props_of.(ri) in
          let delayin_succs_intersect =
            if List.is_empty succs
            then ConvSet.empty
            else
              List.fold_left
                ~f:(fun acc s ->
                  ConvSet.inter acc delayin.(Addr.Hashtbl.find rpo_index s))
                ~init:all_convs
                succs
          in
          ConvSet.inter
            delayin_pc
            (ConvSet.union
               b_props.antloc
               (ConvSet.diff all_convs delayin_succs_intersect)))
    in
    (* Step 5: Isolation (backward dataflow, all-paths).

       ISOLATEDIN(b) = LATEST(b) ∪ (ISOLATEDOUT(b) \ ANTLOC(b))
       ISOLATEDOUT(b) = ∩ { ISOLATEDIN(s) ∪ PARAMCONV(s) | s ∈ successors(b) }

       A conversion is isolated at the exit of b if its value there is not
       used afterwards: on every path, it is either inserted again or its
       operand is rebound (PARAMCONV(s): the conversions of the parameters of
       s) before any use. Exit blocks have everything isolated, and the
       analysis computes the greatest fixpoint.

       The final insertion set is LATEST(b) \ ISOLATEDOUT(b). A conversion
       that is latest and isolated at b is only used by its occurrence in b,
       which is kept as is: inserting it would just create a new temporary. *)
    let param_convs_of =
      Array.init n_blocks ~f:(fun si ->
          let params = param_sets_of.(si) in
          if VarSet.is_empty params
          then ConvSet.empty
          else ConvSet.filter (fun (_, arg) -> VarSet.mem arg params) all_convs)
    in
    let isolatedout = Array.make n_blocks all_convs in
    let isolatedin = Array.make n_blocks all_convs in
    changed := true;
    while !changed do
      changed := false;
      (* Backward: sweep in post-order = reverse RPO *)
      for ri = n_blocks - 1 downto 0 do
        st.isol_iterations <- st.isol_iterations + 1;
        let b_props = props_of.(ri) in
        let succs = succs_of.(ri) in
        let new_isolatedout =
          List.fold_left
            ~f:(fun acc s ->
              let si = Addr.Hashtbl.find rpo_index s in
              ConvSet.inter acc (ConvSet.union isolatedin.(si) param_convs_of.(si)))
            ~init:all_convs
            succs
        in
        isolatedout.(ri) <- new_isolatedout;
        let new_isolatedin =
          ConvSet.union latest.(ri) (ConvSet.diff new_isolatedout b_props.antloc)
        in
        if not (ConvSet.equal isolatedin.(ri) new_isolatedin)
        then (
          isolatedin.(ri) <- new_isolatedin;
          changed := true)
      done
    done;
    (* Convert array results back to maps for the rewrite phase. *)
    let to_map arr =
      let m = ref Addr.Map.empty in
      for i = 0 to n_blocks - 1 do
        m := Addr.Map.add rpo_order.(i) arr.(i) !m
      done;
      !m
    in
    let avin = to_map avin in
    let latest = to_map latest in
    let isolatedout = ref (to_map isolatedout) in
    let t1 = tick () in
    st.time_dataflow <- st.time_dataflow +. (t1 -. t0);

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
    (* Substitutions of eliminated conversions. They are applied to each block
       as it is rewritten, then to the whole function, as the result of a
       conversion can be used outside of its block (a box moved out of a
       loop, for instance). *)
    let all_substs = ref Var.Map.empty in
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
              List.filter_map ~f:(fun p -> Addr.Map.find_opt p !conv_out_map) preds_pc
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
        let conv_to_var = ref (ConvTracker.of_map safe_conv_in) in
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
            then
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
                    st.conversions_inserted <- st.conversions_inserted + 1;
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
                conv_to_var := ConvTracker.add conv phi_var !conv_to_var)
              else to_insert_remaining := ConvSet.add conv !to_insert_remaining
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
            conv_to_var := ConvTracker.add conv tmp !conv_to_var;
            st.conversions_inserted <- st.conversions_inserted + 1;
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
                conv_to_var := ConvTracker.kill_var v !conv_to_var;
                subst := Var.Map.remove v !subst;
                match kind_of_prim p with
                | Some kind -> (
                    let conv = kind, arg in
                    match ConvTracker.find_opt conv !conv_to_var with
                    | Some tmp ->
                        st.conversions_eliminated <- st.conversions_eliminated + 1;
                        subst := Var.Map.add v tmp !subst;
                        all_substs := Var.Map.add v tmp !all_substs
                    | None ->
                        body_rev := i :: !body_rev;
                        conv_to_var := ConvTracker.add conv v !conv_to_var)
                | None -> body_rev := i :: !body_rev)
            | Let (v, Apply _) ->
                conv_to_var :=
                  ConvTracker.filter
                    (fun conv -> not (not_live_across_calls conv))
                    (ConvTracker.kill_var v !conv_to_var);
                subst := Var.Map.remove v !subst;
                body_rev := i :: !body_rev
            | Let (v, _) ->
                conv_to_var := ConvTracker.kill_var v !conv_to_var;
                subst := Var.Map.remove v !subst;
                body_rev := i :: !body_rev
            | Assign (v, _) ->
                conv_to_var := ConvTracker.kill_var v !conv_to_var;
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
                    match ConvTracker.find_opt conv !conv_to_var with
                    | Some tmp -> tmp
                    | None -> arg)
                | None -> arg)
              args
              target_types
          in
          pc', args'
        in
        let rewrite_branch branch =
          let int_n = Typing.Int Typing.Integer.Small_normalized in
          match branch with
          | Stop -> branch
          | Return y ->
              let y' =
                match
                  number_conversion_kind ~from:(Typing.var_type types y) ~into:return_type
                with
                | Some kind -> (
                    match ConvTracker.find_opt (kind, y) !conv_to_var with
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
                    match ConvTracker.find_opt (kind, y) !conv_to_var with
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
                    match ConvTracker.find_opt (kind, v) !conv_to_var with
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
                    match ConvTracker.find_opt (kind, v) !conv_to_var with
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
        conv_out_map := Addr.Map.add pc (ConvTracker.to_map !conv_to_var) !conv_out_map;
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
    (if not (Var.Map.is_empty !all_substs)
     then
       let subst_var x = apply_subst !all_substs x in
       result :=
         Addr.Map.map
           (fun b ->
             { b with
               body = List.map ~f:(Subst.Excluding_Binders.instr subst_var) b.body
             ; branch = Subst.Excluding_Binders.last subst_var b.branch
             })
           !result);
    let t2 = tick () in
    st.time_rewrite <- st.time_rewrite +. (t2 -. t1);
    let result = optimize_peephole_conversions !result preds ~assigned ~st in
    let t3 = tick () in
    st.time_peephole <- st.time_peephole +. (t3 -. t2);
    let result =
      eliminate_param_conversions result types preds ~assigned ~global_flow_info ~st
    in
    st.time_widening <- st.time_widening +. (tick () -. t3);
    result)

(* Entry point. Three steps:
   1. Decide which functions can return unboxed/untagged values for direct calls.
   2. For each function, lower implicit conversions into explicit IR primitives.
   3. Run the LCM analysis and rewrite to eliminate redundant conversions. *)
let f (p : program) (types : Typing.t) ~(global_flow_info : Global_flow.info) ~fun_info =
  let t = Timer.make () in
  let st = make_stats () in
  (* Decide return-type unboxing for functions whose call sites are all known.
     If a function always returns an unboxed number or a normalised int, record
     that as its return type so callers can avoid re-boxing. *)
  fold_closures
    p
    (fun name_opt _ _ _ () ->
      match name_opt with
      | Some g ->
          if Typing.can_unbox_parameters fun_info g
          then
            let s = Var.Map.find g global_flow_info.info_return_vals in
            let t =
              Var.Set.fold
                (fun x acc -> Typing.join (Typing.var_type types x) acc)
                s
                Typing.Bot
            in
            if Typing.is_unboxed_repr t then Typing.set_return_type types g t
      | None -> ())
    ();
  (* Collect function entry points, recording the defining block PC for closures *)
  let fun_entries = ref [ None, p.start, None ] in
  Addr.Map.iter
    (fun def_pc block ->
      List.iter
        ~f:(function
          | Let (x, Closure (_, (pc, _), _)) ->
              fun_entries := (Some x, pc, Some def_pc) :: !fun_entries
          | _ -> ())
        block.body)
    p.blocks;

  (* Precompute per-function block submaps via DFS.
     Each function's blocks are disjoint, so total work is O(N log N). *)
  let fun_block_tbl = Addr.Hashtbl.create (List.length !fun_entries) in
  List.iter
    ~f:(fun (_, entry, _) ->
      let visited = ref Addr.Map.empty in
      let rec visit pc =
        if not (Addr.Map.mem pc !visited)
        then (
          let block = Addr.Map.find pc p.blocks in
          visited := Addr.Map.add pc block !visited;
          List.iter ~f:visit (CFG.successors p.blocks pc))
      in
      visit entry;
      Addr.Hashtbl.add fun_block_tbl entry !visited)
    !fun_entries;

  (* Process each function independently to avoid cross-function variable leakage
     in the data flow analysis. *)
  let assigned = assigned_vars p.blocks in
  let conv_types = ref ConvMap.empty in
  let blocks = ref p.blocks in
  let free_pc = ref p.free_pc in
  let start = ref p.start in
  (* Closure entries that have been redirected to a forwarding block, and the
     blocks defining these closures. The redirections are applied once all
     functions have been processed: the defining block belongs to the
     enclosing function, which may be processed later and would then
     overwrite it. *)
  let entry_redirects = ref Addr.Map.empty in
  let redirected_defs = ref Addr.Set.empty in
  List.iter
    ~f:(fun (name_opt, entry, def_pc_opt) ->
      let ts = tick () in
      let return_type =
        match name_opt with
        | Some f -> Typing.return_type types f
        | None -> Typing.Top
      in
      let fun_blocks = Addr.Hashtbl.find fun_block_tbl entry in
      let t0 = tick () in
      st.time_setup <- st.time_setup +. (t0 -. ts);
      let fun_blocks =
        lower_conversions fun_blocks types global_flow_info return_type free_pc ~st
      in
      let fun_blocks = split_critical_edges fun_blocks free_pc in
      st.time_lowering <- st.time_lowering +. (tick () -. t0);
      (* If the entry block has CFG predecessors (e.g. it's a loop header),
         split the entry edge: insert a forwarding block so that the implicit
         Closure→entry edge doesn't conflict with parameter widening. *)
      let ts2 = tick () in
      let fun_blocks, entry =
        let preds = CFG.predecessors fun_blocks in
        match Addr.Map.find_opt entry preds with
        | Some (_ :: _) ->
            let entry_block = Addr.Map.find entry fun_blocks in
            let new_entry_pc = !free_pc in
            free_pc := new_entry_pc + 1;
            (* Fresh params for the new entry block *)
            let new_params =
              List.map
                ~f:(fun p ->
                  let p' = Var.fork p in
                  Typing.set_var_type types p' (Typing.var_type types p);
                  p')
                entry_block.params
            in
            let new_entry_block =
              { params = new_params; body = []; branch = Branch (entry, new_params) }
            in
            let fun_blocks = Addr.Map.add new_entry_pc new_entry_block fun_blocks in
            (* The Closure instruction in the defining block will target the
               new entry block *)
            (match def_pc_opt with
            | Some def_pc ->
                entry_redirects := Addr.Map.add entry new_entry_pc !entry_redirects;
                redirected_defs := Addr.Set.add def_pc !redirected_defs
            | None ->
                (* Program start — update start pc *)
                start := new_entry_pc);
            fun_blocks, new_entry_pc
        | _ -> fun_blocks, entry
      in
      st.time_setup <- st.time_setup +. (tick () -. ts2);
      let result =
        process_function
          types
          conv_types
          entry
          fun_blocks
          return_type
          ~assigned
          ~global_flow_info
          ~st
      in
      let ts3 = tick () in
      Addr.Map.iter (fun pc block -> blocks := Addr.Map.add pc block !blocks) result;
      st.time_setup <- st.time_setup +. (tick () -. ts3))
    !fun_entries;
  Addr.Set.iter
    (fun def_pc ->
      let def_block = Addr.Map.find def_pc !blocks in
      let body =
        List.map
          ~f:(function
            | Let (x, Closure (fv, (pc, args), cc)) when Addr.Map.mem pc !entry_redirects
              -> Let (x, Closure (fv, (Addr.Map.find pc !entry_redirects, args), cc))
            | i -> i)
          def_block.body
      in
      blocks := Addr.Map.add def_pc { def_block with body } !blocks)
    !redirected_defs;
  let p = { start = !start; blocks = !blocks; free_pc = !free_pc } in
  if times ()
  then (
    Format.eprintf "  lcm: %a@." Timer.print t;
    Format.eprintf
      "    lowering: %.2f@.    dataflow: %.2f@.    rewrite: %.2f@.    peephole: \
       %.2f@.    widening: %.2f@.    setup: %.2f@."
      st.time_lowering
      st.time_dataflow
      st.time_rewrite
      st.time_peephole
      st.time_widening
      st.time_setup);
  if stats ()
  then
    Format.eprintf
      "Stats - lcm: %d functions (%d with conversions), %d lowered, %d tracked, ant:%d \
       avail:%d delay:%d isol:%d iters, %d inserted, %d eliminated, %d peephole, %d \
       params widened@."
      st.functions_processed
      st.functions_with_conversions
      st.conversions_lowered
      st.conversions_tracked
      st.ant_iterations
      st.avail_iterations
      st.delay_iterations
      st.isol_iterations
      st.conversions_inserted
      st.conversions_eliminated
      st.peephole_eliminated
      st.params_widened;
  if debug ()
  then (
    prerr_endline "AFTER";
    Print.program Format.err_formatter (fun _ _ -> "") p);
  p, types
