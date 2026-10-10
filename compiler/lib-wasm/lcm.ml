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
      as explicit IR primitives ([Wasm_conversion]).
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

let check = Debug.find "lcm-check"

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
  ; mutable boxes_hoisted_from_loops : int
  ; mutable unboxes_hoisted_from_loops : int
  ; mutable boxes_hoisted_from_closures : int
  ; mutable dead_conversions_removed : int
  ; mutable dead_params_removed : int
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
  ; boxes_hoisted_from_loops = 0
  ; unboxes_hoisted_from_loops = 0
  ; boxes_hoisted_from_closures = 0
  ; dead_conversions_removed = 0
  ; dead_params_removed = 0
  ; time_lowering = 0.
  ; time_dataflow = 0.
  ; time_rewrite = 0.
  ; time_peephole = 0.
  ; time_widening = 0.
  ; time_setup = 0.
  }

let tick () = Sys.time ()

type conversion_kind = wasm_conversion

module Conv = struct
  type t = conversion_kind * Var.t

  (* In declaration order, as a polymorphic comparison, which is slower *)
  let kind_index = function
    | Unbox_i32 -> 0
    | Unbox_i64 -> 1
    | Unbox_f64 -> 2
    | Box_i32 -> 3
    | Box_i64 -> 4
    | Box_f64 -> 5
    | Untag_int -> 6
    | Normalize_int -> 7
    | Tag_int -> 8

  let compare (k1, v1) (k2, v2) =
    let c = Int.compare (kind_index k1) (kind_index k2) in
    if c <> 0 then c else Var.compare v1 v2
end

module ConvSet = Set.Make (Conv)
module ConvMap = Map.Make (Conv)

(* Sets of the conversions of a function, as bit vectors indexed by the
   rank of the conversion in the set of all its conversions. The data flow
   analyses keep several sets per block: balanced trees would take memory
   proportional to the number of blocks times the number of conversions,
   which is too much for large functions. All the sets of a function have
   the same length. *)
module Bits : sig
  type t

  val empty : int -> t

  val full : int -> t
  (** [full n] contains the conversions of rank [0] to [n - 1] *)

  val add : t -> int -> unit

  val remove : t -> int -> unit

  val copy : t -> t

  val union : t -> t -> t

  val inter : t -> t -> t

  val diff : t -> t -> t

  val equal : t -> t -> bool

  val iter : f:(int -> unit) -> t -> unit
end = struct
  type t = int array

  let length n = (n + Sys.int_size - 1) / Sys.int_size

  let empty n = Array.make (length n) 0

  let full n =
    let a = Array.make (length n) (-1) in
    let r = n mod Sys.int_size in
    if r <> 0 then a.(Array.length a - 1) <- (1 lsl r) - 1;
    a

  let add a i =
    let j = i / Sys.int_size in
    a.(j) <- a.(j) lor (1 lsl (i mod Sys.int_size))

  let remove a i =
    let j = i / Sys.int_size in
    a.(j) <- a.(j) land lnot (1 lsl (i mod Sys.int_size))

  let copy = Array.copy

  let union a b = Array.mapi ~f:(fun i x -> x lor b.(i)) a

  let inter a b = Array.mapi ~f:(fun i x -> x land b.(i)) a

  let diff a b = Array.mapi ~f:(fun i x -> x land lnot b.(i)) a

  let equal a b =
    let rec loop i = i < 0 || (a.(i) = b.(i) && loop (i - 1)) in
    loop (Array.length a - 1)

  let iter ~f a =
    Array.iteri
      ~f:(fun j x ->
        if x <> 0
        then
          for k = 0 to Sys.int_size - 1 do
            if x land (1 lsl k) <> 0 then f ((j * Sys.int_size) + k)
          done)
      a
end

let prim_of_kind kind = Wasm_conversion kind

let kind_of_prim p =
  match p with
  | Wasm_conversion kind -> Some kind
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
  | Normalize_int -> None

let type_of_kind = Typing.conversion_type

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
  | Box_i32 | Box_i64 | Box_f64 | Tag_int | Normalize_int -> true
  | Untag_int -> (
      match typ with
      | Typing.Int
          (Typing.Integer.Ref | Typing.Integer.Normalized | Typing.Integer.Unnormalized)
        -> true
      | _ -> false)

(* The variables whose untaggings are hoisted out of a loop although their
   type is not known to be an integer. These untaggings are performed
   speculatively, so they must not fail: [Generate] returns a dummy value
   rather than failing when the value is not an integer (the original
   untagging then only happens on paths where it is an integer). *)
let guarded = Var.Hashtbl.create 16

let guarded_untag x = Var.Hashtbl.mem guarded x

let is_untag kind =
  match kind with
  | Untag_int -> true
  | Unbox_i32
  | Unbox_i64
  | Unbox_f64
  | Box_i32
  | Box_i64
  | Box_f64
  | Tag_int
  | Normalize_int -> false

(* Whether a conversion of [y] cannot fail *)
let is_safe_conversion types (kind, y) =
  is_safe_input kind (Typing.var_type types y) || (is_untag kind && guarded_untag y)

(* Whether the result of a conversion should not be kept live across a
   call. Untagging a 31-bit integer is a single shift, and normalizing it
   two: recomputing it after the call is cheaper than keeping the result
   in a register, which the call forces to spill. (This would not hold
   with a more expensive integer representation, such as mixed 63-bit
   integers.) *)
let not_live_across_calls (kind, _) =
  match kind with
  | Untag_int | Normalize_int -> true
  | Unbox_i32 | Unbox_i64 | Unbox_f64 | Box_i32 | Box_i64 | Box_f64 | Tag_int -> false

(* Determine which conversion operation, if any, is needed to go from one
   representation to another. Returns [None] when the representations match
   or no conversion is applicable. *)
let number_conversion_kind = Typing.conversion

(* Apply [f] to the continuations of a branch, from left to right *)
let map_conts f branch =
  match branch with
  | Return _ | Raise _ | Stop -> branch
  | Branch c -> Branch (f c)
  | Poptrap c -> Poptrap (f c)
  | Cond (x, c1, c2) ->
      let c1 = f c1 in
      let c2 = f c2 in
      Cond (x, c1, c2)
  | Switch (x, a) -> Switch (x, Array.map ~f a)
  | Pushtrap (c1, x, c2) ->
      let c1 = f c1 in
      let c2 = f c2 in
      Pushtrap (c1, x, c2)

let iter_conts f branch =
  match branch with
  | Return _ | Raise _ | Stop -> ()
  | Branch c | Poptrap c -> f c
  | Cond (_, c1, c2) | Pushtrap (c1, _, c2) ->
      f c1;
      f c2
  | Switch (_, a) -> Array.iter ~f a

let lower_var_conversion ~types ~(from : Typing.typ) ~(into : Typing.typ) x =
  match number_conversion_kind ~from ~into with
  | None -> [], x
  | Some kind ->
      let tmp = Var.fresh () in
      (* The result is given the type of the conversion rather than the
         type expected by the use. A tagged integer or a boxed number is
         then known to be one: it is compared with a physical equality
         rather than with [js_eqeqeq] when the use expects any value. And
         an untagged integer is always normalized, even when the use
         accepts an unnormalized one. Otherwise, as the types of the
         occurrences of a conversion are joined when they are merged, a
         single use accepting unnormalized integers would force the other
         ones to normalize the value again. *)
      Typing.set_var_type types tmp (type_of_kind kind);
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
   if a function parameter expects an unboxed float but the argument is boxed, a
   [Wasm_conversion Unbox_f64] instruction is inserted before the call.
   Similarly, if a primitive produces an unboxed result but the variable is
   typed as boxed, a boxing instruction is inserted after the definition.

   For branches with multiple targets (Cond, Switch, Pushtrap), conversions
   needed for different targets are placed on split edges (fresh intermediate
   blocks) so they don't execute on the wrong path. *)
let lower_conversions
    (blocks : block Addr.Map.t)
    (types : Typing.t)
    (global_flow_info : Global_flow.info)
    (return_type : Typing.typ)
    (free_pc : int ref)
    ~constant_twin
    ~(st : lcm_stats) =
  (* A conversion of a number constant is replaced by a variable bound to
     the constant in the target representation (see [constant_twins]): a
     boxed constant is a static value and an unboxed one an immediate. *)
  let lower_var_conversion ~types ~from ~into x =
    let constant =
      match constant_of global_flow_info x with
      | Some (Float _ | Int32 _ | Int64 _ | NativeInt _) as c -> c
      | ((Some (Float32 _) as c) [@if oxcaml]) -> c
      | Some _ | None -> None
    in
    match constant with
    | Some c when Option.is_some (number_conversion_kind ~from ~into) ->
        [], constant_twin x c ~into
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
      let int_n = Typing.Int Typing.Integer.Normalized in
      let is_int a =
        match a with
        | Pv v -> (
            match Typing.var_type types v with
            | Typing.Int _ -> true
            | _ -> false)
        | Pc c -> (
            match Typing.constant_type c with
            | Typing.Int _ -> true
            | _ -> false)
      in
      match p with
      | Extern ("caml_array_unsafe_get", _) | Array_get _ -> Some [ top; int_n ]
      | Extern
          ( (( "caml_ba_get_1"
             | "caml_ba_get_2"
             | "caml_ba_get_3"
             | "caml_ba_set_1"
             | "caml_ba_set_2"
             | "caml_ba_set_3"
             | "caml_ba_float32_get_1"
             | "caml_ba_float32_get_2"
             | "caml_ba_float32_get_3"
             | "caml_ba_float32_set_1"
             | "caml_ba_float32_set_2"
             | "caml_ba_float32_set_3" ) as nm)
          , hint )
        when String.starts_with ~prefix:"caml_ba_float32_" nm
             || (match hint with
               | Some (Optimization_hint.Hint_bigarray _) -> true
               | _ -> false)
             ||
             match args with
             | Pv ba :: _ -> (
                 match Typing.var_type types ba with
                 | Typing.Bigarray _ -> true
                 | _ -> false)
             | _ -> false ->
          (* The code generator converts the indices of specialized bigarray
             accesses (and of all float32 accesses) itself. Make these
             conversions explicit, so that they can be shared with other uses
             of the indices. The bigarray and the value keep their
             representation. *)
          let indices = Char.code nm.[String.length nm - 1] - Char.code '0' in
          let own a =
            match a with
            | Pv v -> Typing.var_type types v
            | Pc c -> Typing.constant_type c
          in
          Some
            (List.mapi ~f:(fun i a -> if i = 0 || i > indices then own a else int_n) args)
      | Extern (nm, _) -> (
          match Typing.prim_sig nm with
          | Typed target_types, _ -> Some target_types
          | Values, _ -> Some (List.map ~f:(fun _ -> top) args)
          | Any_representation, _ ->
              (* The code generator uses the types of the arguments *)
              None)
      | Eq | Neq -> (
          (* Integers are compared directly, other values physically *)
          match args with
          | [ a; b ] when is_int a && is_int b -> None
          | _ -> Some (List.map ~f:(fun _ -> top) args))
      | Lt | Le | Ult ->
          (* Unnormalized integers are compared by shifting them, which is
             cheaper than normalizing them. A normalized copy is still used
             when one is available (see [process_function]). *)
          let int_u = Typing.Int Typing.Integer.Unnormalized in
          Some [ int_u; int_u ]
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
    | Prim (p, args) -> (
        match p with
        | Wasm_conversion ((Unbox_f64 | Unbox_i32 | Unbox_i64) as kind) ->
            type_of_kind kind
        | Wasm_conversion Untag_int
        | Lt | Le | Ult | IsInt _ | Eq | Neq | Not | Vectlength _ ->
            Typing.Int Typing.Integer.Normalized
        | Extern (nm, _) ->
            let t = Typing.prim_result_type types nm args in
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
          | Block (tag, fields, kind, mut) when tag <> 254 ->
              (* The fields of a block are values *)
              let lowered_rev = ref [] in
              let fields =
                Array.map
                  ~f:(fun y ->
                    let lowered, y' =
                      lower_var_conversion
                        ~types
                        ~from:(Typing.var_type types y)
                        ~into:Typing.Top
                        y
                    in
                    lowered_rev := List.rev_append lowered !lowered_rev;
                    y')
                  fields
              in
              List.rev !lowered_rev @ [ Let (x, Block (tag, fields, kind, mut)) ]
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
    | Stop -> [], Stop
    | Branch _ | Poptrap _ ->
        (* The conversions are performed before the branch *)
        let lowered = ref [] in
        let branch =
          map_conts
            (fun cont ->
              let l, cont = lower_cont cont in
              lowered := l;
              cont)
            branch
        in
        !lowered, branch
    | Cond _ | Switch _ | Pushtrap _ ->
        (* The conversions are performed on the edge towards each target *)
        let lowered_v, branch =
          match branch with
          | Switch (v, conts) ->
              let lowered_v, v' =
                lower_var_conversion ~types ~from:(Typing.var_type types v) ~into:int_n v
              in
              lowered_v, Switch (v', conts)
          | _ -> [], branch
        in
        ( lowered_v
        , map_conts
            (fun cont ->
              let lowered, cont = lower_cont cont in
              split_edge lowered cont)
            branch )
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

   - [kill]: conversions which are not transparent: their operand is killed
     in this block, or their result should not be kept live across a call
     and the block contains a call. The transparent conversions are the
     other ones.
   - [kill_ant]: the conversions which cannot be anticipated through this
     block: the ones in [kill], and the conversions which may fail
     ([is_safe_conversion]) if the block contains an instruction which may
     raise ([Typing.may_raise]). Such a conversion is only known to
     succeed when it is reached: it must not be performed before an
     instruction which may prevent reaching it (e.g. with GADTs).
   - [comp]: conversions that are computed (appear) in this block and are not
     subsequently killed by a later redefinition of their operand.
     (Downward-exposed computations.)
   - [antloc]: conversions that are computed in this block and whose operand was
     not killed before the computation, nor, for a conversion which may
     fail, preceded by an instruction which may raise. (Locally
     anticipatable: the operand's value at block entry reaches the
     conversion.) *)
type block_props =
  { kill : Bits.t
  ; kill_ant : Bits.t
  ; comp : Bits.t
  ; antloc : Bits.t
  }

let remove_conversions_of_var convs v =
  ConvSet.filter (fun (_, arg) -> not (Var.equal arg v)) convs

(* The ranks of the conversions of a function. The conversions tracked by
   the data flow analyses have ranks [0] to [n - 1], in increasing order;
   other conversions get larger ranks when they are first added to a
   [Tracker]. *)
module Ranks : sig
  type t

  val create : ConvSet.t -> t

  val tracked : t -> int
  (** The number of tracked conversions *)

  val find : t -> Conv.t -> int option

  val get : t -> Conv.t -> int
  (** Allocates a rank if needed *)

  val conv : t -> int -> Conv.t
end = struct
  module Tbl = Hashtbl.Make (struct
    type t = Conv.t

    let equal a b = Conv.compare a b = 0

    let hash (k, v) = Hashtbl.hash (Conv.kind_index k, Var.idx v)
  end)

  type t =
    { ranks : int Tbl.t
    ; convs : Conv.t Int_trie.t ref
    ; tracked : int
    ; mutable next : int
    }

  let create convs =
    let ranks = Tbl.create 16 in
    let n, convs_by_rank =
      ConvSet.fold
        (fun conv (i, m) ->
          Tbl.add ranks conv i;
          i + 1, Int_trie.add i conv m)
        convs
        (0, Int_trie.empty)
    in
    { ranks; convs = ref convs_by_rank; tracked = n; next = n }

  let tracked t = t.tracked

  let find t conv = Tbl.find_opt t.ranks conv

  let get t conv =
    match Tbl.find_opt t.ranks conv with
    | Some i -> i
    | None ->
        let i = t.next in
        t.next <- i + 1;
        Tbl.add t.ranks conv i;
        t.convs := Int_trie.add i conv !(t.convs);
        i

  let conv t i = Int_trie.find i !(t.convs)
end

(* The available conversions at a point of a block, with the variable
   holding their result. The map is indexed by the ranks of the
   conversions, with a reverse index from variables to the conversions
   they appear in, so that killing a variable is cheap. The set of the
   tracked conversions in the map is also kept as a bit vector, so that it
   can be combined cheaply with the result of the data flow analyses. *)
module Tracker : sig
  type t

  val empty : Ranks.t -> t

  val entry : Ranks.t -> t list -> avin:Bits.t -> t * int list
  (** [entry ranks preds ~avin] is the tracker at the entry of a block,
      given the trackers at the exit of its processed predecessors and the
      conversions available at its entry. The conversions which are
      available in all predecessors with the same variable are kept. The
      other conversions of [avin] present in all predecessors are returned,
      by increasing rank. *)

  val find_opt : Ranks.t -> Conv.t -> t -> Var.t option

  val mem : Ranks.t -> Conv.t -> t -> bool

  val add : Ranks.t -> Conv.t -> Var.t -> t -> t

  val kill_var : Ranks.t -> Var.t -> t -> t

  val kill_at_call : Ranks.t -> t -> t
  (** Remove the conversions whose result should not be kept live across a
      call *)
end = struct
  type t =
    { fwd : Var.t Int_trie.t
    ; dom : Bits.t
          (* The tracked conversions of [fwd]. It is updated in place: a
             tracker must not be used once it has been updated. *)
    ; extra : int list (* The other conversions of [fwd], possibly removed since *)
    ; rev : int list Var.Map.t
          (* var -> conversions mentioning it; may contain conversions
             already removed *)
    ; at_call : int list
          (* conversions removed at calls; may contain conversions already
             removed *)
    }

  let empty ranks =
    { fwd = Int_trie.empty
    ; dom = Bits.empty (Ranks.tracked ranks)
    ; extra = []
    ; rev = Var.Map.empty
    ; at_call = []
    }

  let entry ranks preds ~avin =
    match preds with
    | [] -> empty ranks, []
    | hd :: tl ->
        let dom =
          List.fold_left
            ~f:(fun d p -> Bits.inter d p.dom)
            ~init:(Bits.inter hd.dom avin)
            tl
        in
        let common = Bits.copy dom in
        let differing = ref [] in
        if not (List.is_empty tl)
        then
          Bits.iter
            ~f:(fun i ->
              let v = Int_trie.find i hd.fwd in
              if
                not
                  (List.for_all
                     ~f:(fun p ->
                       match Int_trie.find_opt i p.fwd with
                       | Some v' -> Var.equal v v'
                       | None -> false)
                     tl)
              then (
                differing := i :: !differing;
                Bits.remove dom i))
            common;
        let fwd =
          List.fold_left ~f:(fun m i -> Int_trie.remove i m) ~init:hd.fwd hd.extra
        in
        let fwd = ref fwd in
        Bits.iter ~f:(fun i -> fwd := Int_trie.remove i !fwd) (Bits.diff hd.dom dom);
        ( { fwd = !fwd; dom; extra = []; rev = hd.rev; at_call = hd.at_call }
        , List.rev !differing )

  let find_opt ranks conv t =
    match Ranks.find ranks conv with
    | Some i -> Int_trie.find_opt i t.fwd
    | None -> None

  let mem ranks conv t =
    match Ranks.find ranks conv with
    | Some i -> Int_trie.mem i t.fwd
    | None -> false

  let rev_add v i rev =
    Var.Map.update
      v
      (function
        | None -> Some [ i ]
        | Some l -> Some (i :: l))
      rev

  let add ranks conv mapped t =
    let i = Ranks.get ranks conv in
    let _, arg = conv in
    let extra =
      if i < Ranks.tracked ranks
      then (
        Bits.add t.dom i;
        t.extra)
      else i :: t.extra
    in
    { fwd = Int_trie.add i mapped t.fwd
    ; dom = t.dom
    ; extra
    ; rev = rev_add arg i (rev_add mapped i t.rev)
    ; at_call = (if not_live_across_calls conv then i :: t.at_call else t.at_call)
    }

  let remove ranks i t =
    if i < Ranks.tracked ranks then Bits.remove t.dom i;
    Int_trie.remove i t.fwd

  let kill_var ranks v t =
    match Var.Map.find_opt v t.rev with
    | None -> t
    | Some l ->
        let fwd =
          List.fold_left
            ~f:(fun fwd i ->
              match Int_trie.find_opt i fwd with
              | Some mapped ->
                  let _, arg = Ranks.conv ranks i in
                  if Var.equal arg v || Var.equal mapped v
                  then remove ranks i { t with fwd }
                  else fwd
              | None -> fwd)
            ~init:t.fwd
            l
        in
        { t with fwd; rev = Var.Map.remove v t.rev }

  let kill_at_call ranks t =
    { t with
      fwd =
        List.fold_left
          ~f:(fun fwd i -> remove ranks i { t with fwd })
          ~init:t.fwd
          t.at_call
    ; at_call = []
    }
end

(* The conversions of each operand *)
let conversions_by_operand all_convs =
  let tbl = Var.Hashtbl.create 16 in
  ConvSet.iter
    (fun ((_, v) as conv) ->
      Var.Hashtbl.replace
        tbl
        v
        (ConvSet.add
           conv
           (Var.Hashtbl.find_opt tbl v |> Option.value ~default:ConvSet.empty)))
    all_convs;
  tbl

let conversions_of_vars convs_by_operand vars =
  List.fold_left
    ~f:(fun acc v ->
      match Var.Hashtbl.find_opt convs_by_operand v with
      | Some s -> ConvSet.union acc s
      | None -> acc)
    ~init:ConvSet.empty
    vars

let is_call i =
  match i with
  | Let (_, Apply _) -> true
  | Let _ | Assign _ | Set_field _ | Offset_ref _ | Array_set _ | Event _ -> false

(* [not_live_convs] and [unsafe_convs] are the conversions whose result
   should not be kept live across a call, and those which may fail; [bits]
   converts a set of conversions into a bit vector *)
let compute_local_props convs_by_operand ~not_live_convs ~unsafe_convs ~bits is_safe block
    =
  (* Block parameters are definitions. They kill conversions involving them. *)
  let killed_vars =
    List.fold_left
      ~f:(fun acc i ->
        match i with
        | Let (v, _) | Assign (v, _) -> v :: acc
        | Set_field _ | Offset_ref _ | Array_set _ | Event _ -> acc)
      ~init:block.params
      block.body
  in
  let kill = bits (conversions_of_vars convs_by_operand killed_vars) in
  (* A call also ends the availability of the conversions whose result
     should not be kept live across calls *)
  let kill =
    if List.exists ~f:is_call block.body then Bits.union kill not_live_convs else kill
  in
  let kill_ant =
    if List.exists ~f:Typing.may_raise block.body
    then Bits.union kill unsafe_convs
    else kill
  in
  let comp = ref ConvSet.empty in
  let antloc = ref ConvSet.empty in
  let current_killed = ref VarSet.empty in
  let seen_call = ref false in
  let seen_raise = ref false in
  List.iter ~f:(fun v -> current_killed := VarSet.add v !current_killed) block.params;
  (* A variable bound by [Let] cannot occur in the conversions computed
     earlier in the block, but an assigned variable can *)
  let kill_var v = current_killed := VarSet.add v !current_killed in
  let kill_assigned_var v =
    kill_var v;
    comp := remove_conversions_of_var !comp v
  in
  List.iter
    ~f:(fun i ->
      (match i with
      | Let (v, Prim (p, [ Pv arg ])) -> (
          match kind_of_prim p with
          | Some kind ->
              let conv = kind, arg in
              if
                (not (VarSet.mem arg !current_killed))
                && (is_safe conv || not !seen_raise)
                && not (not_live_across_calls conv && !seen_call)
              then antloc := ConvSet.add conv !antloc;
              comp := ConvSet.add conv !comp;
              kill_var v
          | None -> kill_var v)
      | Let (v, Apply _) ->
          seen_call := true;
          comp := ConvSet.filter (fun conv -> not (not_live_across_calls conv)) !comp;
          kill_var v
      | Let (v, _) -> kill_var v
      | Assign (v, _) -> kill_assigned_var v
      | Set_field _ | Offset_ref _ | Array_set _ | Event _ -> ());
      if Typing.may_raise i then seen_raise := true)
    block.body;
  { kill; kill_ant; comp = bits !comp; antloc = bits !antloc }

module CFG = struct
  let successors blocks pc =
    List.rev (Code.fold_children blocks pc (fun pc l -> pc :: l) [])

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
  let split_branch branch =
    match branch with
    | Cond _ | Switch _ | Pushtrap _ ->
        let targets = ref [] in
        iter_conts (fun (pc, _) -> targets := pc :: !targets) branch;
        map_conts (make_split_cont (needs_split !targets)) branch
    | Branch _ | Poptrap _ | Return _ | Raise _ | Stop -> branch
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
              iter_conts collect_args pred_block.branch)
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
    Addr.Map.map
      (fun block ->
        let body =
          List.filter_map
            ~f:(fun i ->
              match i with
              | Let (x, _) when Var.Map.mem x !subst -> None
              | _ -> Some (Subst.Excluding_Binders.instr subst_var i))
            block.body
        in
        { block with body; branch = Subst.Excluding_Binders.last subst_var block.branch })
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
      iter_conts (fun (t, a) -> if t = target_pc then acc := a :: !acc) branch;
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
    (* The conversion can be performed in a predecessor of the block [target]
       containing it when this predecessor is the entry of the function (it
       has no predecessor): it is then performed once per call, when the
       block is entered for the first time, rather than at each iteration
       when the block is a loop header. This is not speculative when the
       block performs the conversion whenever it is entered ([reached]: no
       instruction which may raise precedes it), or when the conversion of
       the argument [q] cannot fail. *)
    let materialize_at ~target ~reached bpc origin pred_pc q kind =
      bpc = target
      && (match origin with
        | Param _ ->
            (* The value is an argument of the branch of the predecessor,
                available there *)
            true
        | Var _ -> false)
      && List.is_empty (Addr.Map.find_opt pred_pc sorted_preds |> Option.value ~default:[])
      && (reached || is_safe_conversion types (kind, q))
    in
    let rec can_supply ~target ~reached bpc origin kind visited =
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
                    | None when materialize_at ~target ~reached bpc origin pred_pc q kind
                      -> check_conts rest_conts acc
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
                          match
                            can_supply ~target ~reached pred_pc pred_origin kind visited
                          with
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
    (* Block parameters with an untagged shadow parameter *)
    let untagged_twins = ref Var.Map.empty in
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
              (* A conversion performed in the entry of the function
                 replaces the untagging of [param_var], which must not fail
                 if it may have been executed speculatively *)
              let guarded_conv = is_untag kind && guarded_untag param_var in
              let guard q = if guarded_conv then Var.Hashtbl.replace guarded q () in
              (* Whether the conversion is performed whenever the block is
                 entered, or cannot fail once performed in the entry *)
              let reached =
                guarded_conv
                ||
                let rec loop l =
                  match l with
                  | [] -> false
                  | Let (x, _) :: _ when Var.equal x v -> true
                  | i :: rem -> (not (Typing.may_raise i)) && loop rem
                in
                loop block.body
              in
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
                      ~target:pc
                      ~reached
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
                      (* The untagged shadow can replace the parameter
                         wherever the parameter is known to be an integer,
                         not only after the untagging (see
                         [compare_untagged]) *)
                      (match kind, initial_origin with
                      | Untag_int, Param _
                        when is_safe_input Untag_int (Typing.var_type types param_var) ->
                          untagged_twins :=
                            Var.Map.add param_var target_sv !untagged_twins
                      | _ -> ());
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
                                    | None
                                      when materialize_at
                                             ~target:pc
                                             ~reached
                                             bpc
                                             origin
                                             pred_pc
                                             q
                                             kind ->
                                        guard q;
                                        let u = Var.fresh () in
                                        Typing.set_var_type types u (type_of_kind kind);
                                        materialised :=
                                          Let (u, Prim (prim_of_kind kind, [ Pv q ]))
                                          :: !materialised;
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
                              let br = map_conts ext pb.branch in
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
    (* An integer compared for equality with a constant or an untagged
       integer can be compared untagged: use the untagged shadow of a block
       parameter, which is in scope wherever the parameter is. The tagged
       parameter may then become useless. The parameter must be known to be
       an integer: the untagging of another value may have been
       speculative, returning 0 ([guarded]), or may not be reached. *)
    let untagged_int a =
      match a with
      | Pc (Int _) -> true
      | Pc _ -> false
      | Pv y -> (
          Var.Map.mem y !untagged_twins
          ||
          match Typing.var_type types y with
          | Typing.Int (Normalized | Unnormalized) -> true
          | _ -> false)
    in
    let twin a =
      match a with
      | Pv y -> (
          match Var.Map.find_opt y !untagged_twins with
          | Some sv -> Pv sv
          | None -> a)
      | Pc _ -> a
    in
    let compare_untagged i =
      match i with
      | Let (x, Prim (((Eq | Neq) as op), [ a; b ]))
        when untagged_int a
             && untagged_int b
             && (Var.Map.mem
                   (match a with
                   | Pv y -> y
                   | Pc _ -> x)
                   !untagged_twins
                || Var.Map.mem
                     (match b with
                     | Pv y -> y
                     | Pc _ -> x)
                     !untagged_twins) -> Let (x, Prim (op, [ twin a; twin b ]))
      | _ -> i
    in
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
          let new_body =
            List.map
              ~f:(fun i -> compare_untagged (Subst.Excluding_Binders.instr subst_var i))
              b.body
          in
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
    let is_safe conv = is_safe_conversion types conv in
    let convs_by_operand = conversions_by_operand all_convs in
    (* The rank of each conversion, for its bit vector representation *)
    let ranks = Ranks.create all_convs in
    let n_convs = Ranks.tracked ranks in
    let conv_of_rank = Array.of_list (ConvSet.elements all_convs) in
    let rank_of_conv conv =
      match Ranks.find ranks conv with
      | Some i when i < n_convs -> Some i
      | _ -> None
    in
    (* The conversions of the assigned variables are not tracked: they are
       left where they are, and are never available at the beginning of a
       block *)
    let bits convs =
      let b = Bits.empty n_convs in
      ConvSet.iter
        (fun conv ->
          match rank_of_conv conv with
          | Some i -> Bits.add b i
          | None -> ())
        convs;
      b
    in
    let all_convs = Bits.full n_convs in
    let no_convs = Bits.empty n_convs in
    let not_live_convs = Bits.empty n_convs in
    let unsafe_convs = Bits.empty n_convs in
    Array.iteri
      ~f:(fun i conv ->
        if not_live_across_calls conv then Bits.add not_live_convs i;
        if not (is_safe conv) then Bits.add unsafe_convs i)
      conv_of_rank;
    let props =
      Addr.Map.map
        (compute_local_props convs_by_operand ~not_live_convs ~unsafe_convs ~bits is_safe)
        fun_blocks
    in
    let preds = CFG.predecessors fun_blocks in
    (* Forward analyses process the blocks in reverse post-order (index
       0..n-1), backward analyses in post-order (index n-1..0). *)
    let rpo_order =
      Array.of_list
        (Structure.blocks_in_reverse_post_order
           (Structure.control_flow_graph fun_blocks entry))
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
    (* The conversions of the parameters of each block *)
    let param_convs_of =
      Array.init n_blocks ~f:(fun i ->
          bits
            (conversions_of_vars
               convs_by_operand
               (Addr.Map.find rpo_order.(i) fun_blocks).params))
    in
    (* Step 1: Anticipatability (backward dataflow, all-paths).

       ANTIN(b) = set of conversions that will definitely be computed on every
       path from b to the function exit, before their operand is redefined.

       A conversion is anticipatable at a point if it is safe (and useful) to
       move its computation to that point.

       Equation: ANTIN(b) = ANTLOC(b) ∪ (ANTOUT(b) \ KILL_ANT(b))
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
          then no_convs
          else
            List.fold_left
              ~f:(fun acc succ ->
                let si = Addr.Hashtbl.find rpo_index succ in
                Bits.inter acc (Bits.diff antin.(si) param_convs_of.(si)))
              ~init:all_convs
              succs
        in
        let new_antin =
          Bits.union b_props.antloc (Bits.diff new_antout b_props.kill_ant)
        in
        if not (Bits.equal antin.(ri) new_antin)
        then (
          antin.(ri) <- new_antin;
          changed := true)
      done
    done;
    (* Step 2: Availability (forward dataflow, all-paths).

       AVOUT(b) = set of conversions that have been computed on every path from
       the function entry to the exit of b, without their operand being redefined.

       Equation: AVIN(b)  = ∩ { AVOUT(p) | p ∈ predecessors(b) }
                 AVOUT(b) = COMP(b) ∪ (AVIN(b) \ KILL(b))

       The entry block is initialised to empty (nothing is available on entry).

       EARLIEST(b) = ANTIN(b) \ AVIN(b)
       A conversion is earliest at b if it is anticipated there but not yet
       available — this is the first point where inserting it is both useful
       and correct. *)
    let avout = Array.make n_blocks all_convs in
    avout.(0) <- no_convs;
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
          then no_convs
          else
            List.fold_left
              ~f:(fun acc p' -> Bits.inter acc avout.(Addr.Hashtbl.find rpo_index p'))
              ~init:all_convs
              ps
        in
        let new_avout = Bits.union b_props.comp (Bits.diff new_avin b_props.kill) in
        if not (Bits.equal avout.(ri) new_avout)
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
          then no_convs
          else
            List.fold_left
              ~f:(fun acc p' -> Bits.inter acc avout.(Addr.Hashtbl.find rpo_index p'))
              ~init:all_convs
              ps)
    in
    let earliest = Array.init n_blocks ~f:(fun ri -> Bits.diff antin.(ri) avin.(ri)) in
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
            Bits.union
              earliest.(ri)
              (List.fold_left
                 ~f:(fun acc p' ->
                   Bits.inter acc delayout.(Addr.Hashtbl.find rpo_index p'))
                 ~init:all_convs
                 ps)
        in
        delayin.(ri) <- new_delayin;
        let new_delayout = Bits.diff new_delayin b_props.antloc in
        if not (Bits.equal delayout.(ri) new_delayout)
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
            then no_convs
            else
              List.fold_left
                ~f:(fun acc s -> Bits.inter acc delayin.(Addr.Hashtbl.find rpo_index s))
                ~init:all_convs
                succs
          in
          Bits.inter
            delayin_pc
            (Bits.union b_props.antloc (Bits.diff all_convs delayin_succs_intersect)))
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
              Bits.inter acc (Bits.union isolatedin.(si) param_convs_of.(si)))
            ~init:all_convs
            succs
        in
        isolatedout.(ri) <- new_isolatedout;
        let new_isolatedin =
          Bits.union latest.(ri) (Bits.diff new_isolatedout b_props.antloc)
        in
        if not (Bits.equal isolatedin.(ri) new_isolatedin)
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
    let rpo = Array.to_list rpo_order in
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
        (* The conversions available in all processed predecessors with the
           same variable are inherited. Those present in all predecessors
           with different variables are merged by a phi, at fully-processed
           merge points. *)
        let conv_in, differing =
          Tracker.entry ranks processed_preds ~avin:(Addr.Map.find pc avin)
        in
        let merge_phis = ref [] in
        if all_preds_processed && List.length preds_pc > 1
        then
          List.iter
            ~f:(fun i ->
              let ((kind, _) as conv) = conv_of_rank.(i) in
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
              merge_phis := (conv, phi_var) :: !merge_phis)
            differing;
        let to_insert =
          let b = Bits.diff (Addr.Map.find pc latest) (Addr.Map.find pc !isolatedout) in
          let s = ref [] in
          Bits.iter ~f:(fun i -> s := conv_of_rank.(i) :: !s) b;
          List.rev !s
        in
        let inserted_rev = ref [] in
        let conv_to_var =
          ref
            (List.fold_left
               ~f:(fun t (conv, phi_var) -> Tracker.add ranks conv phi_var t)
               ~init:conv_in
               (List.rev !merge_phis))
        in
        (* Partial redundancy elimination: for each conversion in to_insert,
           check if some predecessors already have it. If so, insert only at
           the missing predecessors and create a phi to merge the results.
           This avoids redundant computation on paths that already have it.
           Only at fully-processed non-loop merge points (same guard as the
           all-preds phi insertion above). *)
        let to_insert_remaining = ref ConvSet.empty in
        List.iter
          ~f:(fun ((kind, arg) as conv) ->
            if all_preds_processed && List.length preds_pc > 1
            then
              let preds_with =
                List.filter
                  ~f:(fun p ->
                    match Addr.Map.find_opt p !conv_out_map with
                    | Some m -> Tracker.mem ranks conv m
                    | None -> false)
                  preds_pc
              in
              let preds_without =
                List.filter
                  ~f:(fun p ->
                    match Addr.Map.find_opt p !conv_out_map with
                    | Some m -> not (Tracker.mem ranks conv m)
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
                      |> Option.value ~default:(Tracker.empty ranks)
                    in
                    conv_out_map :=
                      Addr.Map.add
                        pred_pc
                        (Tracker.add ranks conv tmp pred_conv_out)
                        !conv_out_map)
                  preds_without;
                phi_info :=
                  Addr.Map.update
                    pc
                    (function
                      | None -> Some [ conv, phi_var ]
                      | Some l -> Some ((conv, phi_var) :: l))
                    !phi_info;
                conv_to_var := Tracker.add ranks conv phi_var !conv_to_var)
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
            conv_to_var := Tracker.add ranks conv tmp !conv_to_var;
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
                conv_to_var := Tracker.kill_var ranks v !conv_to_var;
                subst := Var.Map.remove v !subst;
                match kind_of_prim p with
                | Some kind -> (
                    let conv = kind, arg in
                    match Tracker.find_opt ranks conv !conv_to_var with
                    | Some tmp ->
                        st.conversions_eliminated <- st.conversions_eliminated + 1;
                        subst := Var.Map.add v tmp !subst;
                        all_substs := Var.Map.add v tmp !all_substs
                    | None ->
                        body_rev := i :: !body_rev;
                        conv_to_var := Tracker.add ranks conv v !conv_to_var)
                | None -> body_rev := i :: !body_rev)
            | Let (v, Prim (((Lt | Le | Ult | Eq | Neq) as p), args)) ->
                (* Compare the normalized copy of an integer when one is
                   available, rather than shifting the integer *)
                let args =
                  List.map
                    ~f:(fun a ->
                      match a with
                      | Pv x -> (
                          match Typing.var_type types x with
                          | Int Unnormalized -> (
                              match
                                Tracker.find_opt ranks (Normalize_int, x) !conv_to_var
                              with
                              | Some x' -> Pv x'
                              | None -> a)
                          | _ -> a)
                      | Pc _ -> a)
                    args
                in
                conv_to_var := Tracker.kill_var ranks v !conv_to_var;
                subst := Var.Map.remove v !subst;
                body_rev := Let (v, Prim (p, args)) :: !body_rev
            | Let (v, Apply _) ->
                conv_to_var :=
                  Tracker.kill_at_call ranks (Tracker.kill_var ranks v !conv_to_var);
                subst := Var.Map.remove v !subst;
                body_rev := i :: !body_rev
            | Let (v, _) ->
                conv_to_var := Tracker.kill_var ranks v !conv_to_var;
                subst := Var.Map.remove v !subst;
                body_rev := i :: !body_rev
            | Assign (v, _) ->
                conv_to_var := Tracker.kill_var ranks v !conv_to_var;
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
                    match Tracker.find_opt ranks conv !conv_to_var with
                    | Some tmp -> tmp
                    | None -> arg)
                | None -> arg)
              args
              target_types
          in
          pc', args'
        in
        (* The untagged integer tested by a branch *)
        let int_arg v =
          match
            number_conversion_kind
              ~from:(Typing.var_type types v)
              ~into:(Typing.Int Typing.Integer.Normalized)
          with
          | Some kind -> (
              match Tracker.find_opt ranks (kind, v) !conv_to_var with
              | Some tmp -> tmp
              | None -> v)
          | None -> v
        in
        let rewrite_branch branch =
          match branch with
          | Stop -> branch
          | Return y ->
              let y' =
                match
                  number_conversion_kind ~from:(Typing.var_type types y) ~into:return_type
                with
                | Some kind -> (
                    match Tracker.find_opt ranks (kind, y) !conv_to_var with
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
                    match Tracker.find_opt ranks (kind, y) !conv_to_var with
                    | Some tmp -> tmp
                    | None -> y)
                | None -> y
              in
              Raise (y', l)
          | Branch _ | Poptrap _ | Pushtrap _ -> map_conts rewrite_cont branch
          | Cond (v, cont1, cont2) ->
              map_conts rewrite_cont (Cond (int_arg v, cont1, cont2))
          | Switch (v, conts) -> map_conts rewrite_cont (Switch (int_arg v, conts))
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
              match Addr.Map.find_opt pred_pc !conv_out_map with
              | Some t -> t
              | None -> Tracker.empty ranks
            in
            let extend_cont ((pc', args) as cont) =
              if pc' = target_pc
              then
                let extra =
                  List.map
                    ~f:(fun (conv, _phi_var) ->
                      match Tracker.find_opt ranks conv pred_conv_out with
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
            let new_branch = map_conts extend_cont pred_block.branch in
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

(* Placement of boxing conversions.

   A boxing conversion (box or tag) is performed where the boxed value is
   used, so that no value is boxed on a path where the boxed value is not
   needed. But a value must not be boxed more than once per execution of its
   definition: a box of [y] must not be repeated by a loop or a function
   body which does not contain the definition of [y]. Such a box is moved to
   the entry of the outermost such region: before the loop, or, for a
   function body, before the creation of the closure in the function where
   [y] is defined (and then before the loops of that function which do not
   contain the definition of [y]). *)

let is_boxing kind =
  match kind with
  | Box_i32 | Box_i64 | Box_f64 | Tag_int -> true
  | Unbox_i32 | Unbox_i64 | Unbox_f64 | Untag_int | Normalize_int -> false

(* [Some (x, kind, y)] if the instruction is [x = kind(y)] with [kind] a
   boxing conversion *)
let boxing i =
  match i with
  | Let (x, Prim (p, [ Pv y ])) -> (
      match kind_of_prim p with
      | Some kind when is_boxing kind -> Some (x, kind, y)
      | Some _ | None -> None)
  | Let _ | Assign _ | Set_field _ | Offset_ref _ | Array_set _ | Event _ -> None

(* The natural loops of a function, with the size of their body. A back
   edge goes to a block which dominates its source, so the body of a loop
   can only be entered through its header. *)
module Loops = struct
  type t = (Addr.Set.t * int) Addr.Map.t

  let compute blocks entry : t =
    let g = Structure.control_flow_graph blocks entry in
    (* Most functions have no loop: the dominators and the predecessors are
       only computed once a backward edge is found *)
    let idom = lazy (Structure.immediate_dominators g) in
    let rec dominates header pc =
      pc = header
      ||
      match Addr.Hashtbl.find_opt (Lazy.force idom) pc with
      | Some pc' -> dominates header pc'
      | None -> false
    in
    let preds = lazy (CFG.predecessors blocks) in
    let preds pc = Addr.Map.find_opt pc (Lazy.force preds) |> Option.value ~default:[] in
    List.fold_left
      ~f:(fun loops src ->
        List.fold_left
          ~f:(fun loops header ->
            if Structure.is_backward g src header && dominates header src
            then (
              let body =
                ref
                  (Addr.Map.find_opt header loops
                  |> Option.value ~default:(Addr.Set.singleton header))
              in
              let rec add pc =
                if not (Addr.Set.mem pc !body)
                then (
                  body := Addr.Set.add pc !body;
                  List.iter ~f:add (preds pc))
              in
              add src;
              Addr.Map.add header !body loops)
            else loops)
          ~init:loops
          (CFG.successors blocks src))
      ~init:Addr.Map.empty
      (Structure.blocks_in_reverse_post_order g)
    |> Addr.Map.map (fun body -> body, Addr.Set.cardinal body)

  (* The outermost loop containing block [pc] but not the definition [def]
     of a value ([None]: the value is defined before the function body,
     e.g. a parameter) *)
  let outermost ?(ok = fun _ -> true) (loops : t) pc ~def =
    Addr.Map.fold
      (fun header (body, size) acc ->
        if
          Addr.Set.mem pc body
          && ok body
          &&
          match def with
          | None -> true
          | Some d -> not (Addr.Set.mem d body)
        then
          match acc with
          | Some (_, (_, size')) when size' >= size -> acc
          | Some _ | None -> Some (header, (body, size))
        else acc)
      loops
      None
end

(* A block executed right before entering the loop, which is created if
   needed. [None] if the loop is entered through an exception handler or
   when leaving one. *)
let loop_preheader ~types ~free_pc ~preds blocks (header, (body, _)) =
  let entry_preds =
    preds header
    |> List.filter ~f:(fun pc -> not (Addr.Set.mem pc body))
    |> List.sort_uniq ~cmp:compare
  in
  let branch pc = (Addr.Map.find pc !blocks).branch in
  if
    List.is_empty entry_preds
    || not
         (List.for_all
            ~f:(fun pc ->
              match branch pc with
              | Branch _ | Cond _ | Switch _ -> true
              | Pushtrap _ | Poptrap _ | Return _ | Raise _ | Stop -> false)
            entry_preds)
  then None
  else
    match entry_preds with
    | [ pc ]
      when match branch pc with
           | Branch _ -> true
           | _ -> false -> Some pc
    | _ ->
        let params =
          List.map
            ~f:(fun x ->
              let x' = Var.fork x in
              Typing.set_var_type types x' (Typing.var_type types x);
              x')
            (Addr.Map.find header !blocks).params
        in
        let pc' = !free_pc in
        incr free_pc;
        blocks :=
          Addr.Map.add pc' { params; body = []; branch = Branch (header, params) } !blocks;
        List.iter
          ~f:(fun pc ->
            let b = Addr.Map.find pc !blocks in
            let redirect ((pc'', args) as cont) =
              if pc'' = header then pc', args else cont
            in
            blocks :=
              Addr.Map.add pc { b with branch = map_conts redirect b.branch } !blocks)
          entry_preds;
        Some pc'

let append_instrs blocks pc l =
  let b = Addr.Map.find pc !blocks in
  blocks := Addr.Map.add pc { b with body = b.body @ l } !blocks

(* Move the boxes of a function out of the loops that do not contain the
   definition of the boxed value. [params] are the parameters of the
   function. The boxes of free variables are handled by
   [hoist_boxes_out_of_closures]. *)
(* Whether a block of the function contains an instruction satisfying [f] *)
let exists_instr f blocks = Addr.Map.exists (fun _ b -> List.exists ~f b.body) blocks

let hoist_boxes_out_of_loops ~types ~free_pc ~params ~(st : lcm_stats) blocks entry =
  let loops =
    if exists_instr (fun i -> Option.is_some (boxing i)) blocks
    then Loops.compute blocks entry
    else Addr.Map.empty
  in
  if Addr.Map.is_empty loops
  then blocks
  else
    let defs = Var.Hashtbl.create 16 in
    Addr.Map.iter
      (fun pc b ->
        List.iter ~f:(fun x -> Var.Hashtbl.replace defs x pc) b.params;
        List.iter
          ~f:(fun i ->
            match i with
            | Let (x, _) -> Var.Hashtbl.replace defs x pc
            | Assign _ | Set_field _ | Offset_ref _ | Array_set _ | Event _ -> ())
          b.body)
      blocks;
    let params =
      List.fold_left ~f:(fun s x -> VarSet.add x s) ~init:VarSet.empty params
    in
    let moves = ref [] in
    Addr.Map.iter
      (fun pc b ->
        List.iter
          ~f:(fun i ->
            match boxing i with
            | Some (x, _, y) -> (
                let def =
                  match Var.Hashtbl.find_opt defs y with
                  | Some d -> Some (Some d)
                  | None -> if VarSet.mem y params then Some None else None
                in
                match def with
                | Some def -> (
                    match Loops.outermost loops pc ~def with
                    | Some l -> moves := (pc, x, i, l) :: !moves
                    | None -> ())
                | None -> ())
            | None -> ())
          b.body)
      blocks;
    if List.is_empty !moves
    then blocks
    else
      let blocks = ref blocks in
      let preds = CFG.predecessors !blocks in
      let preds pc = Addr.Map.find_opt pc preds |> Option.value ~default:[] in
      let preheaders = Addr.Hashtbl.create 8 in
      List.iter
        ~f:(fun (pc, x, i, ((header, _) as l)) ->
          let ph =
            match Addr.Hashtbl.find_opt preheaders header with
            | Some ph -> ph
            | None ->
                let ph = loop_preheader ~types ~free_pc ~preds blocks l in
                Addr.Hashtbl.add preheaders header ph;
                ph
          in
          match ph with
          | None -> ()
          | Some ph ->
              st.boxes_hoisted_from_loops <- st.boxes_hoisted_from_loops + 1;
              let b = Addr.Map.find pc !blocks in
              let body =
                List.filter
                  ~f:(fun i ->
                    match i with
                    | Let (x', _) -> not (Var.equal x x')
                    | Assign _ | Set_field _ | Offset_ref _ | Array_set _ | Event _ ->
                        true)
                  b.body
              in
              blocks := Addr.Map.add pc { b with body } !blocks;
              append_instrs blocks ph [ i ])
        (List.rev !moves);
      !blocks

(* Speculative hoisting of unboxing conversions out of loops.

   An unboxing (unbox or untag) of a loop-invariant value is performed in
   the loop preheader, once per entry into the loop; LCM then eliminates the
   occurrences inside the loop, which have become redundant. This is done
   only when the conversion cannot fail, that is when the type of the
   operand is known ([is_safe_conversion]), or for an untagging, which can be
   made not to fail ([guarded]): the conversion may be executed on paths
   where it was not before, such as when the loop is exited before reaching
   it. It is also cheap, so executing it speculatively is fine.
   An untagging is not hoisted out of a loop containing calls, since its
   result should not be kept live across calls ([not_live_across_calls]). *)
let hoist_unboxes_out_of_loops ~types ~free_pc ~assigned ~(st : lcm_stats) blocks entry =
  let loops =
    if
      exists_instr
        (fun i ->
          match i with
          | Let (_, Prim (p, [ Pv _ ])) -> (
              match kind_of_prim p with
              | Some kind -> not (is_boxing kind)
              | None -> false)
          | Let _ | Assign _ | Set_field _ | Offset_ref _ | Array_set _ | Event _ -> false)
        blocks
    then Loops.compute blocks entry
    else Addr.Map.empty
  in
  if Addr.Map.is_empty loops
  then blocks
  else
    let defs = Var.Hashtbl.create 16 in
    Addr.Map.iter
      (fun pc b ->
        List.iter ~f:(fun x -> Var.Hashtbl.replace defs x pc) b.params;
        List.iter
          ~f:(fun i ->
            match i with
            | Let (x, _) -> Var.Hashtbl.replace defs x pc
            | Assign _ | Set_field _ | Offset_ref _ | Array_set _ | Event _ -> ())
          b.body)
      blocks;
    let has_call body =
      Addr.Set.exists
        (fun pc ->
          List.exists
            ~f:(fun i ->
              match i with
              | Let (_, Apply _) -> true
              | Let _ | Assign _ | Set_field _ | Offset_ref _ | Array_set _ | Event _ ->
                  false)
            (Addr.Map.find pc blocks).body)
        body
    in
    (* For each loop header, the conversions to perform in the preheader *)
    let hoisted = ref Addr.Map.empty in
    Addr.Map.iter
      (fun pc b ->
        List.iter
          ~f:(fun i ->
            match i with
            | Let (_, Prim (p, [ Pv y ])) when not (VarSet.mem y assigned) -> (
                match kind_of_prim p with
                | Some kind
                  when (not (is_boxing kind))
                       && (is_safe_conversion types (kind, y) || is_untag kind) -> (
                    (* [None]: defined before the function body (parameter or
                       free variable) *)
                    let def = Var.Hashtbl.find_opt defs y in
                    let ok body =
                      (not (not_live_across_calls (kind, y))) || not (has_call body)
                    in
                    match Loops.outermost ~ok loops pc ~def with
                    | Some (header, l) ->
                        let convs, _ =
                          Addr.Map.find_opt header !hoisted
                          |> Option.value ~default:(ConvSet.empty, l)
                        in
                        hoisted :=
                          Addr.Map.add header (ConvSet.add (kind, y) convs, l) !hoisted
                    | None -> ())
                | Some _ | None -> ())
            | Let _ | Assign _ | Set_field _ | Offset_ref _ | Array_set _ | Event _ -> ())
          b.body)
      blocks;
    if Addr.Map.is_empty !hoisted
    then blocks
    else
      let blocks = ref blocks in
      let preds = CFG.predecessors !blocks in
      let preds pc = Addr.Map.find_opt pc preds |> Option.value ~default:[] in
      Addr.Map.iter
        (fun header (convs, l) ->
          match loop_preheader ~types ~free_pc ~preds blocks (header, l) with
          | None -> ()
          | Some ph ->
              append_instrs
                blocks
                ph
                (List.map
                   ~f:(fun ((kind, y) : Conv.t) ->
                     st.unboxes_hoisted_from_loops <- st.unboxes_hoisted_from_loops + 1;
                     if not (is_safe_conversion types (kind, y))
                     then Var.Hashtbl.replace guarded y ();
                     let x = Var.fresh () in
                     Typing.set_var_type types x (type_of_kind kind);
                     Let (x, Prim (prim_of_kind kind, [ Pv y ])))
                   (ConvSet.elements convs)))
        !hoisted;
      !blocks

(* The functions of a program, and where variables are defined *)
type functions =
  { fun_of_block : Addr.t Addr.Hashtbl.t  (** Entry of the function of a block *)
  ; blocks_of_fun : Addr.t list Addr.Hashtbl.t
  ; parent : Addr.t Addr.Hashtbl.t  (** Block where a closure is created *)
  ; def_fun : Addr.t Var.Hashtbl.t
        (** Function where a variable is defined (only for the operands of
            boxing conversions) *)
  ; def_block : Addr.t Var.Hashtbl.t
        (** Block where a variable is defined (only for the operands of
            boxing conversions, and not for function parameters) *)
  }

let functions (p : program) =
  let fun_of_block = Addr.Hashtbl.create 1024 in
  let blocks_of_fun = Addr.Hashtbl.create 64 in
  let parent = Addr.Hashtbl.create 64 in
  let rec visit f pc =
    if not (Addr.Hashtbl.mem fun_of_block pc)
    then (
      Addr.Hashtbl.add fun_of_block pc f;
      Addr.Hashtbl.replace
        blocks_of_fun
        f
        (pc :: (Addr.Hashtbl.find_opt blocks_of_fun f |> Option.value ~default:[]));
      List.iter ~f:(visit f) (CFG.successors p.blocks pc))
  in
  visit p.start p.start;
  Addr.Map.iter
    (fun pc b ->
      List.iter
        ~f:(fun i ->
          match i with
          | Let (_, Closure (_, (entry, _), _)) ->
              Addr.Hashtbl.replace parent entry pc;
              visit entry entry
          | Let _ | Assign _ | Set_field _ | Offset_ref _ | Array_set _ | Event _ -> ())
        b.body)
    p.blocks;
  (* Only the definitions of the operands of boxing conversions are
     needed: recording all the variables of the program would be costly *)
  let boxed = Var.Hashtbl.create 1024 in
  Addr.Map.iter
    (fun _ b ->
      List.iter
        ~f:(fun i ->
          match boxing i with
          | Some (_, _, y) -> Var.Hashtbl.replace boxed y ()
          | None -> ())
        b.body)
    p.blocks;
  let def_fun = Var.Hashtbl.create 1024 in
  let def_block = Var.Hashtbl.create 1024 in
  Addr.Map.iter
    (fun pc b ->
      match Addr.Hashtbl.find_opt fun_of_block pc with
      | None -> ()
      | Some f ->
          let def x =
            if Var.Hashtbl.mem boxed x
            then (
              Var.Hashtbl.replace def_fun x f;
              Var.Hashtbl.replace def_block x pc)
          in
          List.iter ~f:def b.params;
          List.iter
            ~f:(fun i ->
              match i with
              | Let (x, e) -> (
                  def x;
                  match e with
                  | Closure (params, (entry, _), _) ->
                      List.iter
                        ~f:(fun y ->
                          if Var.Hashtbl.mem boxed y
                          then Var.Hashtbl.replace def_fun y entry)
                        params
                  | _ -> ())
              | Assign _ | Set_field _ | Offset_ref _ | Array_set _ | Event _ -> ())
            b.body)
    p.blocks;
  { fun_of_block; blocks_of_fun; parent; def_fun; def_block }

let fun_blocks (p : program) fns f =
  List.fold_left
    ~f:(fun m pc -> Addr.Map.add pc (Addr.Map.find pc p.blocks) m)
    ~init:Addr.Map.empty
    (Addr.Hashtbl.find fns.blocks_of_fun f)

(* Move the boxes of free variables of a function to the function where the
   variable is defined, before the creation of the closure. *)
let hoist_boxes_out_of_closures ~types ~(st : lcm_stats) (p : program) =
  let fns = functions p in
  (* For each closure created in the function where a boxed variable is
     defined, the boxes to perform before creating it *)
  let insertions = Addr.Hashtbl.create 16 in
  let subst = Var.Hashtbl.create 16 in
  Addr.Map.iter
    (fun pc b ->
      match Addr.Hashtbl.find_opt fns.fun_of_block pc with
      | None -> ()
      | Some f ->
          List.iter
            ~f:(fun i ->
              match boxing i with
              | Some (x, kind, y) -> (
                  match Var.Hashtbl.find_opt fns.def_fun y with
                  | Some g when g <> f -> (
                      (* The outermost closure containing [f] created in [g] *)
                      let rec closure f =
                        match Addr.Hashtbl.find_opt fns.parent f with
                        | None -> None
                        | Some def_pc ->
                            let f' = Addr.Hashtbl.find fns.fun_of_block def_pc in
                            if f' = g then Some f else closure f'
                      in
                      match closure f with
                      | None -> ()
                      | Some c ->
                          let m =
                            Addr.Hashtbl.find_opt insertions c
                            |> Option.value ~default:ConvMap.empty
                          in
                          let yb =
                            match ConvMap.find_opt (kind, y) m with
                            | Some yb -> yb
                            | None ->
                                let yb = Var.fresh () in
                                Typing.set_var_type types yb (Typing.var_type types x);
                                Addr.Hashtbl.replace
                                  insertions
                                  c
                                  (ConvMap.add (kind, y) yb m);
                                yb
                          in
                          st.boxes_hoisted_from_closures <-
                            st.boxes_hoisted_from_closures + 1;
                          Var.Hashtbl.replace subst x yb)
                  | Some _ | None -> ())
              | None -> ())
            b.body)
    p.blocks;
  if Var.Hashtbl.length subst = 0
  then p
  else
    let subst_var x = Var.Hashtbl.find_opt subst x |> Option.value ~default:x in
    let blocks =
      ref
        (Addr.Map.map
           (fun b ->
             let body =
               List.filter_map
                 ~f:(fun i ->
                   match i with
                   | Let (x, _) when Var.Hashtbl.mem subst x -> None
                   | _ -> Some (Subst.Excluding_Binders.instr subst_var i))
                 b.body
             in
             { b with body; branch = Subst.Excluding_Binders.last subst_var b.branch })
           p.blocks)
    in
    let free_pc = ref p.free_pc in
    (* Loops and loop preheaders of the functions where boxes are inserted *)
    let loops = Addr.Hashtbl.create 16 in
    let get_loops g =
      match Addr.Hashtbl.find_opt loops g with
      | Some l -> l
      | None ->
          let fb = fun_blocks p fns g in
          let preds = CFG.predecessors fb in
          let l =
            ( Loops.compute fb g
            , (fun pc -> Addr.Map.find_opt pc preds |> Option.value ~default:[])
            , Addr.Hashtbl.create 8 )
          in
          Addr.Hashtbl.add loops g l;
          l
    in
    Addr.Hashtbl.iter
      (fun c m ->
        let def_pc = Addr.Hashtbl.find fns.parent c in
        let g = Addr.Hashtbl.find fns.fun_of_block def_pc in
        let loops, preds, preheaders = get_loops g in
        ConvMap.iter
          (fun (kind, y) yb ->
            let instr = Let (yb, Prim (prim_of_kind kind, [ Pv y ])) in
            let def = Var.Hashtbl.find_opt fns.def_block y in
            let ph =
              match Loops.outermost loops def_pc ~def with
              | None -> None
              | Some ((header, _) as l) -> (
                  match Addr.Hashtbl.find_opt preheaders header with
                  | Some ph -> ph
                  | None ->
                      let ph = loop_preheader ~types ~free_pc ~preds blocks l in
                      Addr.Hashtbl.add preheaders header ph;
                      ph)
            in
            match ph with
            | Some ph -> append_instrs blocks ph [ instr ]
            | None ->
                (* Before the group of closures that contains [c] *)
                let b = Addr.Map.find def_pc !blocks in
                let rec insert rev_group l =
                  match l with
                  | (Let (_, Closure (_, (entry, _), _)) as i) :: rem ->
                      if entry = c
                      then (instr :: List.rev rev_group) @ (i :: rem)
                      else insert (i :: rev_group) rem
                  | i :: rem -> List.rev_append rev_group (i :: insert [] rem)
                  | [] -> assert false
                in
                blocks := Addr.Map.add def_pc { b with body = insert [] b.body } !blocks)
          m)
      insertions;
    { p with blocks = !blocks; free_pc = !free_pc }

(* Boxes which may be performed more than once per execution of the
   definition of the boxed value: boxes of free variables, and boxes inside
   a loop which does not contain the definition of the value *)
let check_box_placement (p : program) =
  let fns = functions p in
  let violations = ref 0 in
  Addr.Hashtbl.iter
    (fun f _ ->
      let fb = fun_blocks p fns f in
      let loops = Loops.compute fb f in
      Addr.Map.iter
        (fun pc b ->
          List.iter
            ~f:(fun i ->
              match boxing i with
              | Some (x, _, y) -> (
                  let report reason =
                    incr violations;
                    if check ()
                    then
                      Format.eprintf
                        "lcm: %a = box(%a) in block %d: %s@."
                        Var.print
                        x
                        Var.print
                        y
                        pc
                        reason
                  in
                  match Var.Hashtbl.find_opt fns.def_fun y with
                  | Some g when g <> f -> report "free variable"
                  | Some _ -> (
                      let def = Var.Hashtbl.find_opt fns.def_block y in
                      match Loops.outermost loops pc ~def with
                      | Some (header, _) ->
                          report (Printf.sprintf "in the loop at block %d" header)
                      | None -> ())
                  | None -> ())
              | None -> ())
            b.body)
        fb)
    fns.blocks_of_fun;
  !violations

(* The conversions whose result is not used, and the block parameters
   which are not used, once the conversions are placed: the peephole pass
   and the widening of parameters remove uses, and no dead code
   elimination is performed afterwards. A variable is live if it is used
   by an instruction other than a conversion, by a conversion whose result
   is live, or as the argument of a live block parameter. *)
let remove_dead_conversions ~(st : lcm_stats) (p : program) =
  let live = Array.make (Var.count ()) false in
  let deps = Var.Hashtbl.create 1024 in
  let add_dep x y =
    Var.Hashtbl.replace
      deps
      x
      (y :: (Var.Hashtbl.find_opt deps x |> Option.value ~default:[]))
  in
  let roots = ref [] in
  let root x = roots := x :: !roots in
  let cont (pc, args) = List.iter2 ~f:add_dep (Addr.Map.find pc p.blocks).params args in
  Addr.Map.iter
    (fun _ block ->
      List.iter
        ~f:(fun i ->
          match i with
          | Let (x, Prim (Wasm_conversion _, [ Pv y ])) -> add_dep x y
          | Let (_, Closure (_, c, _)) -> cont c
          | Assign (x, y) ->
              root x;
              root y
          | _ -> Freevars.iter_instr_free_vars root i)
        block.body;
      (match block.branch with
      | Return x | Raise (x, _) | Cond (x, _, _) | Switch (x, _) -> root x
      | Stop | Branch _ | Poptrap _ | Pushtrap _ -> ());
      iter_conts cont block.branch)
    p.blocks;
  let rec mark l =
    match l with
    | [] -> ()
    | x :: rem ->
        let i = Var.idx x in
        if live.(i)
        then mark rem
        else (
          live.(i) <- true;
          mark
            (List.rev_append
               (Var.Hashtbl.find_opt deps x |> Option.value ~default:[])
               rem))
  in
  mark !roots;
  let is_live x = live.(Var.idx x) in
  let filter_cont ((pc, args) as c) =
    let params = (Addr.Map.find pc p.blocks).params in
    if List.for_all ~f:is_live params
    then c
    else
      ( pc
      , List.filter_map
          ~f:(fun (x, a) -> if is_live x then Some a else None)
          (List.combine params args) )
  in
  let blocks =
    Addr.Map.map
      (fun block ->
        let params =
          List.filter
            ~f:(fun x ->
              is_live x
              ||
              (st.dead_params_removed <- st.dead_params_removed + 1;
               false))
            block.params
        in
        let body =
          List.filter_map
            ~f:(fun i ->
              match i with
              | Let (x, Prim (Wasm_conversion _, _)) when not (is_live x) ->
                  st.dead_conversions_removed <- st.dead_conversions_removed + 1;
                  None
              | Let (x, Closure (params, c, loc)) ->
                  Some (Let (x, Closure (params, filter_cont c, loc)))
              | _ -> Some i)
            block.body
        in
        { params; body; branch = map_conts filter_cont block.branch })
      p.blocks
  in
  { p with blocks }

(* Entry point. For each function, lower implicit conversions into explicit
   IR primitives, then run the LCM analysis and rewrite to eliminate
   redundant conversions. The return types of functions are decided by
   [Typing]. *)
let f (p : program) (types : Typing.t) ~(global_flow_info : Global_flow.info) =
  let t = Timer.make () in
  let st = make_stats () in
  (* Collect function entry points, recording the defining block PC for closures *)
  let fun_entries = ref [ None, p.start, None, [] ] in
  Addr.Map.iter
    (fun def_pc block ->
      List.iter
        ~f:(function
          | Let (x, Closure (params, (pc, _), _)) ->
              fun_entries := (Some x, pc, Some def_pc, params) :: !fun_entries
          | _ -> ())
        block.body)
    p.blocks;

  (* Precompute per-function block submaps via DFS.
     Each function's blocks are disjoint, so total work is O(N log N). *)
  let fun_block_tbl = Addr.Hashtbl.create (List.length !fun_entries) in
  List.iter
    ~f:(fun (_, entry, _, _) ->
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
  Var.Hashtbl.reset guarded;
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
  (* A number constant [x] used in another representation gets a single
     variable bound to the constant in that representation, defined right
     after [x] (inserted once all functions have been processed). This
     costs nothing at run time and preserves sharing: all the boxed uses of
     [x] get the same static boxed value. *)
  let constant_twins = Var.Hashtbl.create 16 in
  let constant_twin x c ~into =
    let unboxed =
      match into with
      | Typing.Number (_, Unboxed) -> true
      | _ -> false
    in
    let twins = Var.Hashtbl.find_opt constant_twins x |> Option.value ~default:[] in
    match
      List.find_map
        ~f:(fun (unboxed', twin) ->
          if Bool.equal unboxed unboxed' then Some twin else None)
        twins
    with
    | Some (x', _) -> x'
    | None ->
        let x' = Var.fresh () in
        Typing.set_var_type types x' into;
        Var.Hashtbl.replace constant_twins x ((unboxed, (x', c)) :: twins);
        x'
  in
  List.iter
    ~f:(fun (name_opt, entry, def_pc_opt, params) ->
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
        lower_conversions
          fun_blocks
          types
          global_flow_info
          return_type
          free_pc
          ~constant_twin
          ~st
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
      let fun_blocks =
        hoist_boxes_out_of_loops ~types ~free_pc ~params ~st fun_blocks entry
      in
      let fun_blocks =
        if Config.Flag.lcm_hoist ()
        then hoist_unboxes_out_of_loops ~types ~free_pc ~assigned ~st fun_blocks entry
        else fun_blocks
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
  if Var.Hashtbl.length constant_twins > 0
  then
    blocks :=
      Addr.Map.map
        (fun b ->
          if
            List.exists
              ~f:(fun i ->
                match i with
                | Let (x, Constant _) -> Var.Hashtbl.mem constant_twins x
                | _ -> false)
              b.body
          then
            { b with
              body =
                List.concat_map
                  ~f:(fun i ->
                    match i with
                    | Let (x, Constant _) when Var.Hashtbl.mem constant_twins x ->
                        i
                        :: List.map
                             ~f:(fun (_, (x', c)) -> Let (x', Constant c))
                             (Var.Hashtbl.find constant_twins x)
                    | _ -> [ i ])
                  b.body
            }
          else b)
        !blocks;
  let p = { start = !start; blocks = !blocks; free_pc = !free_pc } in
  let p = hoist_boxes_out_of_closures ~types ~st p in
  let p = remove_dead_conversions ~st p in
  let misplaced_boxes = if check () || stats () then check_box_placement p else 0 in
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
       params widened, boxes hoisted: %d from loops %d from closures, %d unboxes hoisted \
       from loops, %d dead conversions and %d dead params removed, %d misplaced@."
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
      st.params_widened
      st.boxes_hoisted_from_loops
      st.boxes_hoisted_from_closures
      st.unboxes_hoisted_from_loops
      st.dead_conversions_removed
      st.dead_params_removed
      misplaced_boxes;
  if debug ()
  then (
    prerr_endline "AFTER";
    Print.program Format.err_formatter (fun _ _ -> "") p);
  p, types
