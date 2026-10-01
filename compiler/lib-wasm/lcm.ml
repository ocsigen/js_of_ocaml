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
   unbox on every iteration. This pass eliminates such redundancy through the phases
   below. The first four are run independently on each function, to avoid cross-function
   variable references.

   1. **Lowering** ([lower_conversions]): Materialises implicit representation mismatches
      as explicit IR primitives ([Wasm_conversion]).
      After this phase, every conversion is a visible instruction that can be analysed.

   2. **LCM dataflow and rewrite** ([process_function]): Applies the classical
      Knoop-Ruthing-Steffen Lazy Code Motion algorithm, in the edge-based
      formulation of Drechsler and Stadel. LCM finds the optimal placement:
      as early as necessary (to eliminate redundancy) but as late as possible (to avoid
      lengthening lifetimes or executing speculatively). The dataflow analyses are:
        - Anticipatability: can the conversion be moved to this point?
        - Availability: has the conversion already been computed on all paths?
        - Earliest (on an edge) = anticipated at the target, not available
          from the source, and not placeable any earlier
        - Later: can we push earliest placements further down?
        - Availability again, once the conversions are inserted
      The rewrite pass inserts conversions on the edges where they cannot be
      delayed any further (at the end of the source block) and substitutes
      redundant occurrences with the result of an earlier one.

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

   5. **Cleanup** ([remove_dead_conversions]): Once all the functions are processed,
      removes the conversions and the block parameters which the peephole pass and the
      widening have left without uses.

   Boxing conversions (box, tag) are placed in a specific way (see "Placement of boxing
   conversions" below): a number is boxed where the boxed value is used, but at most once
   per execution of its definition. So boxes are moved out of loops before the dataflow
   analysis ([hoist_boxes_out_of_loops]) and, once all the functions are processed, out of
   closures ([hoist_boxes_out_of_closures]); the boxes of a value which may still follow
   one another are then merged ([merge_repeated_boxes]). A number constant used in another
   representation is not converted: a variable is bound to the constant in that
   representation, right after the constant.

   The phases are ordered so that each feeds into the next: LCM creates optimal placements
   but may introduce inverse pairs; peephole cleans those up; widening then eliminates the
   conversions of values flowing through block parameters.

   LCM places conversions as late as possible, so no conversion is partially dead
   afterwards: there is no need for a partial dead code elimination phase.

   References: J. Knoop, O. Ruthing, B. Steffen, "Lazy Code Motion", PLDI 1992;
   K.-H. Drechsler, M. P. Stadel, "A variation of Knoop, Rüthing, and Steffen's
   Lazy Code Motion", SIGPLAN Notices 28(5), 1993. *)

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
  ; mutable later_iterations : int
  ; mutable conversions_inserted : int
  ; mutable conversions_eliminated : int
  ; mutable peephole_eliminated : int
  ; mutable params_widened : int
  ; mutable boxes_hoisted_from_loops : int
  ; mutable boxes_hoisted_from_closures : int
  ; mutable boxes_merged : int
  ; mutable dead_conversions_removed : int
  ; mutable dead_params_removed : int
  ; mutable time_lowering : float
  ; mutable time_dataflow : float
  ; mutable time_rewrite : float
  ; mutable time_peephole : float
  ; mutable time_widening : float
  ; mutable time_setup : float
  ; mutable time_boxes : float
  ; mutable time_cleanup : float
  }

let make_stats () =
  { functions_processed = 0
  ; functions_with_conversions = 0
  ; conversions_lowered = 0
  ; conversions_tracked = 0
  ; ant_iterations = 0
  ; avail_iterations = 0
  ; later_iterations = 0
  ; conversions_inserted = 0
  ; conversions_eliminated = 0
  ; peephole_eliminated = 0
  ; params_widened = 0
  ; boxes_hoisted_from_loops = 0
  ; boxes_hoisted_from_closures = 0
  ; boxes_merged = 0
  ; dead_conversions_removed = 0
  ; dead_params_removed = 0
  ; time_lowering = 0.
  ; time_dataflow = 0.
  ; time_rewrite = 0.
  ; time_peephole = 0.
  ; time_widening = 0.
  ; time_setup = 0.
  ; time_boxes = 0.
  ; time_cleanup = 0.
  }

let tick () = Sys.time ()

type conversion_kind = wasm_conversion

module Conv = struct
  type t = conversion_kind * Var.t

  (* In declaration order, as a polymorphic comparison, which is slower *)
  let kind_index = function
    | Unbox_i32 -> 0
    | Unbox_i64 -> 1
    | Unbox_nativeint -> 2
    | Unbox_f64 -> 3
    | Unbox_f32 -> 4
    | Box_i32 -> 5
    | Box_i64 -> 6
    | Box_nativeint -> 7
    | Box_f64 -> 8
    | Box_f32 -> 9
    | Untag_int -> 10
    | Tag_int -> 11

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

  val mem : t -> int -> bool

  val copy : t -> t

  val union : t -> t -> t

  val inter : t -> t -> t

  val diff : t -> t -> t

  (** In-place operations, which update their first argument *)

  val blit : src:t -> dst:t -> unit

  val union_into : t -> t -> unit

  val inter_into : t -> t -> unit

  val diff_into : t -> t -> unit

  val inter_diff_into : t -> t -> t -> unit
  (** [inter_diff_into a b c] sets [a] to [a ∩ (b \ c)] *)

  val inter_union_inter_into : t -> t -> t -> t -> unit
  (** [inter_union_inter_into a b c d] sets [a] to [a ∩ (b ∪ (c ∩ d))] *)

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

  let mem a i = a.(i / Sys.int_size) land (1 lsl (i mod Sys.int_size)) <> 0

  let copy = Array.copy

  let blit ~src ~dst = Array.blit ~src ~src_pos:0 ~dst ~dst_pos:0 ~len:(Array.length src)

  let union_into a b =
    for i = 0 to Array.length a - 1 do
      Array.unsafe_set a i (Array.unsafe_get a i lor b.(i))
    done

  let inter_into a b =
    for i = 0 to Array.length a - 1 do
      Array.unsafe_set a i (Array.unsafe_get a i land b.(i))
    done

  let diff_into a b =
    for i = 0 to Array.length a - 1 do
      Array.unsafe_set a i (Array.unsafe_get a i land lnot b.(i))
    done

  let inter_diff_into a b c =
    for i = 0 to Array.length a - 1 do
      Array.unsafe_set a i (Array.unsafe_get a i land b.(i) land lnot c.(i))
    done

  let inter_union_inter_into a b c d =
    for i = 0 to Array.length a - 1 do
      Array.unsafe_set a i (Array.unsafe_get a i land (b.(i) lor (c.(i) land d.(i))))
    done

  let union a b =
    let a = Array.copy a in
    union_into a b;
    a

  let inter a b =
    let a = Array.copy a in
    inter_into a b;
    a

  let diff a b =
    let a = Array.copy a in
    diff_into a b;
    a

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
  | Unbox_nativeint -> Some Box_nativeint
  | Unbox_f64 -> Some Box_f64
  | Unbox_f32 -> Some Box_f32
  | Box_i32 -> Some Unbox_i32
  | Box_i64 -> Some Unbox_i64
  | Box_nativeint -> Some Unbox_nativeint
  | Box_f64 -> Some Unbox_f64
  | Box_f32 -> Some Unbox_f32
  | Untag_int -> Some Tag_int
  | Tag_int -> Some Untag_int

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
  | Unbox_nativeint -> Poly.equal typ (Typing.Number (Typing.Nativeint, Typing.Boxed))
  | Unbox_f64 -> Poly.equal typ (Typing.Number (Typing.Float, Typing.Boxed))
  | Unbox_f32 -> Poly.equal typ (Typing.Number (Typing.Float32, Typing.Boxed))
  | Box_i32 | Box_i64 | Box_nativeint | Box_f64 | Box_f32 | Tag_int -> true
  | Untag_int -> (
      match typ with
      | Typing.Int
          (Typing.Integer.Ref | Typing.Integer.Normalized | Typing.Integer.Unnormalized)
        -> true
      | _ -> false)

(* Whether the result of a conversion should not be kept live across a
   call. Untagging a 31-bit integer is a single shift: recomputing it after
   the call is cheaper than keeping the result in a register, which the
   call forces to spill. (This would not hold with a more expensive
   integer representation, such as mixed 63-bit integers.) *)
let not_live_across_calls (kind, _) =
  match kind with
  | Untag_int -> true
  | Unbox_i32
  | Unbox_i64
  | Unbox_nativeint
  | Unbox_f64
  | Unbox_f32
  | Box_i32
  | Box_i64
  | Box_nativeint
  | Box_f64
  | Box_f32
  | Tag_int -> false

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
  match Typing.conversion ~from ~into with
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
      Typing.set_var_type types tmp (Typing.conversion_type kind);
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
    ~rebind_call
    ~direct_callee
    ~(st : lcm_stats) =
  (* A conversion of a number constant is replaced by a variable bound to
     the constant in the target representation (see [constant_twins]): a
     boxed constant is a static value and an unboxed one an immediate. *)
  let lower_var_conversion ~types ~from ~into x =
    let twin =
      match constant_of global_flow_info x, Typing.conversion ~from ~into with
      | Some c, Some kind -> (
          (* The constant may not be of the kind of number expected by the
             use, on a dead path (with GADTs, for instance): the conversion
             is then kept, and fails if it is ever executed. *)
          match c, kind with
          | Float _, (Box_f64 | Unbox_f64)
          | Int32 _, (Box_i32 | Unbox_i32)
          | Int64 _, (Box_i64 | Unbox_i64)
          | NativeInt _, (Box_nativeint | Unbox_nativeint) -> Some (c, kind)
          | ((Float32 _, (Box_f32 | Unbox_f32)) [@if oxcaml]) -> Some (c, kind)
          | _ -> None)
      | _ -> None
    in
    match twin with
    | Some (c, kind) -> [], constant_twin x c kind
    | None -> lower_var_conversion ~types ~from ~into x
  in
  let lower_apply x ~f ~args ~exact =
    (* The function is a value. It is an integer or an unboxed number on a
       dead path only. The copy is not known to the flow analysis, so the
       code generator does not call the function directly either. *)
    let tf = Typing.var_type types f in
    let lowered_f, f' = lower_var_conversion ~types ~from:tf ~into:Typing.Top f in
    let closure =
      if exact && Var.equal f f'
      then
        (* This should agree with the code generator: with double
           translation, a CPS call to a function without CPS version
           ([cont]) calls the function directly and passes the result to
           the continuation, which is the last argument. *)
        match Global_flow.get_unique_closure global_flow_info ~call:x f with
        | Some (g, params, cont)
          when direct_callee g
               && List.compare_length_with
                    args
                    ~len:(List.length params + if cont then 1 else 0)
                  = 0 -> Some (g, params, cont)
        | Some _ | None -> None
      else None
    in
    let target_types =
      match closure with
      | Some (_, params, cont) ->
          List.map params ~f:(Typing.var_type types) @ if cont then [ Typing.Top ] else []
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
    let lowered_args = lowered_f @ List.rev !lowered_args_rev in
    let from =
      match closure with
      | Some (g, _, false) -> Typing.return_type types g
      | Some (_, _, true) | None ->
          (* The result of the continuation, or of an unknown function, is a
             value. The code generator boxes the result of the direct call
             itself before passing it to the continuation: this conversion
             cannot be expressed here. *)
          Typing.Top
    in
    let into = Typing.var_type types x in
    match Typing.conversion ~from ~into with
    | None -> lowered_args @ [ Let (x, Apply { f = f'; args = args'; exact }) ]
    | Some kind ->
        let tmp = Var.fresh () in
        Typing.set_var_type types tmp from;
        (* The call is now bound to [tmp] *)
        rebind_call x tmp;
        lowered_args
        @ [ Let (tmp, Apply { f = f'; args = args'; exact })
          ; Let (x, Prim (prim_of_kind kind, [ Pv tmp ]))
          ]
  in
  let lower_prim x p args =
    let target_types_opt =
      let top = Typing.Top in
      let int_n = Typing.Int Typing.Integer.Normalized in
      let own a =
        match a with
        | Pv v -> Typing.var_type types v
        | Pc c -> Typing.constant_type c
      in
      (* Whether an argument is an untagged integer *)
      let is_int a =
        match own a with
        | Typing.Int (Typing.Integer.Normalized | Typing.Integer.Unnormalized) -> true
        | _ -> false
      in
      match p with
      | Extern ("caml_array_unsafe_get", _) | Array_get _ -> Some [ top; int_n ]
      | Extern (nm, hint) -> (
          match Typing.prim_sig nm with
          | Typed target_types, _ -> Some target_types
          | Values, _ -> Some (List.map ~f:(fun _ -> top) args)
          | Computed f, _ ->
              Some (f ~hint ~info:global_flow_info (List.map ~f:(fun a -> a, own a) args))
          | Any_representation, _ ->
              (* Unboxed numbers are boxed. The code generator converts the
                 other arguments itself, according to their types. *)
              Some
                (List.map
                   ~f:(fun a ->
                     match own a with
                     | Typing.Number (_, Unboxed) -> top
                     | t -> t)
                   args))
      | Eq _ | Neq _ -> (
          (* Untagged integers are compared directly, other values
             physically *)
          match args with
          | [ a; b ] when is_int a && is_int b -> None
          | _ -> Some (List.map ~f:(fun _ -> top) args))
      | Lt | Le | Ult -> Some [ int_n; int_n ]
      | IsInt _ | Vectlength _ ->
          (* The argument is a value. It is an integer on a dead path only,
             or, for [IsInt], when the test is known to succeed. *)
          Some [ top ]
      | Not -> (
          (* An untagged integer is tested directly, and any other value is
             compared with the tagged integer zero *)
          match args with
          | [ a ] when is_int a -> None
          | _ -> Some [ top ])
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
        | Lt | Le | Ult | IsInt _ | Eq _ | Neq _ | Not | Vectlength _ ->
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
  let float_u = Typing.Number (Typing.Float, Typing.Unboxed) in
  (* The block of a field access is a value. It is not an integer or an
     unboxed number but on a dead path, where the code generator still
     needs it as a value. *)
  let lower_block x =
    lower_var_conversion ~types ~from:(Typing.var_type types x) ~into:Typing.Top x
  in
  let lower_instr = function
    | Set_field (x, n, kind, y) ->
        let lowered_x, x' = lower_block x in
        let into =
          match kind with
          | Non_float | Immediate -> Typing.Top
          | Float -> float_u
        in
        let lowered_y, y' =
          lower_var_conversion ~types ~from:(Typing.var_type types y) ~into y
        in
        lowered_x @ lowered_y @ [ Set_field (x', n, kind, y') ]
    | Array_set (x, y, z) ->
        let int_n = Typing.Int Typing.Integer.Normalized in
        let lowered1, y' =
          lower_var_conversion ~types ~from:(Typing.var_type types y) ~into:int_n y
        in
        let lowered2, z' =
          lower_var_conversion ~types ~from:(Typing.var_type types z) ~into:Typing.Top z
        in
        let lowered0, x' = lower_block x in
        lowered0 @ lowered1 @ lowered2 @ [ Array_set (x', y', z') ]
    | Offset_ref (x, n) ->
        let lowered, x' = lower_block x in
        lowered @ [ Offset_ref (x', n) ]
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
          | Block (tag, fields, kind, mut, desc) when tag <> 254 ->
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
              List.rev !lowered_rev @ [ Let (x, Block (tag, fields, kind, mut, desc)) ]
          | Block (tag, fields, kind, mut, desc) ->
              (* The fields of a float array are unboxed floats *)
              let lowered_rev = ref [] in
              let fields =
                Array.map
                  ~f:(fun y ->
                    let lowered, y' =
                      lower_var_conversion
                        ~types
                        ~from:(Typing.var_type types y)
                        ~into:float_u
                        y
                    in
                    lowered_rev := List.rev_append lowered !lowered_rev;
                    y')
                  fields
              in
              List.rev !lowered_rev @ [ Let (x, Block (tag, fields, kind, mut, desc)) ]
          | Field (y, n, kind) ->
              let lowered, y' = lower_block y in
              lowered @ [ Let (x, Field (y', n, kind)) ]
          | _ -> [ Let (x, e) ]
        in
        let into = Typing.var_type types x in
        let from = get_from ~into e in
        let lowered, _ =
          match Typing.conversion ~from ~into with
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
    | Event _ as i -> [ i ]
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
          | Cond (v, cont1, cont2) -> (
              match Typing.var_type types v with
              | Number (_, Unboxed) as from ->
                  (* An unboxed number is tested on a dead path only: the
                     code generator needs it as a value *)
                  let lowered_v, v' =
                    lower_var_conversion ~types ~from ~into:Typing.Top v
                  in
                  lowered_v, Cond (v', cont1, cont2)
              | _ -> [], branch)
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
  if stats ()
  then
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

(* Variables that are the target of an [Assign]. They hold the context of
   an exception handler: they are parameters of the block which installs
   the handler ([Pushtrap]), but are only used in the handler.
   [Assign (x, y)], in the body of the [try], stands for passing [y] to the
   parameter [x] of the handler, should an exception be raised from then
   on. This avoids an explicit edge to the handler, with its arguments,
   from each instruction which may raise. So [x] is in effect defined at
   the entry of the handler, while in the CFG it is defined in the
   [Pushtrap] block, the only predecessor of the handler: a conversion of
   [x] must not be performed, nor considered as available, before the
   entry of the handler. To keep things simple, the conversions whose
   operand is one of these variables are left alone by the data flow
   analysis, the peephole pass and the widening of parameters: they are
   never moved, merged or threaded through block parameters. *)
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
     ([is_safe_input]) if the block contains an instruction which may
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

let is_call i =
  match i with
  | Let (_, Apply _) -> true
  | Let _ | Assign _ | Set_field _ | Offset_ref _ | Array_set _ | Event _ -> false

(* [not_live_convs] and [unsafe_convs] are the conversions whose result
   should not be kept live across a call, and those which may fail; [bits]
   converts a set of conversions into a bit vector, and [vars_bits] gives
   the conversions of a list of variables *)
let compute_local_props ~vars_bits ~not_live_convs ~unsafe_convs ~bits is_safe block =
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
  let kill = vars_bits killed_vars in
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
  (* The computed conversions whose result is not kept live across calls *)
  let comp_not_live = ref ConvSet.empty in
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
    comp := remove_conversions_of_var !comp v;
    comp_not_live := remove_conversions_of_var !comp_not_live v
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
              if not_live_across_calls conv
              then comp_not_live := ConvSet.add conv !comp_not_live
              else comp := ConvSet.add conv !comp;
              kill_var v
          | None -> kill_var v)
      | Let (v, Apply _) ->
          seen_call := true;
          comp_not_live := ConvSet.empty;
          kill_var v
      | Let (v, _) -> kill_var v
      | Assign (v, _) -> kill_assigned_var v
      | Set_field _ | Offset_ref _ | Array_set _ | Event _ -> ());
      if Typing.may_raise i then seen_raise := true)
    block.body;
  { kill
  ; kill_ant
  ; comp = bits (ConvSet.union !comp !comp_not_live)
  ; antloc = bits !antloc
  }

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

(* The blocks reachable from [entry], in reverse post-order. This is the
   order of [Structure.blocks_in_reverse_post_order] (successors visited by
   increasing address), without building the other parts of the control
   flow graph. *)
let reverse_post_order blocks entry =
  let visited = Addr.Hashtbl.create 16 in
  let l = ref [] in
  let rec visit pc =
    if not (Addr.Hashtbl.mem visited pc)
    then (
      Addr.Hashtbl.add visited pc ();
      Addr.Set.iter visit (Code.fold_children blocks pc Addr.Set.add Addr.Set.empty);
      l := pc :: !l)
  in
  visit entry;
  !l

let split_critical_edges ~preds blocks free_pc =
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

let compare_origin a b =
  match a, b with
  | Param i, Param j -> compare i j
  | Var v1, Var v2 -> Var.compare v1 v2
  | Param _, Var _ -> -1
  | Var _, Param _ -> 1

module AddrOrigin = struct
  type t = Addr.t * origin

  let compare (a1, o1) (a2, o2) =
    let c = compare a1 a2 in
    if c <> 0 then c else compare_origin o1 o2
end

module AddrOriginSet = Set.Make (AddrOrigin)
module AddrOriginMap = Map.Make (AddrOrigin)

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
                  (* Only the conversions of a parameter of the block are
                     candidates. When the operand is a free variable, all
                     predecessors see the same variable, so [can_supply]
                     would need the conversion to already exist in every
                     predecessor, which is rare (0 successes on ocamlc)
                     and expensive to check (deep DFS over 3000+
                     candidates). Free variables are still traced when
                     following a parameter across blocks. *)
                  if List.mem ~eq:Var.equal y block.params
                  then candidates := (pc, x, k, y) :: !candidates
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
      | Some (NativeInt _ as c), (Unbox_nativeint | Box_nativeint)
      | Some (Int _ as c), Untag_int -> Some c
      | ((Some (Float32 _ as c), (Unbox_f32 | Box_f32)) [@if oxcaml]) -> Some c
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
              match initial_origin with
              | Var _ ->
                  (* Not a candidate (see above) *)
                  ()
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
                            Typing.set_var_type types sv (Typing.conversion_type kind);
                            bpc, orig, sv)
                          all_shadows
                      in
                      let shadow_var_map =
                        List.fold_left
                          ~f:(fun m (bpc, orig, sv) -> AddrOriginMap.add (bpc, orig) sv m)
                          ~init:AddrOriginMap.empty
                          shadow_vars
                      in
                      let find_shadow_var bpc orig =
                        AddrOriginMap.find (bpc, orig) shadow_var_map
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
                                        Typing.set_var_type
                                          types
                                          u
                                          (Typing.conversion_type kind);
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
   optimal placement using dataflow analyses, then rewrite the IR.

   The analysis must be per-function to avoid cross-function variable references
   in the inserted conversion instructions. *)
let process_function types entry fun_blocks ~assigned ~global_flow_info ~(st : lcm_stats)
    =
  let all_convs, conv_types = get_all_conversions fun_blocks types ~assigned in
  st.functions_processed <- st.functions_processed + 1;
  if ConvSet.is_empty all_convs
  then fun_blocks
  else (
    st.functions_with_conversions <- st.functions_with_conversions + 1;
    st.conversions_tracked <- st.conversions_tracked + ConvSet.cardinal all_convs;
    let t0 = tick () in
    let is_safe (kind, var) = is_safe_input kind (Typing.var_type types var) in
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
    (* The conversions of each operand *)
    let ranks_by_operand = Var.Hashtbl.create 16 in
    Array.iteri
      ~f:(fun i (_, v) ->
        Var.Hashtbl.replace
          ranks_by_operand
          v
          (i :: (Var.Hashtbl.find_opt ranks_by_operand v |> Option.value ~default:[])))
      conv_of_rank;
    let vars_bits vars =
      let b = Bits.empty n_convs in
      List.iter
        ~f:(fun v ->
          match Var.Hashtbl.find_opt ranks_by_operand v with
          | Some l -> List.iter ~f:(fun i -> Bits.add b i) l
          | None -> ())
        vars;
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
        (compute_local_props ~vars_bits ~not_live_convs ~unsafe_convs ~bits is_safe)
        fun_blocks
    in
    let preds = CFG.predecessors fun_blocks in
    (* Forward analyses process the blocks in reverse post-order (index
       0..n-1), backward analyses in post-order (index n-1..0). *)
    let rpo_order = Array.of_list (reverse_post_order fun_blocks entry) in
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
    let index pc = Addr.Hashtbl.find rpo_index pc in
    let succ_index =
      Array.map ~f:(fun l -> Array.of_list (List.map ~f:index l)) succs_of
    in
    let pred_index =
      Array.map ~f:(fun l -> Array.of_list (List.map ~f:index l)) preds_of
    in
    let props_of = Array.init n_blocks ~f:(fun i -> Addr.Map.find rpo_order.(i) props) in
    (* The new value of a set is computed in [scratch], and only copied when
       it differs from the current value *)
    let scratch = Bits.empty n_convs in
    let update sets ri =
      if Bits.equal sets.(ri) scratch
      then false
      else (
        sets.(ri) <- Bits.copy scratch;
        true)
    in
    (* The conversions of the parameters of each block *)
    let param_convs_of =
      Array.init n_blocks ~f:(fun i ->
          vars_bits (Addr.Map.find rpo_order.(i) fun_blocks).params)
    in
    (* Step 1: Anticipatability (backward dataflow, all-paths).

       ANTIN(b) = set of conversions that will definitely be computed on every
       path from b to the function exit, before their operand is redefined.

       A conversion is anticipatable at a point if it is safe (and useful) to
       move its computation to that point.

       Equation: ANTIN(b) = ANTLOC(b) ∪ (ANTOUT(b) \ KILL_ANT(b))
                 ANTOUT(b) = ∩ { ANTIN(s) | s ∈ successors(b) }

       Conversions referencing a successor's block parameters are excluded from
       ANTOUT since those variables are rebound at the block boundary.

       This is the greatest solution of the equations (the sets are
       initialised to the universal set), so that a conversion is
       anticipated through a loop which does not use it. But a conversion
       is not anticipated where no exit of the function can be reached
       unless it is eventually computed: the greatest solution would make
       any conversion anticipated in a loop which is never left, and a
       conversion which may fail would then be moved before this loop,
       from a path where the loop is not entered. So the blocks from which
       no exit can be reached are dealt with first, taking the least
       solution (the sets are initialised to the empty set). All their
       successors are such blocks as well. *)
    let reaches_exit = Array.make n_blocks false in
    let rec mark_reaching ri =
      if not reaches_exit.(ri)
      then (
        reaches_exit.(ri) <- true;
        Array.iter ~f:mark_reaching pred_index.(ri))
    in
    Array.iteri
      ~f:(fun ri succs -> if Array.length succs = 0 then mark_reaching ri)
      succ_index;
    let antin =
      Array.init n_blocks ~f:(fun ri -> if reaches_exit.(ri) then all_convs else no_convs)
    in
    let changed = ref true in
    let solve_antin ~reaching =
      changed := true;
      while !changed do
        changed := false;
        (* Backward: sweep in post-order = reverse RPO *)
        for ri = n_blocks - 1 downto 0 do
          if Bool.equal reaches_exit.(ri) reaching
          then (
            st.ant_iterations <- st.ant_iterations + 1;
            let b_props = props_of.(ri) in
            let succs = succ_index.(ri) in
            (* ANTOUT *)
            if Array.length succs = 0
            then Bits.blit ~src:no_convs ~dst:scratch
            else (
              Bits.blit ~src:all_convs ~dst:scratch;
              Array.iter
                ~f:(fun si -> Bits.inter_diff_into scratch antin.(si) param_convs_of.(si))
                succs);
            Bits.diff_into scratch b_props.kill_ant;
            Bits.union_into scratch b_props.antloc;
            if update antin ri then changed := true)
        done
      done
    in
    if Array.exists ~f:(fun r -> not r) reaches_exit then solve_antin ~reaching:false;
    solve_antin ~reaching:true;
    (* Step 2: Availability (forward dataflow, all-paths).

       AVOUT(b) = set of conversions that have been computed on every path from
       the function entry to the exit of b, without their operand being redefined.

       Equation: AVIN(b)  = ∩ { AVOUT(p) | p ∈ predecessors(b) }
                 AVOUT(b) = COMP(b) ∪ (AVIN(b) \ KILL(b))

       The entry block is initialised to empty (nothing is available on entry). *)
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
        let ps = pred_index.(ri) in
        (* AVIN *)
        if pc = entry || Array.length ps = 0
        then Bits.blit ~src:no_convs ~dst:scratch
        else (
          Bits.blit ~src:all_convs ~dst:scratch;
          Array.iter ~f:(fun pi -> Bits.inter_into scratch avout.(pi)) ps);
        Bits.diff_into scratch b_props.kill;
        Bits.union_into scratch b_props.comp;
        if update avout ri then changed := true
      done
    done;
    (* Step 3: Placement (forward dataflow, all-paths), on the edges of the
       control flow graph (Drechsler and Stadel's formulation).

       A conversion is earliest on the edge from i to j if it is anticipated
       at the entry of j, not available at the exit of i, and cannot be
       placed any earlier: it is killed in i or is not anticipated at the
       exit of i.

         EARLIEST(i,j) = ANTIN(j) \ PARAMCONV(j) \ AVOUT(i)
                         ∩ (KILL_ANT(i) ∪ ¬ANTOUT(i))

       An insertion is then delayed as long as it does not meet a use:

         LATER(i,j) = EARLIEST(i,j) ∪ (LATERIN(i) \ ANTLOC(i))
         LATERIN(j) = NEW(j) ∪ ∩ { LATER(i,j) | i ∈ preds(j) }

       where NEW(j) are the conversions anticipated at j which cannot be
       placed before j: those of the parameters of j (PARAMCONV(j)), and
       all of them when j is the entry of the function.

       A conversion delayed down to the entry of a block which uses it is
       left where it is. Otherwise, it is inserted on the edges where it
       cannot be delayed any further:

         INSERT(i,j) = LATER(i,j) \ LATERIN(j)

       When j has a single predecessor, LATERIN(j) contains LATER(i,j), so
       there is nothing to insert. Otherwise, as critical edges have been
       split, j is the only successor of i, and the conversion is inserted
       at the end of i. In particular, a conversion anticipated at a loop
       header and not available when the loop is entered is inserted before
       the loop rather than in the header. *)
    let antin_edge =
      Array.init n_blocks ~f:(fun ri -> Bits.diff antin.(ri) param_convs_of.(ri))
    in
    (* The conversions which are earliest on the edges leaving a block, if
       anticipated at their target: ¬AVOUT(i) ∩ (KILL_ANT(i) ∪ ¬ANTOUT(i)) *)
    let earliest_from =
      Array.init n_blocks ~f:(fun ri ->
          let succs = succ_index.(ri) in
          let antout =
            if Array.length succs = 0
            then Bits.copy no_convs
            else
              let s = Bits.copy all_convs in
              Array.iter ~f:(fun si -> Bits.inter_into s antin_edge.(si)) succs;
              s
          in
          Bits.diff_into antout props_of.(ri).kill_ant;
          Bits.diff (Bits.diff all_convs antout) avout.(ri))
    in
    let is_root ri = rpo_order.(ri) = entry || Array.length pred_index.(ri) = 0 in
    let new_convs =
      Array.init n_blocks ~f:(fun ri ->
          if is_root ri then antin.(ri) else Bits.inter antin.(ri) param_convs_of.(ri))
    in
    let laterin = Array.make n_blocks all_convs in
    (* LATERIN(i) \ ANTLOC(i) *)
    let laterout = Array.make n_blocks all_convs in
    changed := true;
    while !changed do
      changed := false;
      (* Forward: sweep in RPO *)
      for ri = 0 to n_blocks - 1 do
        st.later_iterations <- st.later_iterations + 1;
        if is_root ri
        then Bits.blit ~src:new_convs.(ri) ~dst:scratch
        else (
          Bits.blit ~src:all_convs ~dst:scratch;
          Array.iter
            ~f:(fun pi ->
              Bits.inter_union_inter_into
                scratch
                laterout.(pi)
                antin_edge.(ri)
                earliest_from.(pi))
            pred_index.(ri);
          Bits.union_into scratch new_convs.(ri));
        ignore (update laterin ri : bool);
        Bits.diff_into scratch props_of.(ri).antloc;
        if update laterout ri then changed := true
      done
    done;
    (* The conversions to insert at the end of each block *)
    let insert_at_end = Array.make n_blocks no_convs in
    for ri = 0 to n_blocks - 1 do
      if not (is_root ri)
      then
        Array.iter
          ~f:(fun pi ->
            Bits.blit ~src:antin_edge.(ri) ~dst:scratch;
            Bits.inter_into scratch earliest_from.(pi);
            Bits.union_into scratch laterout.(pi);
            Bits.diff_into scratch laterin.(ri);
            if
              (not (Bits.equal scratch no_convs))
              && Array.for_all ~f:(fun si -> si = ri) succ_index.(pi)
            then insert_at_end.(pi) <- Bits.union insert_at_end.(pi) scratch)
          pred_index.(ri)
    done;
    (* Step 4: Availability, once the conversions are inserted.

       AVOUT'(b) = COMP(b) ∪ INSERT(b) ∪ (AVIN'(b) \ KILL(b))

       A use of a conversion which is available is replaced by the variable
       holding its result. *)
    let avout = Array.make n_blocks all_convs in
    avout.(0) <- no_convs;
    changed := true;
    while !changed do
      changed := false;
      (* Forward: sweep in RPO *)
      for ri = 0 to n_blocks - 1 do
        st.avail_iterations <- st.avail_iterations + 1;
        let b_props = props_of.(ri) in
        if is_root ri
        then Bits.blit ~src:no_convs ~dst:scratch
        else (
          Bits.blit ~src:all_convs ~dst:scratch;
          Array.iter ~f:(fun pi -> Bits.inter_into scratch avout.(pi)) pred_index.(ri));
        Bits.diff_into scratch b_props.kill;
        Bits.union_into scratch b_props.comp;
        Bits.union_into scratch insert_at_end.(ri);
        if update avout ri then changed := true
      done
    done;
    let avin =
      Array.init n_blocks ~f:(fun ri ->
          if is_root ri
          then no_convs
          else
            let s = Bits.copy all_convs in
            Array.iter ~f:(fun pi -> Bits.inter_into s avout.(pi)) pred_index.(ri);
            s)
    in
    (* Convert array results back to maps for the rewrite phase. *)
    let to_map arr =
      let m = ref Addr.Map.empty in
      for i = 0 to n_blocks - 1 do
        m := Addr.Map.add rpo_order.(i) arr.(i) !m
      done;
      !m
    in
    let avin = to_map avin in
    let insert_at_end = to_map insert_at_end in
    let t1 = tick () in
    st.time_dataflow <- st.time_dataflow +. (t1 -. t0);

    (* Rewrite phase: walk blocks in reverse post-order and apply the LCM results.

       For each block:
       1. Inherit available conversions from predecessors (conv_in): a mapping
          from (kind, operand) to the variable holding the already-computed result.
          Only conversions that are available (in AVIN) and agree across all
          processed predecessors are inherited.

       2. Walk the block body: for each conversion instruction, check if the
          conv_to_var mapping already has a result for it. If so, record a
          substitution (the original variable maps to the pre-computed one) and
          remove the instruction. Otherwise, keep it and register its result
          in conv_to_var for later uses.

       3. Insert the conversions placed at the end of the block. Each
          insertion creates a fresh variable and adds it to the conv_to_var
          mapping.

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
    (* A fresh variable holding the result of a conversion *)
    let conv_var ((kind, _) as conv) =
      let x = Var.fresh () in
      let typ =
        ConvMap.find_opt conv conv_types
        |> Option.value ~default:(Typing.conversion_type kind)
      in
      Typing.set_var_type types x typ;
      x
    in
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
        let add_phi conv =
          let phi_var = conv_var conv in
          phi_info :=
            Addr.Map.update
              pc
              (function
                | None -> Some [ conv, phi_var ]
                | Some l -> Some ((conv, phi_var) :: l))
              !phi_info;
          merge_phis := (conv, phi_var) :: !merge_phis
        in
        if all_preds_processed && List.length preds_pc > 1
        then List.iter ~f:(fun i -> add_phi conv_of_rank.(i)) differing;
        (* A conversion inserted at the end of a predecessor which has not
           been processed yet (on a back edge) is merged by a phi as well,
           when the other predecessors provide it: the predecessors
           processed so far hold it, and the remaining ones insert it. *)
        (if (not all_preds_processed) && not (List.is_empty processed_preds)
         then
           let remaining =
             List.filter ~f:(fun p -> not (Addr.Map.mem p !conv_out_map)) preds_pc
           in
           let inserted p = Addr.Map.find p insert_at_end in
           match remaining with
           | [] -> ()
           | p :: rem ->
               let candidates = Bits.inter (inserted p) (Addr.Map.find pc avin) in
               List.iter ~f:(fun p -> Bits.inter_into candidates (inserted p)) rem;
               Bits.iter
                 ~f:(fun i ->
                   let conv = conv_of_rank.(i) in
                   if List.for_all ~f:(fun t -> Tracker.mem ranks conv t) processed_preds
                   then add_phi conv)
                 candidates);
        let conv_to_var =
          ref
            (List.fold_left
               ~f:(fun t (conv, phi_var) -> Tracker.add ranks conv phi_var t)
               ~init:conv_in
               (List.rev !merge_phis))
        in
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
        (* The conversions placed at the end of the block *)
        Bits.iter
          ~f:(fun i ->
            let ((kind, arg) as conv) = conv_of_rank.(i) in
            if not (Tracker.mem ranks conv !conv_to_var)
            then (
              let tmp = conv_var conv in
              conv_to_var := Tracker.add ranks conv tmp !conv_to_var;
              st.conversions_inserted <- st.conversions_inserted + 1;
              body_rev := Let (tmp, Prim (prim_of_kind kind, [ Pv arg ])) :: !body_rev))
          (Addr.Map.find pc insert_at_end);
        let branch = Subst.Excluding_Binders.last subst_var block.branch in
        (* The untagged integer tested by a branch *)
        let int_arg v =
          match
            Typing.conversion
              ~from:(Typing.var_type types v)
              ~into:(Typing.Int Typing.Integer.Normalized)
          with
          | Some kind -> (
              match Tracker.find_opt ranks (kind, v) !conv_to_var with
              | Some tmp -> tmp
              | None -> v)
          | None -> v
        in
        (* Lowering has already replaced the arguments of the branch by
           their converted copies: only the tested variable remains *)
        let branch =
          match branch with
          | Cond (v, cont1, cont2) -> Cond (int_arg v, cont1, cont2)
          | Switch (v, conts) -> Switch (int_arg v, conts)
          | Stop | Return _ | Raise _ | Branch _ | Poptrap _ | Pushtrap _ -> branch
        in
        let new_block = { block with body = List.rev !body_rev; branch } in
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
                          (* The conversion is available at the entry of
                             the block: all its predecessors hold it *)
                          assert false)
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
       result := Addr.Map.map (fun b -> Subst.Excluding_Binders.block subst_var b) !result);
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
   contain the definition of [y]). Nor must a box of [y] be followed by
   another one: the data flow analysis removes a box when [y] has been
   boxed on all the paths leading to it, and the boxes remaining on a same
   path are merged ([merge_repeated_boxes]). *)

let is_boxing kind =
  match kind with
  | Box_i32 | Box_i64 | Box_nativeint | Box_f64 | Box_f32 | Tag_int -> true
  | Unbox_i32 | Unbox_i64 | Unbox_nativeint | Unbox_f64 | Unbox_f32 | Untag_int -> false

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
  let outermost (loops : t) pc ~def =
    Addr.Map.fold
      (fun header (body, size) acc ->
        if
          Addr.Set.mem pc body
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

(* Whether a block of the function contains an instruction satisfying [f] *)
let exists_instr f blocks = Addr.Map.exists (fun _ b -> List.exists ~f b.body) blocks

(* Move the boxes of a function out of the loops that do not contain the
   definition of the boxed value. [params] are the parameters of the
   function. The boxes of free variables are handled by
   [hoist_boxes_out_of_closures]. Assigned variables need no specific
   treatment here, although they are in effect defined at the entry of an
   exception handler rather than in the [Pushtrap] block (see
   [assigned_vars]): a loop which contains a use of such a variable, in
   the handler, but not the [Pushtrap] block lies inside the handler. *)
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
   variable is defined, before the creation of the closure. The functions
   of the resulting program are returned as well. *)
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
  then p, fns
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
                      (* A new block *)
                      (match ph with
                      | Some ph when not (Addr.Hashtbl.mem fns.fun_of_block ph) ->
                          Addr.Hashtbl.add fns.fun_of_block ph g;
                          Addr.Hashtbl.replace
                            fns.blocks_of_fun
                            g
                            (ph :: Addr.Hashtbl.find fns.blocks_of_fun g)
                      | Some _ | None -> ());
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
                  | (Let (_, Prim (Extern ("caml_cps_closure", _), _)) as i) :: rem ->
                      (* Double translation: pairing the two versions of a
                         function, this is part of the group (see
                         [Closure_conversion.iter_closures]) *)
                      insert (i :: rev_group) rem
                  | i :: rem -> List.rev_append rev_group (i :: insert [] rem)
                  | [] -> assert false
                in
                blocks := Addr.Map.add def_pc { b with body = insert [] b.body } !blocks)
          m)
      insertions;
    { p with blocks = !blocks; free_pc = !free_pc }, fns

(* The boxes which may be performed on a value that has already been boxed
   since it was defined. Each function is returned with its blocks and
   the conversions [(kind, y)] it performs at several places, when one of
   these places can be reached from another one without going through the
   definition of [y]. The places are given as the block and the result of
   each conversion, in execution order within a block, together with the
   block where [y] is defined ([None] for a parameter of the function).

   The data flow analysis only removes a conversion which is redundant on
   all paths, and does not see the boxes moved out of closures. As in
   [hoist_boxes_out_of_loops], assigned variables need no specific
   treatment: their boxes are all in the exception handler. *)
let repeated_boxes (p : program) fns =
  let sites = Var.Hashtbl.create 64 in
  let repeated = ref false in
  Addr.Map.iter
    (fun pc b ->
      List.iter
        ~f:(fun i ->
          match boxing i with
          | Some (x, kind, y) ->
              let l = Var.Hashtbl.find_opt sites y |> Option.value ~default:[] in
              if not (List.is_empty l) then repeated := true;
              Var.Hashtbl.replace sites y ((kind, pc, x) :: l)
          | None -> ())
        b.body)
    p.blocks;
  if not !repeated
  then []
  else
    (* The values boxed at several places of the function where they are
       defined *)
    let candidates = Addr.Hashtbl.create 16 in
    Var.Hashtbl.iter
      (fun y l ->
        match l with
        | [] | [ _ ] -> ()
        | (kind, pc, _) :: _ -> (
            let f = Addr.Hashtbl.find_opt fns.fun_of_block pc in
            match f, Var.Hashtbl.find_opt fns.def_fun y with
            | Some f, Some g
              when f = g
                   && List.for_all
                        ~f:(fun (kind', pc', _) ->
                          Poly.equal kind kind'
                          &&
                          match Addr.Hashtbl.find_opt fns.fun_of_block pc' with
                          | Some f' -> f = f'
                          | None -> false)
                        l ->
                Addr.Hashtbl.replace
                  candidates
                  f
                  (( (kind, y)
                   , Var.Hashtbl.find_opt fns.def_block y
                   , List.rev_map ~f:(fun (_, pc, x) -> pc, x) l )
                  :: (Addr.Hashtbl.find_opt candidates f |> Option.value ~default:[]))
            | _ -> ()))
      sites;
    Addr.Hashtbl.fold
      (fun f l acc ->
        let candidates = Array.of_list l in
        let n = Array.length candidates in
        let fb = fun_blocks p fns f in
        let repeated = Array.make n false in
        (* Forward analysis: the values which may have been boxed since
           they were defined, at the exit of each block *)
        let gen = Addr.Hashtbl.create 16 in
        let kill = Addr.Hashtbl.create 16 in
        let get tbl pc =
          match Addr.Hashtbl.find_opt tbl pc with
          | Some s -> s
          | None ->
              let s = Bits.empty n in
              Addr.Hashtbl.add tbl pc s;
              s
        in
        Array.iteri
          ~f:(fun i (_, def, sites) ->
            Option.iter ~f:(fun pc -> Bits.add (get kill pc) i) def;
            List.iter
              ~f:(fun (pc, _) ->
                let s = get gen pc in
                if Bits.mem s i then repeated.(i) <- true else Bits.add s i)
              sites)
          candidates;
        let preds = CFG.predecessors fb in
        (* An exception handler can be reached from any block of the body
           of the [try] *)
        let try_blocks = Addr.Hashtbl.create 16 in
        Addr.Map.iter
          (fun _ b ->
            match b.branch with
            | Pushtrap ((body, _), _, (handler, _)) ->
                let visited = ref Addr.Set.empty in
                let rec visit depth pc =
                  if not (Addr.Set.mem pc !visited)
                  then (
                    visited := Addr.Set.add pc !visited;
                    match (Addr.Map.find pc fb).branch with
                    | Poptrap (pc', _) -> if depth > 0 then visit (depth - 1) pc'
                    | Pushtrap ((pc', _), _, (pc'', _)) ->
                        visit (depth + 1) pc';
                        visit depth pc''
                    | Branch _ | Cond _ | Switch _ | Return _ | Raise _ | Stop ->
                        List.iter ~f:(visit depth) (CFG.successors fb pc))
                in
                visit 0 body;
                Addr.Hashtbl.replace
                  try_blocks
                  handler
                  (Addr.Set.elements !visited
                  @ (Addr.Hashtbl.find_opt try_blocks handler |> Option.value ~default:[])
                  )
            | Branch _ | Cond _ | Switch _ | Return _ | Raise _ | Stop | Poptrap _ -> ())
          fb;
        let preds pc =
          (Addr.Map.find_opt pc preds |> Option.value ~default:[])
          @ (Addr.Hashtbl.find_opt try_blocks pc |> Option.value ~default:[])
        in
        let out = Addr.Hashtbl.create 16 in
        (* The values which may have been boxed at the entry of a block,
           and are not defined in the block *)
        let boxed_at_entry pc =
          let s = Bits.empty n in
          List.iter
            ~f:(fun pc' ->
              match Addr.Hashtbl.find_opt out pc' with
              | Some s' -> Bits.union_into s s'
              | None -> ())
            (preds pc);
          (match Addr.Hashtbl.find_opt kill pc with
          | Some s' -> Bits.diff_into s s'
          | None -> ());
          s
        in
        let order = reverse_post_order fb f in
        let changed = ref true in
        while !changed do
          changed := false;
          List.iter
            ~f:(fun pc ->
              let s = boxed_at_entry pc in
              (match Addr.Hashtbl.find_opt gen pc with
              | Some s' -> Bits.union_into s s'
              | None -> ());
              match Addr.Hashtbl.find_opt out pc with
              | Some s' when Bits.equal s s' -> ()
              | Some _ | None ->
                  Addr.Hashtbl.replace out pc s;
                  changed := true)
            order
        done;
        Addr.Hashtbl.iter
          (fun pc s ->
            let s' = boxed_at_entry pc in
            Bits.inter_into s' s;
            Bits.iter ~f:(fun i -> repeated.(i) <- true) s')
          gen;
        let l = List.filteri ~f:(fun i _ -> repeated.(i)) l in
        if List.is_empty l then acc else (f, fb, l) :: acc)
      candidates
      []

(* Perform only once the boxes found by [repeated_boxes]: the value is
   boxed by the first of them when it is executed before all the other
   ones, and otherwise where the paths leading to them diverge (at the end
   of their nearest common dominator), outside of the loops which do not
   contain the definition of the value. *)
let merge_repeated_boxes ~types ~(st : lcm_stats) (p : program) fns =
  match repeated_boxes p fns with
  | [] -> p
  | l ->
      let blocks = ref p.blocks in
      let subst = Var.Hashtbl.create 16 in
      List.iter
        ~f:(fun (f, fb, groups) ->
          let g = Structure.control_flow_graph fb f in
          let idom = Structure.immediate_dominators g in
          let loops = lazy (Loops.compute fb f) in
          let rec dominators pc acc =
            let acc = Addr.Set.add pc acc in
            match Addr.Hashtbl.find_opt idom pc with
            | Some pc' when pc' <> pc -> dominators pc' acc
            | Some _ | None -> acc
          in
          let common_dominator pc pc' =
            let s = dominators pc Addr.Set.empty in
            let rec find pc' =
              if Addr.Set.mem pc' s then pc' else find (Addr.Hashtbl.find idom pc')
            in
            find pc'
          in
          List.iter
            ~f:(fun ((kind, y), def, sites) ->
              let pc =
                match sites with
                | (pc, _) :: rem ->
                    List.fold_left
                      ~f:(fun pc (pc', _) -> common_dominator pc pc')
                      ~init:pc
                      rem
                | [] -> assert false
              in
              (* The block still dominates the boxes, and is dominated by
                 the definition of the value, which is outside of the
                 loop *)
              let rec leave_loops pc =
                match Loops.outermost (Lazy.force loops) pc ~def with
                | None -> Some pc
                | Some (header, _) -> (
                    match Addr.Hashtbl.find_opt idom header with
                    | Some pc' when pc' <> header -> leave_loops pc'
                    | Some _ | None -> None)
              in
              match leave_loops pc with
              | None -> ()
              | Some pc ->
                  let x =
                    match List.find_opt ~f:(fun (pc', _) -> pc' = pc) sites with
                    | Some (_, x) -> x
                    | None ->
                        let _, x' = List.hd sites in
                        let x = Var.fork x' in
                        Typing.set_var_type types x (Typing.var_type types x');
                        append_instrs
                          blocks
                          pc
                          [ Let (x, Prim (prim_of_kind kind, [ Pv y ])) ];
                        x
                  in
                  List.iter
                    ~f:(fun (pc', x') ->
                      if not (Var.equal x x')
                      then (
                        st.boxes_merged <- st.boxes_merged + 1;
                        Var.Hashtbl.replace subst x' x;
                        let b = Addr.Map.find pc' !blocks in
                        let body =
                          List.filter
                            ~f:(fun i ->
                              match i with
                              | Let (x'', _) -> not (Var.equal x' x'')
                              | Assign _
                              | Set_field _
                              | Offset_ref _
                              | Array_set _
                              | Event _ -> true)
                            b.body
                        in
                        blocks := Addr.Map.add pc' { b with body } !blocks))
                    sites)
            groups)
        l;
      if Var.Hashtbl.length subst = 0
      then p
      else
        let subst_var x = Var.Hashtbl.find_opt subst x |> Option.value ~default:x in
        { p with blocks = Addr.Map.map (Subst.Excluding_Binders.block subst_var) !blocks }

(* Boxes which may be performed more than once per execution of the
   definition of the boxed value: boxes of free variables, boxes inside a
   loop which does not contain the definition of the value, and boxes
   which may follow another box of the same value *)
let check_box_placement (p : program) fns =
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
  List.iter
    ~f:(fun (_, _, groups) ->
      List.iter
        ~f:(fun ((_, y), _, sites) ->
          incr violations;
          if check ()
          then
            Format.eprintf
              "lcm: %a may be boxed several times (blocks %s)@."
              Var.print
              y
              (String.concat
                 ~sep:", "
                 (List.map ~f:(fun (pc, _) -> string_of_int pc) sites)))
        groups)
    (repeated_boxes p fns);
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
let f
    (p : program)
    (types : Typing.t)
    ~(live_vars : int array)
    ~(in_cps : Effects.in_cps)
    ~(global_flow_info : Global_flow.info) =
  let t = Timer.make () in
  let st = make_stats () in
  (* Whether the code generator calls the function [g] directly when it is
     the unique closure called: it must be live and defined in the program
     (see [Generate.translate_expr]) *)
  let direct_callee =
    let closures =
      Code.fold_closures_outermost_first
        p
        (fun name_opt _ _ _ s ->
          match name_opt with
          | Some g -> Var.Set.add g s
          | None -> s)
        Var.Set.empty
    in
    fun g -> live_vars.(Var.idx g) > 0 && Var.Set.mem g closures
  in
  let ts = tick () in
  (* Collect function entry points, recording the defining block PC for closures *)
  let fun_entries = ref [ None, p.start, None, [], [] ] in
  Addr.Map.iter
    (fun def_pc block ->
      List.iter
        ~f:(function
          | Let (x, Closure (params, (pc, args), _)) ->
              fun_entries := (Some x, pc, Some def_pc, params, args) :: !fun_entries
          | _ -> ())
        block.body)
    p.blocks;

  (* Precompute per-function block submaps via DFS.
     Each function's blocks are disjoint, so total work is O(N log N). *)
  let fun_block_tbl = Addr.Hashtbl.create (List.length !fun_entries) in
  List.iter
    ~f:(fun (_, entry, _, _, _) ->
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
  st.time_setup <- st.time_setup +. (tick () -. ts);
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
  let constant_twin x c kind =
    let twins = Var.Hashtbl.find_opt constant_twins x |> Option.value ~default:[] in
    match
      List.find_map
        ~f:(fun (kind', twin) -> if Poly.equal kind kind' then Some twin else None)
        twins
    with
    | Some (x', _) -> x'
    | None ->
        let x' = Var.fresh () in
        Typing.set_var_type types x' (Typing.conversion_type kind);
        Var.Hashtbl.replace constant_twins x ((kind, (x', c)) :: twins);
        x'
  in
  (* The code generator and the global flow analysis identify a call by
     the variable it is bound to, to tell whether it is a CPS call. A call
     bound to a fresh variable, as its result is converted, remains one. *)
  let in_cps = ref in_cps in
  let cps_calls = ref global_flow_info.info_cps_calls in
  let rebind_call x tmp =
    if Var.Set.mem x !in_cps then in_cps := Var.Set.add tmp !in_cps;
    if Var.Set.mem x !cps_calls then cps_calls := Var.Set.add tmp !cps_calls
  in
  List.iter
    ~f:(fun (name_opt, entry, def_pc_opt, params, entry_args) ->
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
          ~rebind_call
          ~direct_callee
          ~st
      in
      (* Splitting critical edges does not change which blocks have
         predecessors *)
      let preds = CFG.predecessors fun_blocks in
      let fun_blocks = split_critical_edges ~preds fun_blocks free_pc in
      st.time_lowering <- st.time_lowering +. (tick () -. t0);
      (* If the entry block has CFG predecessors (e.g. it's a loop header),
         split the entry edge: insert a forwarding block so that the implicit
         Closure→entry edge doesn't conflict with parameter widening. The
         parameters of the forwarding block have the types of the arguments
         of the function, and the conversions to the types of the parameters
         of the entry block, which may differ when the entry block is a loop
         header, are performed explicitly there. *)
      let ts2 = tick () in
      let fun_blocks, entry =
        match Addr.Map.find_opt entry preds with
        | Some (_ :: _) ->
            let entry_block = Addr.Map.find entry fun_blocks in
            let new_entry_pc = !free_pc in
            free_pc := new_entry_pc + 1;
            (* Fresh params for the new entry block *)
            let arg_types =
              if List.compare_lengths entry_args entry_block.params = 0
              then List.map ~f:(Typing.var_type types) entry_args
              else List.map ~f:(Typing.var_type types) entry_block.params
            in
            let new_params =
              List.map2
                ~f:(fun p typ ->
                  let p' = Var.fork p in
                  Typing.set_var_type types p' typ;
                  p')
                entry_block.params
                arg_types
            in
            let body, args =
              List.fold_right2
                ~f:(fun p' p (body, args) ->
                  let instrs, x =
                    lower_var_conversion
                      ~types
                      ~from:(Typing.var_type types p')
                      ~into:(Typing.var_type types p)
                      p'
                  in
                  instrs @ body, x :: args)
                new_params
                entry_block.params
                ~init:([], [])
            in
            let new_entry_block =
              { params = new_params; body; branch = Branch (entry, args) }
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
      st.time_setup <- st.time_setup +. (tick () -. ts2);
      let result =
        process_function types entry fun_blocks ~assigned ~global_flow_info ~st
      in
      let ts3 = tick () in
      Addr.Map.iter (fun pc block -> blocks := Addr.Map.add pc block !blocks) result;
      st.time_setup <- st.time_setup +. (tick () -. ts3))
    !fun_entries;
  let ts = tick () in
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
  let ts' = tick () in
  st.time_cleanup <- st.time_cleanup +. (ts' -. ts);
  let p, fns = hoist_boxes_out_of_closures ~types ~st p in
  let p = merge_repeated_boxes ~types ~st p fns in
  let ts = tick () in
  st.time_boxes <- st.time_boxes +. (ts -. ts');
  let p = remove_dead_conversions ~st p in
  st.time_cleanup <- st.time_cleanup +. (tick () -. ts);
  let misplaced_boxes = if check () || stats () then check_box_placement p fns else 0 in
  if times ()
  then (
    Format.eprintf "  lcm: %a@." Timer.print t;
    Format.eprintf
      "    lowering: %.2f@.    dataflow: %.2f@.    rewrite: %.2f@.    peephole: \
       %.2f@.    widening: %.2f@.    setup: %.2f@.    boxes: %.2f@.    cleanup: %.2f@."
      st.time_lowering
      st.time_dataflow
      st.time_rewrite
      st.time_peephole
      st.time_widening
      st.time_setup
      st.time_boxes
      st.time_cleanup);
  if stats ()
  then
    Format.eprintf
      "Stats - lcm: %d functions (%d with conversions), %d lowered, %d tracked, ant:%d \
       avail:%d later:%d iters, %d inserted, %d eliminated, %d peephole, %d params \
       widened, boxes hoisted: %d from loops %d from closures, %d boxes merged, %d dead \
       conversions and %d dead params removed, %d misplaced@."
      st.functions_processed
      st.functions_with_conversions
      st.conversions_lowered
      st.conversions_tracked
      st.ant_iterations
      st.avail_iterations
      st.later_iterations
      st.conversions_inserted
      st.conversions_eliminated
      st.peephole_eliminated
      st.params_widened
      st.boxes_hoisted_from_loops
      st.boxes_hoisted_from_closures
      st.boxes_merged
      st.dead_conversions_removed
      st.dead_params_removed
      misplaced_boxes;
  if debug ()
  then (
    prerr_endline "AFTER";
    Print.program Format.err_formatter (fun _ _ -> "") p);
  p, types, !in_cps, { global_flow_info with info_cps_calls = !cps_calls }
