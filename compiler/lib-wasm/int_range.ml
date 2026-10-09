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

(* Integer range analysis.

   We compute an interval for each integer variable, to find the
   arithmetic operations which cannot overflow: the result of such an
   operation is normalized when its operands are, so no normalization is
   needed before the uses of the value which require it (comparisons,
   array indices, divisions, ...). A range is only computed when no
   overflow can occur; otherwise the value is [Top].

   Each variable has a single range, but the operands of an expression
   are refined by the facts known where the expression is evaluated: the
   conditions of the branches that dominate it, and the bound checks
   performed before it. The arguments of a branch are refined by the
   condition of the branch as well, and an argument computed by an
   arithmetic operation is evaluated again with its operands refined.
   This deals with the counter of a [for] loop, which is incremented
   before the exit test [i <> hi] of the loop.

   A widening at loop headers and at the other points where cycles of
   dependencies can occur ensures termination. It uses as thresholds the
   bounds of the facts known on the edges leading to a loop header, so that
   a loop counter reaches the bound of its loop in one step. A narrowing
   phase then recovers some of the precision lost. *)

open! Stdlib
open Code

let debug = Debug.find "int-range"

let times = Debug.find "times"

type range =
  | Bot
  | Range of int64 * int64  (** An integer within these bounds *)
  | Tuple of range array  (** A block, with the ranges of its fields *)
  | Top  (** Unknown, possibly not an integer *)

type bound =
  | B_var of Var.t
  | B_const of int64

(* A fact about the value of a variable *)
type constr =
  | Lt of bound
  | Le of bound
  | Gt of bound
  | Ge of bound
  | Eq of bound
  | Ne of bound
  | Ult of bound
  | Within of int64 * int64
  | In_bounds of Var.t  (** A valid index of this array or string *)

type env = constr list Var.Map.t

type def =
  | Expr_def of expr * env  (** The facts known where the expression is evaluated *)
  | Param_def of (Var.t * env) list
      (** A block parameter: the arguments passed to it, with the facts
          known on the corresponding edge *)
  | Other

type t =
  { min_t : int64
  ; max_t : int64
  ; ranges : range Var.Tbl.t
  ; defs : def array
  ; def_block : Addr.t array
        (** The block defining each variable, [-1] for a closure parameter,
            [-2] if unknown *)
  ; dom_intervals : (Addr.t * int * int) Addr.Hashtbl.t
        (** For each block, its closure and its preorder and postorder numbers in
            the dominator tree of the closure *)
  ; global_flow_state : Global_flow.state
  ; global_flow_info : Global_flow.info
  ; reps : int array
        (** The index of the representative of the variables with the same
            value, see [compute_reps], or [-1] if not computed yet *)
  ; lengths : Var.t list Var.Hashtbl.t
        (** The variables holding the length of an array, string or bigarray,
            given by its representative (one per value number) *)
  ; members : Var.t list Var.Hashtbl.t
        (** The variables with a given representative, when there are
            several *)
  }

let int_constant c = Targetint.to_int64 c

let rec range_equal r r' =
  match r, r' with
  | Bot, Bot | Top, Top -> true
  | Range (l, h), Range (l', h') -> Int64.(l = l' && h = h')
  | Tuple t, Tuple t' ->
      Array.length t = Array.length t' && Array.for_all2 ~f:range_equal t t'
  | (Bot | Top | Range _ | Tuple _), _ -> false

(* Combine the fields of two tuples, which may have different lengths *)
let map2_fields f t t' =
  let l = Array.length t in
  let l' = Array.length t' in
  Array.init (max l l') ~f:(fun i ->
      if i < l then if i < l' then f t.(i) t'.(i) else t.(i) else t'.(i))

let rec join r r' =
  match r, r' with
  | Bot, r | r, Bot -> r
  | Range (l, h), Range (l', h') -> Range (Int64.min l l', Int64.max h h')
  | Tuple t, Tuple t' -> Tuple (map2_fields join t t')
  | (Top | Range _ | Tuple _), _ -> Top

(* Only used to refine a range which [r'] is known to over-approximate, or
   the other way around: when the two are not comparable, we keep [r'] *)
let rec meet r r' =
  match r, r' with
  | Bot, _ | _, Bot -> Bot
  | (Top, Range (l, h) | Range (l, h), Top) when Int64.(l > h) ->
      (* An unsatisfiable constraint, such as [x > max_int] *)
      Bot
  | Top, r | r, Top -> r
  | Range (l, h), Range (l', h') ->
      let l = Int64.max l l' in
      let h = Int64.min h h' in
      if Int64.(l > h) then Bot else Range (l, h)
  | Tuple t, Tuple t' when Array.length t = Array.length t' ->
      Tuple (Array.map2 ~f:meet t t')
  | (Range _ | Tuple _), _ -> r'

(* An upper bound on the length of arrays and strings. The runtime limits
   arrays to [Max_wosize = 2^28 - 1] elements (see [caml_make_vect]). The
   length of a byte sequence is a nonnegative integer which fits in an
   [i32] (see [caml_create_bytes]). *)
let max_length st = Int64.min st.max_t 0x7fffffffL

(* A range, or [Top] if the operation may have overflowed *)
let norm st l h = if Int64.(l < st.min_t || h > st.max_t) then Top else Range (l, h)

let rec print_range f r =
  match r with
  | Bot -> Format.fprintf f "bot"
  | Top -> Format.fprintf f "top"
  | Range (l, h) -> Format.fprintf f "[%Ld, %Ld]" l h
  | Tuple t ->
      Format.fprintf
        f
        "(%a)"
        (Format.pp_print_list ~pp_sep:(fun f () -> Format.fprintf f ",") print_range)
        (Array.to_list t)

let get st x = Var.Tbl.get st.ranges x

(* See [compute_reps] *)
let rep st x =
  let i = Var.idx x in
  if i < Array.length st.reps && st.reps.(i) >= 0 then Var.of_idx st.reps.(i) else x

let members st x =
  let r = rep st x in
  Var.Hashtbl.find_opt st.members r |> Option.value ~default:[ r ]

(* Whether the value of [x] is computed without overflow *)
let cannot_overflow st x =
  Var.idx x < Var.Tbl.length st.ranges
  &&
  match Var.Tbl.get st.ranges x with
  | Range _ -> true
  | Bot | Top | Tuple _ -> false

(* Some variables are created after the global flow analysis *)
let gf_def st x =
  let defs = st.global_flow_state.defs in
  if Var.idx x < Array.length defs
  then defs.(Var.idx x)
  else Global_flow.Phi { known = Var.Set.empty; others = true; unit = false }

let gf_approx st x : Global_flow.approx =
  if Var.idx x < Var.Tbl.length st.global_flow_info.info_approximation
  then Var.Tbl.get st.global_flow_info.info_approximation x
  else Top

let bound_range st b =
  match b with
  | B_const c -> Range (c, c)
  | B_var z -> get st z

let apply_constr st r c =
  match r with
  | Bot -> Bot
  | Tuple _ -> r
  | Top | Range _ -> (
      (* Comparisons are between integers: a bound of unknown range is
         still within the range of integers, so that [x < b] implies
         [x < max_int] *)
      let upper b f =
        match bound_range st b with
        | Bot -> Bot
        | Tuple _ -> r
        | Top -> meet r (Range (st.min_t, f st.min_t st.max_t))
        | Range (l, h) -> meet r (Range (st.min_t, f l h))
      in
      let lower b f =
        match bound_range st b with
        | Bot -> Bot
        | Tuple _ -> r
        | Top -> meet r (Range (f st.min_t st.max_t, st.max_t))
        | Range (l, h) -> meet r (Range (f l h, st.max_t))
      in
      match c with
      | Within (l, h) -> meet r (Range (l, h))
      | In_bounds _ -> r
      | Lt b -> upper b (fun _ h -> Int64.pred h)
      | Le b -> upper b (fun _ h -> h)
      | Gt b -> lower b (fun l _ -> Int64.succ l)
      | Ge b -> lower b (fun l _ -> l)
      | Eq b -> meet r (bound_range st b)
      | Ne b -> (
          match bound_range st b, r with
          | Bot, _ -> Bot
          | Range (c, c'), Range (l, h) when Int64.(c = c') ->
              if Int64.(l = c && h = c)
              then Bot
              else if Int64.(l = c)
              then Range (Int64.succ l, h)
              else if Int64.(h = c)
              then Range (l, Int64.pred h)
              else r
          | (Top | Range _ | Tuple _), _ -> r)
      | Ult b -> (
          (* [x <u b] implies [0 <= x < b] when [b] is non-negative *)
          match bound_range st b with
          | Bot -> Bot
          | Range (l, h) when Int64.(l >= 0L) -> meet r (Range (0L, Int64.pred h))
          | Top | Range _ | Tuple _ -> r))

let refine st env y =
  match Var.Map.find_opt y env with
  | None -> get st y
  | Some l -> List.fold_left ~f:(apply_constr st) ~init:(get st y) l

let arg_range st env a =
  match a with
  | Pc (Int c) ->
      let c = int_constant c in
      Range (c, c)
  | Pc _ -> Top
  | Pv y -> refine st env y

let pow2_above n =
  (* The smallest power of two strictly larger than [n >= 0] *)
  let rec loop p =
    if Int64.(p > n) || Int64.(p < 0L) then p else loop (Int64.shift_left p 1)
  in
  loop 1L

let small_magnitude x = Int64.(x <= 0x80000000L && x >= -0x80000000L)

let arith_prim st name rs =
  if List.exists ~f:(fun r -> range_equal r Bot) rs
  then Bot
  else
    (* The operands are integers *)
    let rs =
      List.map
        ~f:(fun r ->
          match r with
          | Top | Tuple _ -> Range (st.min_t, st.max_t)
          | Bot | Range _ -> r)
        rs
    in
    let width = Targetint.num_bits () in
    match name, rs with
    | "%int_add", [ Range (a, b); Range (c, d) ] ->
        norm st (Int64.add a c) (Int64.add b d)
    | "%int_sub", [ Range (a, b); Range (c, d) ] ->
        norm st (Int64.sub a d) (Int64.sub b c)
    | "%int_neg", [ Range (a, b) ] -> norm st (Int64.neg b) (Int64.neg a)
    | ("%int_mul" | "%direct_int_mul"), [ Range (a, b); Range (c, d) ]
      when List.for_all ~f:small_magnitude [ a; b; c; d ] ->
        let l = [ Int64.mul a c; Int64.mul a d; Int64.mul b c; Int64.mul b d ] in
        norm
          st
          (List.fold_left ~f:Int64.min ~init:Int64.max_int l)
          (List.fold_left ~f:Int64.max ~init:Int64.min_int l)
    | ("%int_div" | "%direct_int_div"), [ Range (a, b); Range (c, d) ]
      when Int64.(c = d && c > 0L) -> Range (Int64.div a c, Int64.div b c)
    | ("%int_div" | "%direct_int_div"), [ Range (a, b); (Range _ | Top) ] ->
        (* The quotient is not larger than the dividend in absolute
           value, except for [min_int / -1] which overflows *)
        let m = Int64.max (Int64.abs a) (Int64.abs b) in
        norm st (Int64.neg m) m
    | ("%int_mod" | "%direct_int_mod"), [ Range (a, b); d ] ->
        (* The remainder has the sign of the dividend, and is smaller
           than the divisor in absolute value *)
        let m =
          match d with
          | Range (c, d) ->
              Int64.max 0L (Int64.pred (Int64.max (Int64.abs c) (Int64.abs d)))
          | Top | Bot | Tuple _ -> st.max_t
        in
        Range
          ( (if Int64.(a >= 0L) then 0L else Int64.max a (Int64.neg m))
          , if Int64.(b <= 0L) then 0L else Int64.min b m )
    | "%int_and", [ r; r' ] -> (
        let non_negative r =
          match r with
          | Range (l, h) when Int64.(l >= 0L) -> Some h
          | Range _ | Top | Bot | Tuple _ -> None
        in
        match non_negative r, non_negative r' with
        | Some h, Some h' -> Range (0L, Int64.min h h')
        | Some h, None | None, Some h -> Range (0L, h)
        | None, None -> Top)
    | ("%int_or" | "%int_xor"), [ Range (a, b); Range (c, d) ]
      when Int64.(a >= 0L && c >= 0L) ->
        Range (0L, Int64.pred (pow2_above (Int64.max b d)))
    | "%int_lsr", [ r; Range (k, k') ] when Int64.(k = k' && k > 0L && k < of_int width)
      -> (
        let k = Int64.to_int k in
        match r with
        | Range (a, b) when Int64.(a >= 0L) ->
            Range (Int64.shift_right_logical a k, Int64.shift_right_logical b k)
        | Range _ | Top | Bot | Tuple _ -> Range (0L, Int64.shift_right st.max_t (k - 1)))
    | "%int_asr", [ Range (a, b); Range (k, k') ]
      when Int64.(k = k' && k >= 0L && k < of_int width) ->
        let k = Int64.to_int k in
        Range (Int64.shift_right a k, Int64.shift_right b k)
    | "%int_lsl", [ Range (a, b); Range (k, k') ]
      when Int64.(k = k' && k >= 0L && k < 31L) && small_magnitude a && small_magnitude b
      ->
        (* [a] and [b] fit in 32 bits: the shift does not overflow [int64] *)
        let k = Int64.to_int k in
        norm st (Int64.shift_left a k) (Int64.shift_left b k)
    | _ -> Top

let is_arith_prim name =
  match name with
  | "%int_add"
  | "%int_sub"
  | "%int_neg"
  | "%int_mul"
  | "%direct_int_mul"
  | "%int_div"
  | "%direct_int_div"
  | "%int_mod"
  | "%direct_int_mod"
  | "%int_and"
  | "%int_or"
  | "%int_xor"
  | "%int_lsr"
  | "%int_asr"
  | "%int_lsl" -> true
  | _ -> false

let extern_range st name args =
  if is_arith_prim name
  then arith_prim st name args
  else
    match name with
    | "caml_string_get"
    | "caml_string_unsafe_get"
    | "caml_bytes_get"
    | "caml_bytes_unsafe_get" -> Range (0L, 255L)
    | "caml_int_compare"
    | "caml_float_compare"
    | "caml_int32_compare"
    | "caml_int64_compare"
    | "caml_nativeint_compare" -> Range (-1L, 1L)
    | "caml_equal"
    | "caml_notequal"
    | "caml_lessthan"
    | "caml_lessequal"
    | "caml_greaterthan"
    | "caml_greaterequal"
    | "caml_string_equal"
    | "caml_string_notequal" -> Range (0L, 1L)
    | "caml_ml_string_length" | "caml_ml_bytes_length" -> Range (0L, max_length st)
    | _ -> Top

let is_mutable_field st z n =
  Var.idx z >= Array.length st.global_flow_state.mutable_fields
  ||
  match st.global_flow_state.mutable_fields.(Var.idx z) with
  | All_fields -> true
  | Some_fields s -> IntSet.mem n s
  | No_field -> false

(* Nested blocks are only tracked up to this depth, to ensure termination *)
let depth_threshold = 2

let rec limit depth r =
  match r with
  | Tuple t -> if depth = 0 then Top else Tuple (Array.map ~f:(limit (depth - 1)) t)
  | Bot | Range _ | Top -> r

let rec constant_range c =
  match c with
  | Int c ->
      let c = int_constant c in
      Range (c, c)
  | Tuple (_, a, _) -> Tuple (Array.map ~f:constant_range a)
  | _ -> Top

(* The elements of the arrays [y] may be bound to. As in [Typing], we rely
   on the global flow analysis to find these arrays. *)
let array_elements st y =
  match gf_approx st y with
  | Top | Values { others = true; _ } -> Top
  | Values { known; others = false } ->
      Var.Set.fold
        (fun z acc ->
          match st.global_flow_state.defs.(Var.idx z) with
          | Expr (Block (_, lst, _, _)) -> (
              match st.global_flow_state.mutable_fields.(Var.idx z) with
              | No_field ->
                  Array.fold_left ~f:(fun acc x -> join acc (get st x)) ~init:acc lst
              | Some_fields _ | All_fields -> Top)
          | Expr (Closure _) -> acc
          | Expr _ | Phi _ -> Top)
        known
        Bot

let eval_expr st env x e =
  match e with
  | Constant c -> limit depth_threshold (constant_range c)
  | Prim ((Array_get _ | Extern ("caml_array_unsafe_get", _)), [ Pv y; _ ]) ->
      array_elements st y
  | Prim (Extern (name, _), args) ->
      extern_range st name (List.map ~f:(arg_range st env) args)
  | Prim ((Not | IsInt _ | Eq _ | Neq _ | Lt | Le | Ult), _) -> Range (0L, 1L)
  | Prim (Vectlength _, _) -> Range (0L, max_length st)
  | Block (_, lst, _, _) ->
      limit
        depth_threshold
        (Tuple
           (Array.mapi
              ~f:(fun i y -> if is_mutable_field st x i then Top else get st y)
              lst))
  | Field (y, n, _) -> (
      match get st y with
      | Bot -> Bot
      | Tuple t when n < Array.length t -> t.(n)
      | Tuple _ | Range _ | Top -> Top)
  | Apply { f; args; _ } -> (
      match gf_approx st f with
      | Top | Values { others = true; _ } -> Top
      | Values { known; others = false } ->
          Var.Set.fold
            (fun g acc ->
              match st.global_flow_state.defs.(Var.idx g) with
              | Expr (Closure (params, _, _)) when List.length args = List.length params
                ->
                  Var.Set.fold
                    (fun y acc -> join acc (get st y))
                    (Var.Map.find g st.global_flow_state.return_values)
                    acc
              | Expr (Block _) -> acc
              | Expr _ | Phi _ -> Top)
            known
            Bot)
  | Prim _ | Closure _ | Special _ -> Top

(* The value of an argument passed on an edge where the facts [env]
   hold *)
let edge_arg_range st env y =
  let r = refine st env y in
  match st.defs.(Var.idx y) with
  | Expr_def ((Prim (Extern (name, _), _) as e), _) when is_arith_prim name ->
      meet r (eval_expr st env y e)
  | Expr_def _ | Param_def _ | Other -> r

let constrs env y = Var.Map.find_opt y env |> Option.value ~default:[]

(* The candidate bounds of a loop counter [x], from the facts [x <> hi] *)
let ne_bounds env x =
  List.filter_map
    ~f:(fun c ->
      match c with
      | Ne (B_var hi) -> Some hi
      | Ne (B_const _) | Lt _ | Le _ | Gt _ | Ge _ | Eq _ | Ult _ | Within _ | In_bounds _
        -> None)
    (constrs env x)

(* The step of the argument [y] of a loop counter [x], if [y] is [x + 1] or
   [x - 1] *)
let counter_step st x y =
  match st.defs.(Var.idx y) with
  | Expr_def
      (Prim (Extern ("%int_add", _), ([ Pv x'; Pc (Int c) ] | [ Pc (Int c); Pv x' ])), _)
    when Var.equal x x' -> (
      match int_constant c with
      | 1L -> Some `Up
      | -1L -> Some `Down
      | _ -> None)
  | Expr_def (Prim (Extern ("%int_sub", _), [ Pv x'; Pc (Int c) ]), _)
    when Var.equal x x' && Int64.(int_constant c = 1L) -> Some `Down
  | Expr_def _ | Param_def _ | Other -> None

(* Whether field [n] of the blocks [y] may be bound to is never modified:
   all the reads of this field then return the same value *)
let immutable_field st y n =
  Var.idx y < Array.length st.defs
  &&
  match gf_approx st y with
  | Values { known; others = false } when not (Var.Set.is_empty known) ->
      Var.Set.for_all
        (fun z ->
          match st.global_flow_state.defs.(Var.idx z) with
          | Expr (Block (_, a, _, _)) ->
              n < Array.length a && not (is_mutable_field st z n)
          | Expr _ | Phi _ -> false)
        known
  | Values _ | Top -> false

(* An expression whose value only depends on the value of its operands:
   an arithmetic operation, the length of an array or a string, the
   dimension of a bigarray, or the read of a field which is never modified.
   We return a name for the operation and its operands. *)
let pure_expr st e =
  match e with
  | Prim (Extern (name, _), args) when is_arith_prim name -> Some (name, args)
  | Prim (Extern (("caml_ml_string_length" | "caml_ml_bytes_length"), _), args) ->
      Some ("length", args)
  | Prim (Extern ("caml_ba_dim_1", _), args) -> Some ("dim_1", args)
  | Prim (Vectlength kind, args) ->
      Some
        ( (match kind with
          | Generic -> "vectlength"
          | Value -> "vectlength_value"
          | Float -> "vectlength_float")
        , args )
  | Field (y, n, _) when immutable_field st y n ->
      Some ("field" ^ string_of_int n, [ Pv y ])
  | _ -> None

(* The range of the value of a pure expression, given the ranges of its
   operands *)
let pure_range st e rs =
  match e, rs with
  | Prim (Extern (name, _), _), _ when is_arith_prim name -> arith_prim st name rs
  | ( Prim
        ( (Vectlength _ | Extern (("caml_ml_string_length" | "caml_ml_bytes_length"), _))
        , _ )
    , _ ) -> Range (0L, max_length st)
  | Field (_, n, _), [ Tuple t ] when n < Array.length t -> t.(n)
  | Field _, [ Bot ] -> Bot
  | _ -> Top

(* Whether [hi] keeps the same value while the parameter [x] is live: [hi]
   is defined in a block which strictly dominates the block of [x], or in an
   enclosing closure ([Dominating]). Then, a path which defines [hi] again
   also goes through an edge entering the block of [x] from outside the loop.
   Otherwise, [hi] has the same value each time it is defined if it is
   computed by a pure expression from operands which keep the same value, as
   when the loop test computes the length of an array again ([Pure r]). But
   [hi] may then not be defined in some iterations, and its range, computed
   with the facts known where it is defined, does not apply to this value:
   [r] is the range of the expression evaluated on the ranges of the
   operands. *)
type invariance =
  | Dominating
  | Pure of range

let rec invariance ?(depth = 3) st ~param:x hi =
  let pc = st.def_block.(Var.idx x) in
  let pc' = st.def_block.(Var.idx hi) in
  if
    pc' = -1
    || pc <> pc'
       &&
       match
         ( Addr.Hashtbl.find_opt st.dom_intervals pc
         , Addr.Hashtbl.find_opt st.dom_intervals pc' )
       with
       | Some (c, pre, post), Some (c', pre', post') ->
           c <> c' || (pre' <= pre && post <= post')
       | _ -> false
  then Some Dominating
  else if depth = 0
  then None
  else
    match st.defs.(Var.idx hi) with
    | Expr_def (e, _) -> (
        match pure_expr st e with
        | Some (_, args) ->
            let rs =
              List.map
                ~f:(fun a ->
                  match a with
                  | Pv y -> (
                      match invariance ~depth:(depth - 1) st ~param:x y with
                      | Some Dominating -> Some (get st y)
                      | Some (Pure r) -> Some r
                      | None -> None)
                  | Pc (Int c) ->
                      let c = int_constant c in
                      Some (Range (c, c))
                  | Pc _ -> Some Top)
                args
            in
            if List.exists ~f:Option.is_none rs
            then None
            else Some (Pure (pure_range st e (List.filter_map ~f:Fun.id rs)))
        | None -> None)
    | Param_def _ | Other -> None

let invariant_bound st ~param:x hi = Option.is_some (invariance st ~param:x hi)

(* A loop counter [x] which is incremented while [x <> hi], and whose initial
   values are at most [hi], remains at most [hi] provided that [hi] is loop
   invariant; this is the case of the counter of a [for] loop, which the
   interval domain cannot capture when [hi] is a variable. Similarly for a
   counter which is decremented. We return the bound [hi], the direction of
   the loop, and the range of the value of [hi] (see [invariance]), if
   applicable. *)
let counter_bound st x l =
  let increments, others =
    List.partition ~f:(fun (y, _) -> Option.is_some (counter_step st x y)) l
  in
  match increments with
  | [] -> None
  | (y, env) :: _ ->
      let dir = Option.get (counter_step st x y) in
      List.find_map
        ~f:(fun hi ->
          match invariance st ~param:x hi with
          | None -> None
          | Some inv ->
              (* A variable with the same value as [hi] where it is
                 defined (see [compute_reps]) *)
              let same hi' = Var.equal (rep st hi) (rep st hi') in
              let increment_ok (y, env) =
                Poly.equal (counter_step st x y) (Some dir)
                && List.exists ~f:same (ne_bounds env x)
              in
              (* The initial value [y] is on the right side of [hi] *)
              let initial_ok (y, env) =
                List.exists
                  ~f:(fun c ->
                    match dir, c with
                    | `Up, (Le (B_var hi') | Lt (B_var hi'))
                    | `Down, (Ge (B_var hi') | Gt (B_var hi')) -> same hi'
                    | _ -> false)
                  (constrs env y)
                ||
                let hi_range =
                  match inv with
                  | Dominating -> refine st env hi
                  | Pure r -> r
                in
                match dir, refine st env y, hi_range with
                | `Up, Range (_, h), Range (l, _) -> Int64.(h <= l)
                | `Down, Range (l, _), Range (_, h) -> Int64.(l >= h)
                | _ -> false
              in
              if
                List.for_all ~f:increment_ok increments
                && List.for_all ~f:initial_ok others
              then
                Some
                  ( hi
                  , dir
                  , match inv with
                    | Dominating -> get st hi
                    | Pure r -> r )
              else None)
        (ne_bounds env x)

(* The bounds [Le (B_var m)] or [Lt (B_var m)] of a counter [x] which is
   decremented: its other arguments are bounded by [m], which is loop
   invariant, and [x] is not [min_int] where it is decremented, so that
   [x - 1] does not wrap around. This is the case of the counter of a [for]
   loop down from [Array.length a - 1], which the interval domain only bounds
   by the range of [Array.length a - 1]. *)
let decreasing_counter_bounds st x l =
  let decrements, others =
    List.partition ~f:(fun (y, _) -> Poly.equal (counter_step st x y) (Some `Down)) l
  in
  (* The bounds of an argument [y], including [y] itself *)
  let bounds (y, env) =
    Le (B_var y)
    :: List.filter
         ~f:(fun c ->
           match c with
           | Le (B_var _) | Lt (B_var _) -> true
           | Le (B_const _)
           | Lt (B_const _)
           | Gt _ | Ge _ | Eq _ | Ne _ | Ult _ | Within _ | In_bounds _ -> false)
         (constrs env y)
  in
  (* [c] implies [c'] *)
  let implies c c' =
    match c, c' with
    | (Le (B_var m) | Lt (B_var m)), Le (B_var m') | Lt (B_var m), Lt (B_var m') ->
        Var.equal m m'
    | _ -> false
  in
  let no_wrap (_, env) =
    match refine st env x with
    | Range (l, _) -> Int64.(l > st.min_t)
    | Bot -> true
    | Top | Tuple _ -> false
  in
  match decrements, others with
  | [], _ | _, [] -> []
  | _, edge :: _ ->
      if List.for_all ~f:no_wrap decrements
      then
        List.filter
          ~f:(fun c ->
            match c with
            | Le (B_var m) | Lt (B_var m) ->
                invariant_bound st ~param:x m
                && List.for_all
                     ~f:(fun e -> List.exists ~f:(fun c' -> implies c' c) (bounds e))
                     others
            | _ -> false)
          (bounds edge)
      else []

let compute st x =
  match st.defs.(Var.idx x) with
  | Expr_def (e, env) -> eval_expr st env x e
  | Param_def l -> (
      let r =
        List.fold_left
          ~f:(fun acc (y, env) -> join acc (edge_arg_range st env y))
          ~init:Bot
          l
      in
      match counter_bound st x l with
      | None -> r
      | Some (_, dir, hi_range) -> (
          match dir, hi_range with
          | `Up, Range (_, h) -> meet r (Range (st.min_t, h))
          | `Down, Range (l, _) -> meet r (Range (l, st.max_t))
          | _, Bot -> Bot
          | _, (Range _ | Top | Tuple _) -> r))
  | Other -> (
      match gf_def st x with
      | Phi { others = true; _ } | Expr _ -> Top
      | Phi { known; others = false; unit } ->
          Var.Set.fold
            (fun y acc -> join acc (get st y))
            known
            (if unit then Range (0L, 0L) else Bot))

let is_access name =
  match name with
  | "caml_check_bound"
  | "caml_check_bound_gen"
  | "caml_check_bound_float"
  | "caml_string_get"
  | "caml_bytes_get"
  | "caml_string_set"
  | "caml_bytes_set"
  | "caml_ba_get_1"
  | "caml_ba_set_1" -> true
  | _ -> false

let is_checked_access name =
  match name with
  | "caml_check_bound"
  | "caml_check_bound_gen"
  | "caml_check_bound_float"
  | "caml_string_get"
  | "caml_bytes_get"
  | "caml_string_set"
  | "caml_bytes_set" -> true
  | _ -> false

module Value_key = Hashtbl.Make (struct
  type t = string * [ `Var of int | `Const of int64 ] list

  let equal (name, args) (name', args') =
    String.equal name name'
    && List.equal
         ~eq:(fun a a' ->
           match a, a' with
           | `Var i, `Var i' -> i = i'
           | `Const c, `Const c' -> Int64.equal c c'
           | (`Var _ | `Const _), _ -> false)
         args
         args'

  let hash = Hashtbl.hash
end)

(* Variables with the same value are identified by a representative: a
   checked array access returns the array, and variables computed by the
   same pure expression from operands with the same representatives have
   the same value. Two such variables have the same value where the
   definitions of both dominate. *)
let compute_reps st p =
  let keys = Value_key.create 128 in
  let rec rep x =
    let i = Var.idx x in
    let r = st.reps.(i) in
    if r >= 0
    then r
    else
      let r =
        match st.defs.(i) with
        | Expr_def
            ( Prim
                ( Extern
                    ( ( "caml_check_bound"
                      | "caml_check_bound_float"
                      | "caml_check_bound_gen" )
                    , _ )
                , Pv a :: _ )
            , _ ) -> rep a
        | Expr_def (e, _) -> (
            match pure_expr st e with
            | Some (name, args) -> (
                let args =
                  List.map
                    ~f:(fun a ->
                      match a with
                      | Pv y -> Some (`Var (rep y))
                      | Pc (Int c) -> Some (`Const (int_constant c))
                      | Pc _ -> None)
                    args
                in
                if List.exists ~f:Option.is_none args
                then i
                else
                  let key = name, List.filter_map ~f:Fun.id args in
                  match Value_key.find_opt keys key with
                  | Some r ->
                      let r' = Var.of_idx r in
                      Var.Hashtbl.replace
                        st.members
                        r'
                        (x
                        :: (Var.Hashtbl.find_opt st.members r'
                           |> Option.value ~default:[ r' ]));
                      r
                  | None ->
                      Value_key.add keys key i;
                      (match key with
                      | ( ( "length"
                          | "dim_1"
                          | "vectlength"
                          | "vectlength_value"
                          | "vectlength_float" )
                        , [ `Var a ] ) ->
                          let a = Var.of_idx a in
                          Var.Hashtbl.replace
                            st.lengths
                            a
                            (x
                            :: (Var.Hashtbl.find_opt st.lengths a
                               |> Option.value ~default:[]))
                      | _ -> ());
                      i)
            | None -> i)
        | Param_def _ | Other -> i
      in
      st.reps.(i) <- r;
      r
  in
  Addr.Map.iter
    (fun _ block ->
      List.iter
        ~f:(fun i ->
          match i with
          | Let (x, _) -> ignore (rep x)
          | Assign _ | Set_field _ | Offset_ref _ | Array_set _ | Event _ -> ())
        block.body)
    p.blocks

(* The size argument of the primitive which created the array or string
   [a], which is then its length *)
let creation_size st a =
  match st.defs.(Var.idx a) with
  | Expr_def
      ( Prim
          ( Extern
              ( ( "caml_make_vect"
                | "caml_array_make"
                | "caml_uniform_array_make"
                | "caml_floatarray_make"
                | "caml_make_float_vect"
                | "caml_floatarray_create"
                | "caml_array_create_float"
                | "caml_floatarray_create_local"
                | "caml_create_bytes"
                | "caml_create_local_bytes" )
              , _ )
          , size :: _ )
      , _ ) -> Some size
  | Expr_def _ | Param_def _ | Other -> None

(* The variables holding the length of the array, string or bigarray
   [obj], a representative: its array lengths, string lengths or
   dimensions, and the size it was created with. They have the length of
   [obj] where their definition dominates. *)
let length_vars st obj =
  List.concat_map
    ~f:(members st)
    ((Var.Hashtbl.find_opt st.lengths obj |> Option.value ~default:[])
    @
    match creation_size st obj with
    | Some (Pv size) -> [ size ]
    | Some (Pc _) | None -> [])

(* Whether [n] is the length of the array, string or bigarray [obj], a
   representative: its array length, string length or dimension, or the size
   it was created with. Whatever the kind of a [Vectlength], it does not
   exceed the length of the array: the float array length is 0 for another
   array, and a [Value] array length only applies to arrays which are not
   float arrays. *)
let is_length st ~obj n =
  (match st.defs.(Var.idx (rep st n)) with
    | Expr_def
        ( Prim
            ( ( Vectlength _
              | Extern
                  (("caml_ml_string_length" | "caml_ml_bytes_length" | "caml_ba_dim_1"), _)
                )
            , [ Pv a ] )
        , _ ) -> Var.equal (rep st a) obj
    | Expr_def _ | Param_def _ | Other -> false)
  ||
  match creation_size st obj with
  | Some (Pv size) -> Var.equal (rep st size) (rep st n)
  | Some (Pc _) | None -> false

(* A lower bound on the length of the array or string [obj], a
   representative of the accessed variable [a]: a string constant, a block,
   an array or string created with a size whose range is known, or the
   blocks [a] may be bound to. We use the approximation of [a] rather than
   of [obj], which may be refined differently: the approximation of a field
   read depends on the case of a [switch] it is in. *)
let min_length st ~accessed:a obj =
  match st.defs.(Var.idx obj) with
  | Expr_def (Block (_, l, _, _), _) -> Some (Int64.of_int (Array.length l))
  | Expr_def (Constant (String s), _) -> Some (Int64.of_int (String.length s))
  | _ -> (
      match creation_size st obj with
      | Some (Pc (Int c)) -> Some (int_constant c)
      | Some (Pv n) -> (
          match get st n with
          | Range (l, _) -> Some l
          | Bot | Top | Tuple _ -> None)
      | Some (Pc _) -> None
      | None -> (
          match gf_approx st a with
          | Values { known; others = false } when not (Var.Set.is_empty known) ->
              Var.Set.fold
                (fun z acc ->
                  match acc, st.global_flow_state.defs.(Var.idx z) with
                  | Some m, Expr (Block (_, l, _, _)) ->
                      Some (Int64.min m (Int64.of_int (Array.length l)))
                  | _ -> None)
                known
                (Some Int64.max_int)
          | Values _ | Top -> None))

(* [x] is computed as [y + k] *)
let offset_def st x =
  match st.defs.(Var.idx x) with
  | Expr_def
      (Prim (Extern ("%int_add", _), ([ Pv y; Pc (Int k) ] | [ Pc (Int k); Pv y ])), _) ->
      Some (y, int_constant k)
  | Expr_def (Prim (Extern ("%int_sub", _), [ Pv y; Pc (Int k) ]), _) ->
      Some (y, Int64.neg (int_constant k))
  | Expr_def _ | Param_def _ | Other -> None

(* The terms [(z, k)] such that the index [i] is equal to [z + k] at an
   access where the facts [env] hold: [i] itself, the variables of the same
   value which have facts there (their definition then dominates the
   access), and the same for [y] if [i] is computed as [y + k] *)
let index_terms ?(check = true) st env i =
  let same z =
    z
    :: List.filter
         ~f:(fun z' -> (not (Var.equal z z')) && not (List.is_empty (constrs env z')))
         (members st z)
  in
  let rec terms depth (z, k) acc =
    let acc = List.map ~f:(fun z' -> z', k) (same z) @ acc in
    match offset_def st z with
    | Some (y, k') when depth > 0 && ((not check) || cannot_overflow st z) ->
        terms (depth - 1) (y, Int64.add k k') acc
    | Some _ | None -> acc
  in
  terms 2 (i, 0L) []

(* The upper bounds [(v, c)] of a variable [z] where the facts [env] hold:
   [z <= v + c]. They come from the facts known about [z], from its loop
   counter bounds, and [z <= v] becomes [z <= v - 1] if [z <> v]. Then, [v]
   is replaced by [w] when [v] is computed as [w + k]: [z <= w + k + c]
   holds unless [w + k] is less than [min_int], since a sum which wraps
   around to the negative numbers is less than its exact value. This is the
   case when [k] is nonnegative, or [w] is large enough, in particular when
   it is nonnegative ([nonneg w]). *)
let upper_bounds st ~nonneg env z =
  let cs = constrs env z in
  let direct =
    List.filter_map
      ~f:(fun c ->
        match c with
        | Lt (B_var v) -> Some (v, -1L)
        | Le (B_var v) | Eq (B_var v) -> Some (v, 0L)
        | Ult (B_var v) -> (
            (* [z <u v] implies [z < v] when [v] is nonnegative *)
            match refine st env v with
            | Range (l, _) when Int64.(l >= 0L) -> Some (v, -1L)
            | Range _ | Bot | Top | Tuple _ -> None)
        | Lt (B_const _)
        | Le (B_const _)
        | Eq (B_const _)
        | Ult (B_const _)
        | Gt _ | Ge _ | Ne _ | Within _ | In_bounds _ -> None)
      cs
  in
  let counter =
    match st.defs.(Var.idx z) with
    | Param_def l ->
        (match counter_bound st z l with
          | Some (hi, `Up, _) -> [ hi, 0L ]
          | Some (_, `Down, _) | None -> [])
        @ List.filter_map
            ~f:(fun c ->
              match c with
              | Le (B_var m) -> Some (m, 0L)
              | Lt (B_var m) -> Some (m, -1L)
              | _ -> None)
            (decreasing_counter_bounds st z l)
    | Expr_def _ | Other -> []
  in
  let not_equal v =
    List.exists
      ~f:(fun c ->
        match c with
        | Ne (B_var v') -> Var.equal (rep st v) (rep st v')
        | _ -> false)
      cs
  in
  let rec normalize depth (v, c) acc =
    let acc = (v, c) :: acc in
    match offset_def st v with
    | Some (w, k)
      when depth > 0
           && (Int64.(k >= 0L)
              || nonneg w
              ||
              match get st w with
              | Range (l, _) -> Int64.(add l k >= st.min_t)
              | Bot | Top | Tuple _ -> false) ->
        normalize (depth - 1) (w, Int64.add c k) acc
    | Some _ | None -> acc
  in
  List.fold_left
    ~f:(fun acc (v, c) ->
      let c = if Int64.equal c 0L && not_equal v then -1L else c in
      normalize 2 (v, c) acc)
    ~init:[]
    (((z, 0L) :: direct) @ counter)

(* The variables [x] depends on *)
let dependencies st x =
  let bound_vars env y acc =
    List.fold_left
      ~f:(fun acc c ->
        match c with
        | Lt (B_var z)
        | Le (B_var z)
        | Gt (B_var z)
        | Ge (B_var z)
        | Eq (B_var z)
        | Ne (B_var z)
        | Ult (B_var z) -> z :: acc
        | Lt (B_const _)
        | Le (B_const _)
        | Gt (B_const _)
        | Ge (B_const _)
        | Eq (B_const _)
        | Ne (B_const _)
        | Ult (B_const _)
        | Within _ | In_bounds _ -> acc)
      ~init:acc
      (constrs env y)
  in
  let arg_vars env a acc =
    match a with
    | Pc _ -> acc
    | Pv y -> y :: bound_vars env y acc
  in
  let array_elements y acc =
    match gf_approx st y with
    | Top -> acc
    | Values { known; _ } ->
        Var.Set.fold
          (fun z acc ->
            match st.global_flow_state.defs.(Var.idx z) with
            | Expr (Block (_, lst, _, _)) -> Array.to_list lst @ acc
            | Expr _ | Phi _ -> acc)
          known
          acc
  in
  let expr_vars env e acc =
    match e with
    | Prim ((Array_get _ | Extern ("caml_array_unsafe_get", _)), [ Pv y; _ ]) ->
        array_elements y acc
    | Prim (Extern (name, _), (Pv a :: i :: _ as args)) when is_access name ->
        (* See [valid_index]: the length variables of the object which have
           facts at the access, the size it was created with, and the
           variables the index is computed from *)
        let a = rep st a in
        let acc =
          List.fold_left
            ~f:(fun acc n ->
              if List.is_empty (constrs env n) then acc else arg_vars env (Pv n) acc)
            ~init:acc
            (length_vars st a)
        in
        let acc =
          match creation_size st a with
          | Some (Pv n) -> n :: acc
          | Some (Pc _) | None -> acc
        in
        let acc =
          match i with
          | Pv i ->
              List.fold_left
                ~f:(fun acc (z, _) -> arg_vars env (Pv z) acc)
                ~init:acc
                (index_terms ~check:false st env i)
          | Pc _ -> acc
        in
        List.fold_left ~f:(fun acc a -> arg_vars env a acc) ~init:acc args
    | Prim (_, args) -> List.fold_left ~f:(fun acc a -> arg_vars env a acc) ~init:acc args
    | Field (y, _, _) -> y :: acc
    | Block (_, lst, _, _) -> Array.to_list lst @ acc
    | Apply { f; _ } -> (
        match gf_approx st f with
        | Top -> acc
        | Values { known; _ } ->
            Var.Set.fold
              (fun g acc ->
                match Var.Map.find_opt g st.global_flow_state.return_values with
                | Some s -> Var.Set.fold (fun y acc -> y :: acc) s acc
                | None -> acc)
              known
              acc)
    | Constant _ | Closure _ | Special _ -> acc
  in
  match st.defs.(Var.idx x) with
  | Expr_def (e, env) -> expr_vars env e []
  | Param_def l ->
      (* The possible bounds of a loop counter, see [counter_bound] *)
      let bounds = List.concat_map ~f:(fun (_, env) -> ne_bounds env x) l in
      List.fold_left
        ~f:(fun acc (y, env) ->
          let acc = arg_vars env (Pv y) acc in
          let acc =
            List.fold_left ~f:(fun acc hi -> hi :: bound_vars env hi acc) ~init:acc bounds
          in
          match st.defs.(Var.idx y) with
          | Expr_def ((Prim (Extern (name, _), _) as e), _) when is_arith_prim name ->
              expr_vars env e acc
          | Expr_def _ | Param_def _ | Other -> acc)
        ~init:[]
        l
  | Other -> (
      match gf_def st x with
      | Phi { known; _ } -> Var.Set.elements known
      | Expr _ -> [])

let add_constr env y c = Var.Map.add y (c :: constrs env y) env

let bound_of_arg a =
  match a with
  | Pv z -> Some (B_var z)
  | Pc (Int c) -> Some (B_const (int_constant c))
  | Pc _ -> None

(* The facts known when [v] is true ([pol]) or false *)
let rec add_cond st env v pol =
  match st.defs.(Var.idx v) with
  | Expr_def (Prim (Not, [ Pv w ]), _) -> add_cond st env w (not pol)
  | Expr_def (Prim (((Lt | Le | Eq _ | Neq _ | Ult) as op), [ a; b ]), _) -> (
      match bound_of_arg a, bound_of_arg b with
      | Some ba, Some bb -> (
          let on arg c env =
            match arg with
            | Pv y -> add_constr env y c
            | Pc _ -> env
          in
          match op, pol with
          | Lt, true -> env |> on a (Lt bb) |> on b (Gt ba)
          | Lt, false -> env |> on a (Ge bb) |> on b (Le ba)
          | Le, true -> env |> on a (Le bb) |> on b (Ge ba)
          | Le, false -> env |> on a (Gt bb) |> on b (Lt ba)
          | Eq _, true | Neq _, false -> env |> on a (Eq bb) |> on b (Eq ba)
          | Eq _, false | Neq _, true -> env |> on a (Ne bb) |> on b (Ne ba)
          | Ult, true -> env |> on a (Ult bb)
          | Ult, false -> env
          | _ -> assert false)
      | _ -> env)
  | Expr_def _ | Param_def _ | Other ->
      add_constr env v (if pol then Ne (B_const 0L) else Eq (B_const 0L))

(* The widening moves a bound which grows to the closest threshold beyond
   it, or to the bound of target integers *)
let rec widen st th old r =
  match old, r with
  | Range (l, h), Range (l', h') ->
      let above v =
        List.fold_left
          ~f:(fun acc c -> if Int64.(c >= v && c < acc) then c else acc)
          ~init:st.max_t
          th
      in
      let below v =
        List.fold_left
          ~f:(fun acc c -> if Int64.(c <= v && c > acc) then c else acc)
          ~init:st.min_t
          th
      in
      Range
        ( (if Int64.(l' < l) then below l' else l')
        , if Int64.(h' > h) then above h' else h' )
  | Tuple t, Tuple t' -> Tuple (map2_fields (widen st th) t t')
  | (Bot | Top | Range _ | Tuple _), _ -> r

let coarse_thresholds st = [ 0L; st.min_t; st.max_t ]

(* The thresholds used to widen the range of a parameter [x]: the bounds of
   the facts known where its arguments are computed or passed, which are the
   likely bounds of a loop counter, and the bounds of integers *)
let thresholds st x =
  let constr_bounds env y acc =
    match Var.Map.find_opt y env with
    | None -> acc
    | Some cs ->
        List.fold_left
          ~f:(fun acc c ->
            match c with
            | Within (l, h) -> l :: h :: acc
            | In_bounds _ -> acc
            | Lt b | Le b | Gt b | Ge b | Eq b | Ne b | Ult b -> (
                match bound_range st b with
                | Range (l, h) ->
                    Int64.pred l
                    :: l
                    :: Int64.succ l
                    :: Int64.pred h
                    :: h
                    :: Int64.succ h
                    :: acc
                | Bot | Top | Tuple _ -> acc))
          ~init:acc
          cs
  in
  (* The facts on an argument [y] known in [env], and on the operands of
     its definition *)
  let arg_bounds env y acc =
    let acc = constr_bounds env y acc in
    match st.defs.(Var.idx y) with
    | Expr_def (Prim (Extern (name, _), args), def_env) when is_arith_prim name ->
        List.fold_left
          ~f:(fun acc a ->
            match a with
            | Pv z -> constr_bounds def_env z (constr_bounds env z acc)
            | Pc _ -> acc)
          ~init:acc
          args
    | Expr_def _ | Param_def _ | Other -> acc
  in
  let coarse = coarse_thresholds st in
  match st.defs.(Var.idx x) with
  | Param_def l ->
      List.fold_left ~f:(fun acc (y, env) -> arg_bounds env y acc) ~init:coarse l
  | Other -> (
      match gf_def st x with
      | Phi { known; others = false; _ } ->
          Var.Set.fold (fun y acc -> arg_bounds Var.Map.empty y acc) known coarse
      | Phi { others = true; _ } | Expr _ -> coarse)
  | Expr_def _ -> coarse

module G = Dgraph.Make_Imperative (Var) (Var.ISet) (Var.Tbl)

module Solver = G.Solver (struct
  type t = range

  let equal = range_equal

  let bot = Bot
end)

(* Whether [i] is a valid index of [obj] where [x] is defined. A variable
   index is equal to [z + k] for each of its terms (see [index_terms]). It
   is valid if it has already been checked against [obj], or if it is
   nonnegative and either less than a lower bound of the length of [obj] (see
   [min_length]; the facts known about the variables holding this length
   tell more), or at most [n - 1] for a length [n] of [obj] (see
   [upper_bounds]). *)
let valid_index st ~at:x ~obj i =
  let analysed z = Var.idx z < Array.length st.defs in
  Var.idx x < Array.length st.defs
  && (match i with
    | Pv i -> analysed i
    | Pc (Int _) -> true
    | Pc _ -> false)
  && analysed obj
  &&
  match st.defs.(Var.idx x) with
  | Expr_def (_, env) when analysed (rep st obj) ->
      let accessed = obj in
      let obj = rep st obj in
      let terms =
        match i with
        | Pv i -> List.filter ~f:(fun (z, _) -> analysed z) (index_terms st env i)
        | Pc _ -> []
      in
      List.exists
        ~f:(fun (z, k) ->
          Int64.equal k 0L
          && List.exists
               ~f:(fun c ->
                 match c with
                 | In_bounds a -> Var.equal a obj
                 | _ -> false)
               (constrs env z))
        terms
      ||
      let lower, upper =
        match i with
        | Pc (Int c) -> int_constant c, int_constant c
        | Pv _ | Pc _ ->
            List.fold_left
              ~f:(fun (l, h) (z, k) ->
                match refine st env z with
                | Range (l', h') ->
                    Int64.max l (Int64.add l' k), Int64.min h (Int64.add h' k)
                | Bot | Top | Tuple _ -> l, h)
              ~init:(Int64.min_int, Int64.max_int)
              terms
      in
      let min_length =
        List.fold_left
          ~f:(fun m n ->
            (* Only the variables with facts at the access, whose definition
               then dominates it, hold the length of [obj] there *)
            if analysed n && not (List.is_empty (constrs env n))
            then
              match refine st env n with
              | Range (l, _) -> Int64.max m l
              | Bot | Top | Tuple _ -> m
            else m)
          ~init:(min_length st ~accessed obj |> Option.value ~default:0L)
          (length_vars st obj)
      in
      Int64.(lower >= 0L)
      && (Int64.(upper < min_length)
         || List.exists
              ~f:(fun (z, k) ->
                List.exists
                  ~f:(fun (v, c) ->
                    Int64.(add c k <= -1L) && analysed v && is_length st ~obj v)
                  (upper_bounds st ~nonneg:(fun w -> is_length st ~obj w) env z))
              terms)
  | Expr_def _ | Param_def _ | Other -> false

(* The variables whose range is looked at: the results of arithmetic
   operations (see [cannot_overflow]) and the accesses whose index may be
   valid (see [valid_index]) *)
let is_root st x =
  match st.defs.(Var.idx x) with
  | Expr_def (Prim (Extern (name, _), _), _) -> is_arith_prim name || is_access name
  | Expr_def _ | Param_def _ | Other -> false

let f ~global_flow_state ~global_flow_info p =
  let t = Timer.make () in
  let max_t = Targetint.to_int64 (Targetint.max_int ()) in
  let min_t = Targetint.to_int64 (Targetint.min_int ()) in
  let n = Var.count () in
  let st =
    { min_t
    ; max_t
    ; ranges = Var.Tbl.make () Bot
    ; defs = Array.make n Other
    ; def_block = Array.make n (-2)
    ; dom_intervals = Addr.Hashtbl.create 128
    ; global_flow_state
    ; global_flow_info
    ; reps = Array.make n (-1)
    ; lengths = Var.Hashtbl.create 128
    ; members = Var.Hashtbl.create 128
    }
  in
  let set_def_block x pc = st.def_block.(Var.idx x) <- pc in
  (* The definitions are recorded beforehand, since [add_cond] looks up the
     definition of a condition, which may be bound in an enclosing closure
     not processed yet. The facts known where they are evaluated are filled
     in below. *)
  Addr.Map.iter
    (fun _ block ->
      List.iter
        ~f:(fun i ->
          match i with
          | Let (x, e) -> st.defs.(Var.idx x) <- Expr_def (e, Var.Map.empty)
          | Assign _ | Set_field _ | Offset_ref _ | Array_set _ | Event _ -> ())
        block.body)
    p.blocks;
  compute_reps st p;
  let loop_params = BitSet.create' n in
  let order = ref [] in
  let param_defs = Array.make n [] in
  let edge env (pc, args) =
    let block = Addr.Map.find pc p.blocks in
    List.iter2
      ~f:(fun x y -> param_defs.(Var.idx x) <- (y, env) :: param_defs.(Var.idx x))
      block.params
      args
  in
  Code.fold_closures
    p
    (fun _ params ((pc, _) as cont) _ () ->
      let closure = pc in
      List.iter
        ~f:(fun x ->
          set_def_block x (-1);
          order := x :: !order)
        params;
      (* The arguments passed to the first block of the closure *)
      edge Var.Map.empty cont;
      (* Record the definitions of a block, and the arguments passed by its
         branch, with the facts [env] known at its entry. Returns the facts
         known at its exit. *)
      let process_block pc env =
        let block = Addr.Map.find pc p.blocks in
        List.iter
          ~f:(fun x ->
            set_def_block x pc;
            order := x :: !order)
          block.params;
        let env =
          List.fold_left
            ~f:(fun env i ->
              match i with
              | Let (x, e) -> (
                  st.defs.(Var.idx x) <- Expr_def (e, env);
                  set_def_block x pc;
                  order := x :: !order;
                  match e with
                  | Prim (Extern (name, _), Pv a :: Pv i :: _) when is_checked_access name
                    ->
                      (* The index is checked against the length of the
                         array or string *)
                      add_constr
                        (add_constr env i (Within (0L, Int64.pred (max_length st))))
                        i
                        (In_bounds (rep st a))
                  | _ -> env)
              | Assign (x, y) ->
                  (* [y] is a possible value of the parameter [x], as if
                     passed by a branch: [x] is only read once the
                     exception handler is entered, and it is not assigned
                     anymore then (see [Code.invariant]) *)
                  param_defs.(Var.idx x) <- (y, env) :: param_defs.(Var.idx x);
                  env
              | Set_field _ | Offset_ref _ | Array_set _ | Event _ -> env)
            ~init:env
            block.body
        in
        (match block.branch with
        | Branch cont | Poptrap cont -> edge env cont
        | Cond (v, cont1, cont2) ->
            edge (add_cond st env v true) cont1;
            edge (add_cond st env v false) cont2
        | Switch (_, a) -> Array.iter ~f:(fun cont -> edge env cont) a
        | Pushtrap (cont1, x, ((pc2, _) as cont2)) ->
            set_def_block x pc2;
            order := x :: !order;
            edge env cont1;
            edge env cont2
        | Return _ | Raise _ | Stop -> ());
        block, env
      in
      let g = Structure.control_flow_graph p.blocks pc in
      let dom = Structure.dominator_tree g in
      Addr.Set.iter
        (fun pc ->
          if Structure.is_loop_header g pc
          then
            List.iter
              ~f:(fun x -> BitSet.set loop_params (Var.idx x))
              (Addr.Map.find pc p.blocks).params)
        (Structure.get_nodes g);
      let preds = Addr.Hashtbl.create 16 in
      Addr.Set.iter
        (fun pc' ->
          Code.fold_children
            p.blocks
            pc'
            (fun pc'' () ->
              Addr.Hashtbl.replace
                preds
                pc''
                (1 + (Addr.Hashtbl.find_opt preds pc'' |> Option.value ~default:0)))
            ())
        (Structure.get_nodes g);
      let counter = ref 0 in
      let rec walk pc env =
        let pre = !counter in
        incr counter;
        let block, env = process_block pc env in
        Addr.Set.iter
          (fun pc' ->
            let env =
              match block.branch with
              | Cond (v, (pc1, _), (pc2, _))
                when pc1 <> pc2 && Addr.Hashtbl.find preds pc' = 1 ->
                  if pc' = pc1
                  then add_cond st env v true
                  else if pc' = pc2
                  then add_cond st env v false
                  else env
              | _ -> env
            in
            walk pc' env)
          (Structure.get_edges dom pc);
        Addr.Hashtbl.replace st.dom_intervals pc (closure, pre, !counter)
      in
      walk pc Var.Map.empty)
    ();
  Array.iteri
    ~f:(fun i l -> if not (List.is_empty l) then st.defs.(i) <- Param_def l)
    param_defs;
  let order = List.rev !order in
  (* We only compute the ranges of the roots and of the variables they
     depend on. [deps] records the reverse dependencies between these
     variables. *)
  let needed = BitSet.create' n in
  let stack = ref [] in
  let push x =
    if not (BitSet.mem needed (Var.idx x))
    then (
      BitSet.set needed (Var.idx x);
      stack := x :: !stack)
  in
  List.iter ~f:(fun x -> if is_root st x then push x) order;
  let deps = Array.make n [] in
  let rec visit () =
    match !stack with
    | [] -> ()
    | x :: rem ->
        stack := rem;
        List.iter
          ~f:(fun y ->
            deps.(Var.idx y) <- x :: deps.(Var.idx y);
            push y)
          (dependencies st x);
        visit ()
  in
  visit ();
  let order = List.filter ~f:(fun x -> BitSet.mem needed (Var.idx x)) order in
  (* Every cycle of dependencies goes through a loop header, a function
     parameter, or a value read from the heap or returned by a function *)
  let widened x =
    match st.defs.(Var.idx x) with
    | Expr_def
        ( ( Field _ | Apply _
          | Prim ((Array_get _ | Extern ("caml_array_unsafe_get", _)), _) )
        , _ ) -> true
    | Expr_def _ -> false
    | Param_def _ -> BitSet.mem loop_params (Var.idx x)
    | Other -> true
  in
  let domain = Var.ISet.empty () in
  List.iter ~f:(fun x -> Var.ISet.add domain x) order;
  let g = { G.domain; iter_children = (fun f x -> List.iter ~f deps.(Var.idx x)) } in
  (* We only widen after a range has grown twice, which is enough for
     short loops. After many steps, we only use coarse thresholds, to
     ensure termination: the bounds of the facts may grow as well. *)
  let updates = Array.make n 0 in
  (* The variables whose range was widened beyond the join of its values *)
  let widened_vars = BitSet.create' n in
  let ranges =
    Solver.f () g (fun ranges x ->
        let st = { st with ranges } in
        let old = get st x in
        let r = join old (compute st x) in
        let i = Var.idx x in
        let r =
          if widened x && updates.(i) >= 2
          then (
            let r' =
              widen
                st
                (if updates.(i) < 20 then thresholds st x else coarse_thresholds st)
                old
                r
            in
            if not (range_equal r r') then BitSet.set widened_vars i;
            r')
          else r
        in
        if not (range_equal old r) then updates.(i) <- updates.(i) + 1;
        r)
  in
  let st = { st with ranges } in
  (* Narrowing, starting from the variables whose range was widened, and
     propagating the changes. From a post-fixpoint, refining the range of a
     variable with the value it is computed from preserves the soundness of
     the result, whatever the order. Each variable is visited at most twice,
     which ensures termination. *)
  let visits = Array.make n 0 in
  let queue = Queue.create () in
  let queued = BitSet.create' n in
  let push x =
    let i = Var.idx x in
    if visits.(i) < 2 && not (BitSet.mem queued i)
    then (
      BitSet.set queued i;
      Queue.push x queue)
  in
  List.iter ~f:(fun x -> if BitSet.mem widened_vars (Var.idx x) then push x) order;
  while not (Queue.is_empty queue) do
    let x = Queue.pop queue in
    let i = Var.idx x in
    BitSet.unset queued i;
    visits.(i) <- visits.(i) + 1;
    let old = get st x in
    let r = meet old (compute st x) in
    if not (range_equal old r)
    then (
      Var.Tbl.set st.ranges x r;
      List.iter ~f:push deps.(i))
  done;
  if times () then Format.eprintf "  integer range analysis: %a@." Timer.print t;
  if debug ()
  then
    List.iter
      ~f:(fun x ->
        match get st x with
        | (Range _ | Tuple _) as r -> Format.eprintf "%a: %a@." Var.print x print_range r
        | Bot | Top -> ())
      order;
  st
