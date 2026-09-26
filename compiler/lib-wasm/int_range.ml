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
   variables whose value always fits in 31 bits. With portable integers,
   these values can be represented as [i32] rather than [i64].

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
  ; global_flow_state : Global_flow.state
  ; global_flow_info : Global_flow.info
  }

let max_i31s = lazy (Targetint.to_int64 (Lazy.force Targetint.max_i31s))

let min_i31s = lazy (Targetint.to_int64 (Lazy.force Targetint.min_i31s))

let imin a b = if Int64.(a <= b) then a else b

let imax a b = if Int64.(a >= b) then a else b

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
  | Range (l, h), Range (l', h') -> Range (imin l l', imax h h')
  | Tuple t, Tuple t' -> Tuple (map2_fields join t t')
  | (Top | Range _ | Tuple _), _ -> Top

(* Only used to refine a range which [r'] is known to over-approximate, or
   the other way around: when the two are not comparable, we keep [r'] *)
let rec meet r r' =
  match r, r' with
  | Bot, _ | _, Bot -> Bot
  | Top, r | r, Top -> r
  | Range (l, h), Range (l', h') ->
      let l = imax l l' in
      let h = imin h h' in
      if Int64.(l > h) then Bot else Range (l, h)
  | Tuple t, Tuple t' when Array.length t = Array.length t' ->
      Tuple (Array.map2 ~f:meet t t')
  | (Range _ | Tuple _), _ -> r'

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
      let upper b f =
        match bound_range st b with
        | Bot -> Bot
        | Top | Tuple _ -> r
        | Range (l, h) -> meet r (Range (st.min_t, f l h))
      in
      let lower b f =
        match bound_range st b with
        | Bot -> Bot
        | Top | Tuple _ -> r
        | Range (l, h) -> meet r (Range (f l h, st.max_t))
      in
      match c with
      | Within (l, h) -> meet r (Range (l, h))
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
      let c = Targetint.to_int64 c in
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
    let width = if Config.Flag.portable_int () then 63 else 31 in
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
          (List.fold_left ~f:imin ~init:Int64.max_int l)
          (List.fold_left ~f:imax ~init:Int64.min_int l)
    | ("%int_div" | "%direct_int_div"), [ Range (a, b); Range (c, d) ]
      when Int64.(c = d && c > 0L) -> Range (Int64.div a c, Int64.div b c)
    | ("%int_div" | "%direct_int_div"), [ Range (a, b); (Range _ | Top) ] ->
        (* The quotient is not larger than the dividend in absolute
           value, except for [min_int / -1] which overflows *)
        let m = imax (Int64.abs a) (Int64.abs b) in
        norm st (Int64.neg m) m
    | ("%int_mod" | "%direct_int_mod"), [ Range (a, b); d ] ->
        (* The remainder has the sign of the dividend, and is smaller
           than the divisor in absolute value *)
        let m =
          match d with
          | Range (c, d) -> imax 0L (Int64.pred (imax (Int64.abs c) (Int64.abs d)))
          | Top | Bot | Tuple _ -> st.max_t
        in
        Range
          ( (if Int64.(a >= 0L) then 0L else imax a (Int64.neg m))
          , if Int64.(b <= 0L) then 0L else imin b m )
    | "%int_and", [ r; r' ] -> (
        let non_negative r =
          match r with
          | Range (l, h) when Int64.(l >= 0L) -> Some h
          | Range _ | Top | Bot | Tuple _ -> None
        in
        match non_negative r, non_negative r' with
        | Some h, Some h' -> Range (0L, imin h h')
        | Some h, None | None, Some h -> Range (0L, h)
        | None, None -> Top)
    | ("%int_or" | "%int_xor"), [ Range (a, b); Range (c, d) ]
      when Int64.(a >= 0L && c >= 0L) -> Range (0L, Int64.pred (pow2_above (imax b d)))
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
    | "caml_ml_string_length" | "caml_ml_bytes_length" -> Range (0L, Lazy.force max_i31s)
    | ("caml_ba_dim_1" | "caml_ba_dim_2" | "caml_ba_dim_3")
      when Config.Flag.portable_int () -> Range (0L, Int64.of_int32 Int32.max_int)
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
      let c = Targetint.to_int64 c in
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
  | Prim ((Not | IsInt _ | Eq | Neq | Lt | Le | Ult), _) -> Range (0L, 1L)
  | Prim (Vectlength _, _) -> Range (0L, Lazy.force max_i31s)
  | Prim
      ((Wasm_untag_int | Wasm_tag_int | Wasm_untag_large_int | Wasm_tag_large_int), [ a ])
    -> arg_range st env a
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

(* The step of the argument [y] of a loop counter [x], if [y] is [x + 1] or
   [x - 1] *)
let counter_step st x y =
  match st.defs.(Var.idx y) with
  | Expr_def
      (Prim (Extern ("%int_add", _), ([ Pv x'; Pc (Int c) ] | [ Pc (Int c); Pv x' ])), _)
    when Var.equal x x' -> (
      match Targetint.to_int64 c with
      | 1L -> Some `Up
      | -1L -> Some `Down
      | _ -> None)
  | Expr_def (Prim (Extern ("%int_sub", _), [ Pv x'; Pc (Int c) ]), _)
    when Var.equal x x' && Int64.(Targetint.to_int64 c = 1L) -> Some `Down
  | Expr_def _ | Param_def _ | Other -> None

(* A loop counter [x] which is incremented while [x <> hi], and whose initial
   values are at most [hi], remains at most [hi]; this is the case of the
   counter of a [for] loop, which the interval domain cannot capture when
   [hi] is a variable. Similarly for a counter which is decremented. We
   return the bound [hi] and the direction of the loop, if applicable. *)
let counter_bound st x l =
  let increments, others =
    List.partition ~f:(fun (y, _) -> Option.is_some (counter_step st x y)) l
  in
  let ne_bounds env =
    List.filter_map
      ~f:(fun c ->
        match c with
        | Ne (B_var hi) -> Some hi
        | Ne (B_const _) | Lt _ | Le _ | Gt _ | Ge _ | Eq _ | Ult _ | Within _ -> None)
      (constrs env x)
  in
  match increments with
  | [] -> None
  | (y, env) :: _ ->
      let dir = Option.get (counter_step st x y) in
      List.find_map
        ~f:(fun hi ->
          let increment_ok (y, env) =
            Poly.equal (counter_step st x y) (Some dir)
            && List.exists ~f:(fun hi' -> Var.equal hi hi') (ne_bounds env)
          in
          (* The initial value [y] is on the right side of [hi] *)
          let initial_ok (y, env) =
            List.exists
              ~f:(fun c ->
                match dir, c with
                | `Up, (Le (B_var hi') | Lt (B_var hi'))
                | `Down, (Ge (B_var hi') | Gt (B_var hi')) -> Var.equal hi hi'
                | _ -> false)
              (constrs env y)
            ||
            match dir, refine st env y, refine st env hi with
            | `Up, Range (_, h), Range (l, _) -> Int64.(h <= l)
            | `Down, Range (l, _), Range (_, h) -> Int64.(l >= h)
            | _ -> false
          in
          if List.for_all ~f:increment_ok increments && List.for_all ~f:initial_ok others
          then Some (hi, dir)
          else None)
        (ne_bounds env)

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
      | Some (hi, dir) -> (
          match dir, get st hi with
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

(* The variables [x] depends on *)
let dependencies st x =
  let bound_vars env y acc =
    match Var.Map.find_opt y env with
    | None -> acc
    | Some l ->
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
            | Within _ -> acc)
          ~init:acc
          l
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
      let bounds =
        List.concat_map
          ~f:(fun (_, env) ->
            List.filter_map
              ~f:(fun c ->
                match c with
                | Ne (B_var hi) -> Some hi
                | _ -> None)
              (Var.Map.find_opt x env |> Option.value ~default:[]))
          l
      in
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

let add_constr env y c =
  Var.Map.add y (c :: (Var.Map.find_opt y env |> Option.value ~default:[])) env

let bound_of_arg a =
  match a with
  | Pv z -> Some (B_var z)
  | Pc (Int c) -> Some (B_const (Targetint.to_int64 c))
  | Pc _ -> None

(* The facts known when [v] is true ([pol]) or false *)
let rec add_cond st env v pol =
  match gf_def st v with
  | Expr (Prim (Not, [ Pv w ])) -> add_cond st env w (not pol)
  | Expr (Prim (((Lt | Le | Eq | Neq | Ult) as op), [ a; b ])) -> (
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
          | Eq, true | Neq, false -> env |> on a (Eq bb) |> on b (Eq ba)
          | Eq, false | Neq, true -> env |> on a (Ne bb) |> on b (Ne ba)
          | Ult, true -> env |> on a (Ult bb)
          | Ult, false -> env
          | _ -> assert false)
      | _ -> env)
  | Expr _ | Phi _ -> add_constr env v (if pol then Ne (B_const 0L) else Eq (B_const 0L))

(* The widening moves a bound which grows to the closest threshold beyond
   it, or to the bound of target integers *)
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

let coarse_thresholds = lazy [ 0L; Lazy.force min_i31s; Lazy.force max_i31s ]

(* The thresholds used to widen the range of a parameter [x]: the bounds of
   the facts known where its arguments are computed or passed, which are the
   likely bounds of a loop counter, and the bounds of 31-bit integers *)
let thresholds st x =
  let constr_bounds env y acc =
    match Var.Map.find_opt y env with
    | None -> acc
    | Some cs ->
        List.fold_left
          ~f:(fun acc c ->
            match c with
            | Within (l, h) -> l :: h :: acc
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
  let coarse = Lazy.force coarse_thresholds in
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

let f ~global_flow_state ~global_flow_info p =
  let t = Timer.make () in
  let max_t =
    if Config.Flag.portable_int ()
    then Int64.(sub (shift_left 1L 62) 1L)
    else Int64.(sub (shift_left 1L 30) 1L)
  in
  let min_t = Int64.(sub (neg max_t) 1L) in
  let n = Var.count () in
  let st =
    { min_t
    ; max_t
    ; ranges = Var.Tbl.make () Bot
    ; defs = Array.make n Other
    ; global_flow_state
    ; global_flow_info
    }
  in
  let assigned = BitSet.create' n in
  Addr.Map.iter
    (fun _ block ->
      List.iter
        ~f:(fun i ->
          match i with
          | Assign (x, _) -> BitSet.set assigned (Var.idx x)
          | _ -> ())
        block.body)
    p.blocks;
  let loop_params = BitSet.create' n in
  let order = ref [] in
  let param_defs = Array.make n [] in
  let edge env (pc, args) =
    let block = Addr.Map.find pc p.blocks in
    List.iter2
      ~f:(fun x y ->
        if not (BitSet.mem assigned (Var.idx x))
        then param_defs.(Var.idx x) <- (y, env) :: param_defs.(Var.idx x))
      block.params
      args
  in
  Code.fold_closures
    p
    (fun _ params ((pc, _) as cont) _ () ->
      List.iter ~f:(fun x -> order := x :: !order) params;
      (* The arguments passed to the first block of the closure *)
      edge Var.Map.empty cont;
      (* Record the definitions of a block, and the arguments passed by its
         branch, with the facts [env] known at its entry. Returns the facts
         known at its exit. *)
      let process_block pc env =
        let block = Addr.Map.find pc p.blocks in
        List.iter ~f:(fun x -> order := x :: !order) block.params;
        let env =
          List.fold_left
            ~f:(fun env i ->
              match i with
              | Let (x, e) -> (
                  st.defs.(Var.idx x) <- Expr_def (e, env);
                  order := x :: !order;
                  match e with
                  | Prim (Extern (name, _), Pv _ :: Pv i :: _) when is_checked_access name
                    ->
                      (* The index is checked against the length of the
                         array or string, which fits in 31 bits *)
                      add_constr env i (Within (0L, Int64.pred (Lazy.force max_i31s)))
                  | _ -> env)
              | Assign _ | Set_field _ | Offset_ref _ | Array_set _ | Event _ -> env)
            ~init:env
            block.body
        in
        (match block.branch with
        | Branch cont | Poptrap cont -> edge env cont
        | Cond (v, cont1, cont2) ->
            edge (add_cond st env v true) cont1;
            edge (add_cond st env v false) cont2
        | Switch (_, a) -> Array.iter ~f:(fun cont -> edge env cont) a
        | Pushtrap (cont1, x, cont2) ->
            order := x :: !order;
            edge env cont1;
            edge env cont2
        | Return _ | Raise _ | Stop -> ());
        block, env
      in
      let g = Structure.build_graph p.blocks pc in
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
      let rec walk pc env =
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
          (Structure.get_edges dom pc)
      in
      walk pc Var.Map.empty)
    ();
  Array.iteri
    ~f:(fun i l -> if not (List.is_empty l) then st.defs.(i) <- Param_def l)
    param_defs;
  let order = List.rev !order in
  (* Dependencies *)
  let deps = Array.make n [] in
  List.iter
    ~f:(fun x ->
      List.iter
        ~f:(fun y -> deps.(Var.idx y) <- x :: deps.(Var.idx y))
        (dependencies st x))
    order;
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
                (if updates.(i) < 20
                 then thresholds st x
                 else Lazy.force coarse_thresholds)
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

let fits_in_i31 st x =
  let idx = Var.idx x in
  idx < Var.Tbl.length st.ranges
  &&
  match Var.Tbl.get st.ranges x with
  | Range (l, h) -> Int64.(l >= Lazy.force min_i31s && h <= Lazy.force max_i31s)
  | Bot | Top | Tuple _ -> false
