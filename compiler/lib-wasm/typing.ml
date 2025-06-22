open! Stdlib
open Code
open Global_flow

let debug = Debug.find "typing"

let times = Debug.find "times"

let can_unbox_parameters fun_info f =
  (* We can unbox the parameters of a function when all its call sites
     are known, and only this function is called there. It would be
     more robust to deal with more cases by using an intermediate
     function that unbox the parameters. When several functions can be
     call from the same call site, one could enforce somehow that they
     have the same signature. *)
  Call_graph_analysis.direct_calls_only fun_info f

let can_unbox_return_value fun_info f =
  (* Unboxing return values can unoptimize a tail call. Since we are
     never unboxing then reboxing a value, this can only happen once
     in a sequence of tail calls, so this is not an issue. *)
  Call_graph_analysis.direct_calls_only fun_info f

module Integer = struct
  type kind =
    | Ref
    | Normalized
    | Unnormalized

  let join r r' =
    match r, r' with
    | Unnormalized, _ | _, Unnormalized -> Unnormalized
    | Ref, Ref -> Ref
    | _ -> Normalized
end

type boxed_number =
  | Int32
  | Int64
  | Nativeint
  | Float

type boxed_status =
  | Boxed
  | Unboxed

type typ =
  | Top
  | Int of Integer.kind
  | Number of boxed_number * boxed_status
  | Tuple of typ array
      (** This value is a block or an integer; if it's an integer, an
          overapproximation of the possible values of each of its
          fields is given by the array of types *)
  | Bot

module Domain = struct
  type t = typ

  let rec join t t' =
    match t, t' with
    | Bot, t | t, Bot -> t
    | Int r, Int r' -> Int (Integer.join r r')
    | Number (n, b), Number (n', b') ->
        if Poly.equal n n'
        then
          Number
            ( n
            , match b, b' with
              | Unboxed, _ | _, Unboxed -> Unboxed
              | Boxed, Boxed -> Boxed )
        else Top
    | Tuple t, Tuple t' ->
        let l = Array.length t in
        let l' = Array.length t' in
        Tuple
          (if l = l'
           then Array.map2 ~f:join t t'
           else
             Array.init (max l l') ~f:(fun i ->
                 if i < l then if i < l' then join t.(i) t'.(i) else t.(i) else t'.(i)))
    | Int _, Tuple _ -> t'
    | Tuple _, Int _ -> t
    | Top, _ | _, Top -> Top
    | (Int _ | Number _ | Tuple _), _ -> Top

  let join_set ?(others = false) f s =
    if others then Top else Var.Set.fold (fun x a -> join (f x) a) s Bot

  let rec equal t t' =
    match t, t' with
    | Top, Top | Bot, Bot -> true
    | Int t, Int t' -> Poly.equal t t'
    | Number (t, b), Number (t', b') -> Poly.equal t t' && Poly.equal b b'
    | Tuple t, Tuple t' ->
        Array.length t = Array.length t' && Array.for_all2 ~f:equal t t'
    | (Top | Tuple _ | Int _ | Number _ | Bot), _ -> false

  let bot = Bot

  let depth_treshold = 4

  let rec depth t =
    match t with
    | Top | Bot | Number _ | Int _ -> 0
    | Tuple l -> 1 + Array.fold_left ~f:(fun acc t' -> max (depth t') acc) l ~init:0

  let rec truncate depth t =
    match t with
    | Top | Bot | Number _ | Int _ -> t
    | Tuple l ->
        if depth = 0
        then Top
        else Tuple (Array.map ~f:(fun t' -> truncate (depth - 1) t') l)

  let limit t = if depth t > depth_treshold then truncate depth_treshold t else t

  let box t =
    match t with
    | Int _ -> Int Ref
    | Number (n, _) -> Number (n, Boxed)
    | _ -> t

  let rec print f t =
    match t with
    | Top -> Format.fprintf f "top"
    | Bot -> Format.fprintf f "bot"
    | Int k ->
        Format.fprintf
          f
          "int{%s}"
          (match k with
          | Ref -> "ref"
          | Normalized -> "normalized"
          | Unnormalized -> "unnormalized")
    | Number (n, b) ->
        Format.fprintf
          f
          "%s{%s}"
          (match n with
          | Int32 -> "int32"
          | Int64 -> "int64"
          | Nativeint -> "nativeint"
          | Float -> "float")
          (match b with
          | Boxed -> "boxed"
          | Unboxed -> "unboxed")
    | Tuple t ->
        Format.fprintf
          f
          "(%a)"
          (Format.pp_print_list ~pp_sep:(fun f () -> Format.fprintf f ",") print)
          (Array.to_list t)
end

let update_deps st { blocks; _ } =
  let add_dep st x y = Var.Tbl.set st.deps y (x :: Var.Tbl.get st.deps y) in
  Addr.Map.iter
    (fun _ block ->
      List.iter block.body ~f:(fun i ->
          match i with
          | Let (x, Block (_, lst, _, _)) -> Array.iter ~f:(fun y -> add_dep st x y) lst
          | Let (x, Prim (Extern ("%int_and" | "%int_or" | "%int_xor"), lst)) ->
              (* The return type of these primitives depend on the input type *)
              List.iter
                ~f:(fun p ->
                  match p with
                  | Pc _ -> ()
                  | Pv y -> add_dep st x y)
                lst
          | _ -> ()))
    blocks

let mark_boxed_function_parameters ~fun_info { blocks; _ } =
  let boxed_function_parameters = Var.ISet.empty () in
  let set x = Var.ISet.add boxed_function_parameters x in
  Addr.Map.iter
    (fun _ block ->
      List.iter block.body ~f:(fun i ->
          match i with
          | Let (x, Closure (params, _, _)) when not (can_unbox_parameters fun_info x) ->
              List.iter ~f:set params
          | _ -> ()))
    blocks;
  boxed_function_parameters

type st =
  { global_flow_state : Global_flow.state
  ; global_flow_info : Global_flow.info
  ; boxed_function_parameters : Var.ISet.t
  ; fun_info : Call_graph_analysis.t
  }

let rec constant_type (c : constant) =
  match c with
  | Int _ -> Int Normalized
  | Int32 _ -> Number (Int32, Unboxed)
  | Int64 _ -> Number (Int64, Unboxed)
  | NativeInt _ -> Number (Nativeint, Unboxed)
  | Float _ -> Number (Float, Unboxed)
  | Tuple (_, a, _) -> Tuple (Array.map ~f:(fun c' -> Domain.box (constant_type c')) a)
  | _ -> Top

let arg_type ~approx arg =
  match arg with
  | Pc c -> constant_type c
  | Pv x -> Var.Tbl.get approx x

let prim_type ~approx prim args =
  match prim with
  | "%int_add" | "%int_sub" | "%int_mul" | "%direct_int_mul" | "%int_lsl" | "%int_neg" ->
      Int Unnormalized
  | "%int_and" -> (
      match List.map ~f:(fun x -> arg_type ~approx x) args with
      | [ (Bot | Int (Ref | Normalized)); _ ] | [ _; (Bot | Int (Ref | Normalized)) ] ->
          Int Normalized
      | _ -> Int Unnormalized)
  | "%int_or" | "%int_xor" -> (
      match List.map ~f:(fun x -> arg_type ~approx x) args with
      | [ (Bot | Int (Ref | Normalized)); (Bot | Int (Ref | Normalized)) ] ->
          Int Normalized
      | _ -> Int Unnormalized)
  | "%int_lsr"
  | "%int_asr"
  | "%int_div"
  | "%int_mod"
  | "%direct_int_div"
  | "%direct_int_mod" -> Int Normalized
  | "caml_greaterthan"
  | "caml_greaterequal"
  | "caml_lessthan"
  | "caml_lessequal"
  | "caml_equal"
  | "caml_notequal"
  | "caml_compare" -> Int Normalized
  | "caml_int32_bswap" -> Number (Int32, Unboxed)
  | "caml_nativeint_bswap" -> Number (Nativeint, Unboxed)
  | "caml_int64_bswap" -> Number (Int64, Unboxed)
  | "caml_int32_compare" | "caml_nativeint_compare" | "caml_int64_compare" ->
      Int Normalized
  | "caml_string_get16" -> Int Normalized
  | "caml_string_get32" -> Number (Int32, Unboxed)
  | "caml_string_get64" -> Number (Int64, Unboxed)
  | "caml_bytes_get16" -> Int Normalized
  | "caml_bytes_get32" -> Number (Int32, Unboxed)
  | "caml_bytes_get64" -> Number (Int64, Unboxed)
  | "caml_lxm_next" -> Number (Int64, Unboxed)
  | "caml_ba_uint8_get16" -> Int Normalized
  | "caml_ba_uint8_get32" -> Number (Int32, Unboxed)
  | "caml_ba_uint8_get64" -> Number (Int64, Unboxed)
  | "caml_nextafter_float" -> Number (Float, Unboxed)
  | "caml_classify_float" -> Int Ref
  | "caml_ldexp_float" | "caml_erf_float" | "caml_erfc_float" -> Number (Float, Unboxed)
  | "caml_float_compare" -> Int Normalized
  | "caml_floatarray_unsafe_get" -> Number (Float, Unboxed)
  | "caml_bytes_unsafe_get"
  | "caml_string_unsafe_get"
  | "caml_bytes_get"
  | "caml_string_get"
  | "caml_ml_string_length"
  | "caml_ml_bytes_length" -> Int Normalized
  | "%direct_obj_tag" -> Int Ref
  | "caml_add_float"
  | "caml_sub_float"
  | "caml_mul_float"
  | "caml_div_float"
  | "caml_copysign_float" -> Number (Float, Unboxed)
  | "caml_signbit_float" -> Int Normalized
  | "caml_neg_float"
  | "caml_abs_float"
  | "caml_ceil_float"
  | "caml_floor_float"
  | "caml_trunc_float"
  | "caml_round_float"
  | "caml_sqrt_float" -> Number (Float, Unboxed)
  | "caml_eq_float"
  | "caml_neq_float"
  | "caml_ge_float"
  | "caml_le_float"
  | "caml_gt_float"
  | "caml_lt_float" -> Int Normalized
  | "caml_int_of_float" -> Int Unnormalized
  | "caml_float_of_int"
  | "caml_cos_float"
  | "caml_sin_float"
  | "caml_tan_float"
  | "caml_acos_float"
  | "caml_asin_float"
  | "caml_atan_float"
  | "caml_atan2_float"
  | "caml_cosh_float"
  | "caml_sinh_float"
  | "caml_tanh_float"
  | "caml_acosh_float"
  | "caml_asinh_float"
  | "caml_atanh_float"
  | "caml_cbrt_float"
  | "caml_exp_float"
  | "caml_exp2_float"
  | "caml_log_float"
  | "caml_expm1_float"
  | "caml_log1p_float"
  | "caml_log2_float"
  | "caml_log10_float"
  | "caml_power_float"
  | "caml_hypot_float"
  | "caml_fmod_float" -> Number (Float, Unboxed)
  | "caml_int32_bits_of_float" -> Number (Int32, Unboxed)
  | "caml_int32_float_of_bits" -> Number (Float, Unboxed)
  | "caml_int32_of_float" -> Number (Int32, Unboxed)
  | "caml_int32_to_float" -> Number (Float, Unboxed)
  | "caml_int32_neg"
  | "caml_int32_add"
  | "caml_int32_sub"
  | "caml_int32_mul"
  | "caml_int32_and"
  | "caml_int32_or"
  | "caml_int32_xor"
  | "caml_int32_div"
  | "caml_int32_mod"
  | "caml_int32_shift_left"
  | "caml_int32_shift_right"
  | "caml_int32_shift_right_unsigned" -> Number (Int32, Unboxed)
  | "caml_int32_to_int" -> Int Unnormalized
  | "caml_int32_of_int" -> Number (Int32, Unboxed)
  | "caml_nativeint_of_int32" -> Number (Nativeint, Unboxed)
  | "caml_nativeint_to_int32" -> Number (Int32, Unboxed)
  | "caml_int64_bits_of_float" -> Number (Int64, Unboxed)
  | "caml_int64_float_of_bits" -> Number (Float, Unboxed)
  | "caml_int64_of_float" -> Number (Int64, Unboxed)
  | "caml_int64_to_float" -> Number (Float, Unboxed)
  | "caml_int64_neg"
  | "caml_int64_add"
  | "caml_int64_sub"
  | "caml_int64_mul"
  | "caml_int64_and"
  | "caml_int64_or"
  | "caml_int64_xor"
  | "caml_int64_div"
  | "caml_int64_mod"
  | "caml_int64_shift_left"
  | "caml_int64_shift_right"
  | "caml_int64_shift_right_unsigned" -> Number (Int64, Unboxed)
  | "caml_int64_to_int" -> Int Unnormalized
  | "caml_int64_of_int" -> Number (Int64, Unboxed)
  | "caml_int64_to_int32" -> Number (Int32, Unboxed)
  | "caml_int64_of_int32" -> Number (Int64, Unboxed)
  | "caml_int64_to_nativeint" -> Number (Nativeint, Unboxed)
  | "caml_int64_of_nativeint" -> Number (Int64, Unboxed)
  | "caml_nativeint_bits_of_float" -> Number (Nativeint, Unboxed)
  | "caml_nativeint_float_of_bits" -> Number (Float, Unboxed)
  | "caml_nativeint_of_float" -> Number (Nativeint, Unboxed)
  | "caml_nativeint_to_float" -> Number (Float, Unboxed)
  | "caml_nativeint_neg"
  | "caml_nativeint_add"
  | "caml_nativeint_sub"
  | "caml_nativeint_mul"
  | "caml_nativeint_and"
  | "caml_nativeint_or"
  | "caml_nativeint_xor"
  | "caml_nativeint_div"
  | "caml_nativeint_mod"
  | "caml_nativeint_shift_left"
  | "caml_nativeint_shift_right"
  | "caml_nativeint_shift_right_unsigned" -> Number (Nativeint, Unboxed)
  | "caml_nativeint_to_int" -> Int Unnormalized
  | "caml_nativeint_of_int" -> Number (Nativeint, Unboxed)
  | "caml_int_compare" -> Int Normalized
  | _ -> Top

let propagate st approx x : Domain.t =
  match st.global_flow_state.defs.(Var.idx x) with
  | Phi { known; others; unit } ->
      let res = Domain.join_set ~others (fun y -> Var.Tbl.get approx y) known in
      let res = if unit then Domain.join (Int Unnormalized) res else res in
      if Var.ISet.mem st.boxed_function_parameters x then Domain.box res else res
  | Expr e -> (
      match e with
      | Constant c -> constant_type c
      | Closure _ -> Top
      | Block (_, lst, _, _) ->
          Tuple
            (Array.mapi
               ~f:(fun i y ->
                 match st.global_flow_state.mutable_fields.(Var.idx x) with
                 | All_fields -> Top
                 | Some_fields s when IntSet.mem i s -> Top
                 | Some_fields _ | No_field ->
                     Domain.limit (Domain.box (Var.Tbl.get approx y)))
               lst)
      | Field (_, _, Float) -> Number (Float, Unboxed)
      | Field (y, n, Non_float) -> (
          match Var.Tbl.get approx y with
          | Tuple t -> if n < Array.length t then t.(n) else Bot
          | Top -> Top
          | _ -> Bot)
      | Prim
          ( Extern ("caml_check_bound" | "caml_check_bound_float" | "caml_check_bound_gen")
          , [ Pv y; _ ] ) -> Var.Tbl.get approx y
      | Prim ((Array_get | Extern "caml_array_unsafe_get"), [ Pv y; _ ]) -> (
          match Var.Tbl.get st.global_flow_info.info_approximation y with
          | Values { known; others } ->
              Domain.join_set
                ~others
                (fun z ->
                  match st.global_flow_state.defs.(Var.idx z) with
                  | Expr (Block (_, lst, _, _)) ->
                      let m =
                        match st.global_flow_state.mutable_fields.(Var.idx z) with
                        | No_field -> false
                        | Some_fields _ | All_fields -> true
                      in
                      if m
                      then Top
                      else
                        Domain.box
                          (Array.fold_left
                             ~f:(fun acc t -> Domain.join (Var.Tbl.get approx t) acc)
                             ~init:Domain.bot
                             lst)
                  | Expr (Closure _) -> Bot
                  | Phi _ | Expr _ -> assert false)
                known
          | Top -> Top)
      | Prim (Array_get, _) -> Top
      | Prim ((Vectlength | Not | IsInt | Eq | Neq | Lt | Le | Ult), _) -> Int Normalized
      | Prim (Extern prim, args) -> prim_type ~approx prim args
      | Special _ -> Top
      | Apply { f; args; _ } -> (
          match Var.Tbl.get st.global_flow_info.info_approximation f with
          | Values { known; others } ->
              Domain.join_set
                ~others
                (fun g ->
                  match st.global_flow_state.defs.(Var.idx g) with
                  | Expr (Closure (params, _, _))
                    when List.length args = List.length params ->
                      let res =
                        Domain.join_set
                          (fun y -> Var.Tbl.get approx y)
                          (Var.Map.find g st.global_flow_state.return_values)
                      in
                      if can_unbox_return_value st.fun_info g then res else Domain.box res
                  | Expr (Closure (_, _, _)) ->
                      (* The function is partially applied or over applied *)
                      Top
                  | Expr (Block _) -> Bot
                  | Phi _ | Expr _ -> assert false)
                known
          | Top -> Top))

module G = Dgraph.Make_Imperative (Var) (Var.ISet) (Var.Tbl)
module Solver = G.Solver (Domain)

let solver st =
  let associated_list h x = try Var.Hashtbl.find h x with Not_found -> [] in
  let g =
    { G.domain = st.global_flow_state.vars
    ; G.iter_children =
        (fun f x ->
          List.iter ~f (Var.Tbl.get st.global_flow_state.deps x);
          List.iter
            ~f:(fun g ->
              List.iter ~f (associated_list st.global_flow_state.function_call_sites g))
            (associated_list st.global_flow_state.functions_from_returned_value x))
    }
  in
  Solver.f () g (propagate st)

(* These are primitives which are handled internally by the compiler,
   plus the specialized primitives listed in Generate. *)
let primitives_with_unboxed_parameters =
  let h = String.Hashtbl.create 256 in
  List.iter
    ~f:(fun s -> String.Hashtbl.add h s ())
    [ "caml_int32_bswap"
    ; "caml_nativeint_bswap"
    ; "caml_int64_bswap"
    ; "caml_int32_compare"
    ; "caml_nativeint_compare"
    ; "caml_int64_compare"
    ; "caml_nextafter_float"
    ; "caml_classify_float"
    ; "caml_ldexp_float"
    ; "caml_erf_float"
    ; "caml_erfc_float"
    ; "caml_float_compare"
    ; "caml_add_float"
    ; "caml_sub_float"
    ; "caml_mul_float"
    ; "caml_div_float"
    ; "caml_copysign_float"
    ; "caml_signbit_float"
    ; "caml_neg_float"
    ; "caml_abs_float"
    ; "caml_ceil_float"
    ; "caml_floor_float"
    ; "caml_trunc_float"
    ; "caml_round_float"
    ; "caml_sqrt_float"
    ; "caml_eq_float"
    ; "caml_neq_float"
    ; "caml_ge_float"
    ; "caml_le_float"
    ; "caml_gt_float"
    ; "caml_lt_float"
    ; "caml_int_of_float"
    ; "caml_cos_float"
    ; "caml_sin_float"
    ; "caml_tan_float"
    ; "caml_acos_float"
    ; "caml_asin_float"
    ; "caml_atan_float"
    ; "caml_atan2_float"
    ; "caml_cosh_float"
    ; "caml_sinh_float"
    ; "caml_tanh_float"
    ; "caml_acosh_float"
    ; "caml_asinh_float"
    ; "caml_atanh_float"
    ; "caml_cbrt_float"
    ; "caml_exp_float"
    ; "caml_exp2_float"
    ; "caml_log_float"
    ; "caml_expm1_float"
    ; "caml_log1p_float"
    ; "caml_log2_float"
    ; "caml_log10_float"
    ; "caml_power_float"
    ; "caml_hypot_float"
    ; "caml_fmod_float"
    ; "caml_int32_bits_of_float"
    ; "caml_int32_float_of_bits"
    ; "caml_int32_of_float"
    ; "caml_int32_to_float"
    ; "caml_int32_neg"
    ; "caml_int32_add"
    ; "caml_int32_sub"
    ; "caml_int32_mul"
    ; "caml_int32_and"
    ; "caml_int32_or"
    ; "caml_int32_xor"
    ; "caml_int32_div"
    ; "caml_int32_mod"
    ; "caml_int32_shift_left"
    ; "caml_int32_shift_right"
    ; "caml_int32_shift_right_unsigned"
    ; "caml_int32_to_int"
    ; "caml_nativeint_of_int32"
    ; "caml_nativeint_to_int32"
    ; "caml_int64_bits_of_float"
    ; "caml_int64_float_of_bits"
    ; "caml_int64_of_float"
    ; "caml_int64_to_float"
    ; "caml_int64_neg"
    ; "caml_int64_add"
    ; "caml_int64_sub"
    ; "caml_int64_mul"
    ; "caml_int64_and"
    ; "caml_int64_or"
    ; "caml_int64_xor"
    ; "caml_int64_div"
    ; "caml_int64_mod"
    ; "caml_int64_shift_left"
    ; "caml_int64_shift_right"
    ; "caml_int64_shift_right_unsigned"
    ; "caml_int64_to_int"
    ; "caml_int64_to_int32"
    ; "caml_int64_of_int32"
    ; "caml_int64_to_nativeint"
    ; "caml_int64_of_nativeint"
    ; "caml_nativeint_bits_of_float"
    ; "caml_nativeint_float_of_bits"
    ; "caml_nativeint_of_float"
    ; "caml_nativeint_to_float"
    ; "caml_nativeint_neg"
    ; "caml_nativeint_add"
    ; "caml_nativeint_sub"
    ; "caml_nativeint_mul"
    ; "caml_nativeint_and"
    ; "caml_nativeint_or"
    ; "caml_nativeint_xor"
    ; "caml_nativeint_div"
    ; "caml_nativeint_mod"
    ; "caml_nativeint_shift_left"
    ; "caml_nativeint_shift_right"
    ; "caml_nativeint_shift_right_unsigned"
    ; "caml_nativeint_to_int"
    ; "caml_floatarray_unsafe_set"
    ];
  h

let box_numbers p st types =
  (* We box numbers eagerly if the boxed value is ever used. *)
  let should_box = Var.ISet.empty () in
  let rec box y =
    if not (Var.ISet.mem should_box y)
    then (
      Var.ISet.add should_box y;
      let typ = Var.Tbl.get types y in
      (match typ with
      | Number (n, Unboxed) -> Var.Tbl.set types y (Number (n, Boxed))
      | _ -> ());
      match typ with
      | Number (_, Unboxed) | Top -> (
          match st.global_flow_state.defs.(Var.idx y) with
          | Expr (Apply { f; _ }) -> (
              match Global_flow.get_unique_closure st.global_flow_info f with
              | None -> ()
              | Some (g, _) ->
                  if can_unbox_return_value st.fun_info g
                  then
                    let s = Var.Map.find g st.global_flow_info.info_return_vals in
                    Var.Set.iter box s)
          | Expr _ -> ()
          | Phi { known; _ } -> Var.Set.iter box known)
      | Number (_, Boxed) | Int _ | Tuple _ | Bot -> ())
  in
  Code.fold_closures
    p
    (fun name_opt _ (pc, _) _ () ->
      traverse
        { fold = Code.fold_children }
        (fun pc () ->
          let b = Addr.Map.find pc p.blocks in
          List.iter
            ~f:(fun i ->
              match i with
              | Let (_, e) -> (
                  match e with
                  | Apply { f; args; _ } ->
                      if
                        match Global_flow.get_unique_closure st.global_flow_info f with
                        | None -> true
                        | Some (g, _) -> not (can_unbox_parameters st.fun_info g)
                      then List.iter ~f:box args
                  | Block (tag, lst, _, _) -> if tag <> 254 then Array.iter ~f:box lst
                  | Prim (Extern s, args) ->
                      if not (String.Hashtbl.mem primitives_with_unboxed_parameters s)
                      then
                        List.iter
                          ~f:(fun a ->
                            match a with
                            | Pv y -> box y
                            | Pc _ -> ())
                          args
                  | Prim ((Eq | Neq), args) ->
                      List.iter
                        ~f:(fun a ->
                          match a with
                          | Pv y -> box y
                          | Pc _ -> ())
                        args
                  | Prim ((Vectlength | Array_get | Not | IsInt | Lt | Le | Ult), _)
                  | Field _ | Closure _ | Constant _ | Special _ -> ())
              | Set_field (_, _, Non_float, y) | Array_set (_, _, y) -> box y
              | Assign _ | Offset_ref _ | Set_field (_, _, Float, _) | Event _ -> ())
            b.body;
          match b.branch with
          | Return y ->
              Option.iter
                ~f:(fun g -> if not (can_unbox_return_value st.fun_info g) then box y)
                name_opt
          | Raise _ | Stop | Branch _ | Cond _ | Switch _ | Pushtrap _ | Poptrap _ -> ())
        pc
        p.blocks
        ())
    ()

type t =
  { types : typ Var.Tbl.t
  ; return_types : typ Var.Hashtbl.t
  }

let f ~global_flow_state ~global_flow_info ~fun_info ~deadcode_sentinal p =
  let t = Timer.make () in
  update_deps global_flow_state p;
  let boxed_function_parameters = mark_boxed_function_parameters ~fun_info p in
  let st = { global_flow_state; global_flow_info; boxed_function_parameters; fun_info } in
  let types = solver st in
  Var.Tbl.set types deadcode_sentinal (Int Normalized);
  box_numbers p st types;
  if times () then Format.eprintf "  type analysis: %a@." Timer.print t;
  if debug ()
  then (
    Var.ISet.iter
      (fun x ->
        match global_flow_state.defs.(Var.idx x) with
        | Expr _ -> ()
        | Phi _ ->
            let t = Var.Tbl.get types x in
            if not (Domain.equal t Top)
            then Format.eprintf "%a: %a@." Var.print x Domain.print t)
      global_flow_state.vars;
    Print.program
      Format.err_formatter
      (fun _ i ->
        match i with
        | Instr (Let (x, _)) -> Format.asprintf "{%a}" Domain.print (Var.Tbl.get types x)
        | _ -> "")
      p);
  let return_types = Var.Hashtbl.create 128 in
  Code.fold_closures
    p
    (fun name_opt _ _ _ () ->
      Option.iter
        ~f:(fun f ->
          if can_unbox_return_value fun_info f
          then
            let s = Var.Map.find f global_flow_info.info_return_vals in
            Var.Hashtbl.replace
              return_types
              f
              (Var.Set.fold (fun x t -> Domain.join (Var.Tbl.get types x) t) s Bot))
        name_opt)
    ();
  { types; return_types }

let var_type info x = Var.Tbl.get info.types x

let return_type info f = try Var.Hashtbl.find info.return_types f with Not_found -> Top
