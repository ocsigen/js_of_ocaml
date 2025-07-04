open! Stdlib
open Code
open Global_flow

let debug = Debug.find "typing"

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

type typ =
  | Top
  | Int of Integer.kind
  | Number of boxed_number
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
    | Number n, Number n' -> if Poly.equal n n' then t else Top
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
    | Number t, Number t' -> Poly.equal t t'
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
    | Number Int32 -> Format.fprintf f "int32"
    | Number Int64 -> Format.fprintf f "int64"
    | Number Nativeint -> Format.fprintf f "nativeint"
    | Number Float -> Format.fprintf f "float"
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

let mark_function_parameters { blocks; _ } =
  let function_parameters = Var.ISet.empty () in
  let set x = Var.ISet.add function_parameters x in
  Addr.Map.iter
    (fun _ block ->
      List.iter block.body ~f:(fun i ->
          match i with
          | Let (_, Closure (params, _, _)) -> List.iter ~f:set params
          | _ -> ()))
    blocks;
  function_parameters

type st =
  { state : state
  ; info : info
  ; function_parameters : Var.ISet.t
  }

let rec constant_type (c : constant) =
  match c with
  | Int _ -> Int Normalized
  | Int32 _ -> Number Int32
  | Int64 _ -> Number Int64
  | NativeInt _ -> Number Nativeint
  | Float _ -> Number Float
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
  | "caml_compare" -> Int Ref
  | "caml_int32_bswap" -> Number Int32
  | "caml_nativeint_bswap" -> Number Nativeint
  | "caml_int64_bswap" -> Number Int64
  | "caml_int32_compare" | "caml_nativeint_compare" | "caml_int64_compare" -> Int Ref
  | "caml_string_get32" -> Number Int32
  | "caml_string_get64" -> Number Int64
  | "caml_bytes_get32" -> Number Int32
  | "caml_bytes_get64" -> Number Int64
  | "caml_lxm_next" -> Number Int64
  | "caml_ba_uint8_get32" -> Number Int32
  | "caml_ba_uint8_get64" -> Number Int64
  | "caml_nextafter_float" -> Number Float
  | "caml_classify_float" -> Int Ref
  | "caml_ldexp_float" | "caml_erf_float" | "caml_erfc_float" -> Number Float
  | "caml_float_compare" -> Int Ref
  | "caml_floatarray_unsafe_get" -> Number Float
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
  | "caml_copysign_float" -> Number Float
  | "caml_signbit_float" -> Int Normalized
  | "caml_neg_float"
  | "caml_abs_float"
  | "caml_ceil_float"
  | "caml_floor_float"
  | "caml_trunc_float"
  | "caml_round_float"
  | "caml_sqrt_float" -> Number Float
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
  | "caml_fmod_float" -> Number Float
  | "caml_int32_bits_of_float" -> Number Int32
  | "caml_int32_float_of_bits" -> Number Float
  | "caml_int32_of_float" -> Number Int32
  | "caml_int32_to_float" -> Number Float
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
  | "caml_int32_shift_right_unsigned" -> Number Int32
  | "caml_int32_to_int" -> Int Unnormalized
  | "caml_int32_of_int" -> Number Int32
  | "caml_nativeint_of_int32" -> Number Nativeint
  | "caml_nativeint_to_int32" -> Number Int32
  | "caml_int64_bits_of_float" -> Number Int64
  | "caml_int64_float_of_bits" -> Number Float
  | "caml_int64_of_float" -> Number Int64
  | "caml_int64_to_float" -> Number Float
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
  | "caml_int64_shift_right_unsigned" -> Number Int64
  | "caml_int64_to_int" -> Int Unnormalized
  | "caml_int64_of_int" -> Number Int64
  | "caml_int64_to_int32" -> Number Int32
  | "caml_int64_of_int32" -> Number Int64
  | "caml_int64_to_nativeint" -> Number Nativeint
  | "caml_int64_of_nativeint" -> Number Int64
  | "caml_nativeint_bits_of_float" -> Number Nativeint
  | "caml_nativeint_float_of_bits" -> Number Float
  | "caml_nativeint_of_float" -> Number Nativeint
  | "caml_nativeint_to_float" -> Number Float
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
  | "caml_nativeint_shift_right_unsigned" -> Number Nativeint
  | "caml_nativeint_to_int" -> Int Unnormalized
  | "caml_nativeint_of_int" -> Number Nativeint
  | "caml_int_compare" -> Int Normalized
  | _ -> Top

let propagate st approx x : Domain.t =
  match st.state.defs.(Var.idx x) with
  | Phi { known; others; unit } ->
      let res = Domain.join_set ~others (fun y -> Var.Tbl.get approx y) known in
      let res = if unit then Domain.join (Int Unnormalized) res else res in
      if Var.ISet.mem st.function_parameters x then Domain.box res else res
  | Expr e -> (
      match e with
      | Constant c -> constant_type c
      | Closure _ -> Top
      | Block (_, lst, _, _) ->
          Tuple
            (Array.mapi
               ~f:(fun i y ->
                 match st.state.mutable_fields.(Var.idx x) with
                 | All_fields -> Top
                 | Some_fields s when IntSet.mem i s -> Top
                 | Some_fields _ | No_field ->
                     Domain.limit (Domain.box (Var.Tbl.get approx y)))
               lst)
      | Field (_, _, Float) -> Number Float
      | Field (y, n, Non_float) -> (
          match Var.Tbl.get approx y with
          | Tuple t -> if n < Array.length t then t.(n) else Bot
          | Top -> Top
          | _ -> Bot)
      | Prim
          ( Extern ("caml_check_bound" | "caml_check_bound_float" | "caml_check_bound_gen")
          , [ Pv y; _ ] ) -> Var.Tbl.get approx y
      | Prim ((Array_get | Extern "caml_array_unsafe_get"), [ Pv y; _ ]) -> (
          match Var.Tbl.get st.info.info_approximation y with
          | Values { known; others } ->
              Domain.join_set
                ~others
                (fun z ->
                  match st.state.defs.(Var.idx z) with
                  | Expr (Block (_, lst, _, _)) ->
                      let m =
                        match st.state.mutable_fields.(Var.idx z) with
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
          match Var.Tbl.get st.info.info_approximation f with
          | Values { known; others } ->
              Domain.join_set
                ~others
                (fun g ->
                  match st.state.defs.(Var.idx g) with
                  | Expr (Closure (params, _, _))
                    when List.length args = List.length params ->
                      Domain.box
                        (Domain.join_set
                           (fun y -> Var.Tbl.get approx y)
                           (Var.Map.find g st.state.return_values))
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
    { G.domain = st.state.vars
    ; G.iter_children =
        (fun f x ->
          List.iter ~f (Var.Tbl.get st.state.deps x);
          List.iter
            ~f:(fun g -> List.iter ~f (associated_list st.state.function_call_sites g))
            (associated_list st.state.functions_from_returned_value x))
    }
  in
  Solver.f () g (propagate st)

let f ~state ~info ~deadcode_sentinal p =
  update_deps state p;
  let function_parameters = mark_function_parameters p in
  let typ = solver { state; info; function_parameters } in
  Var.Tbl.set typ deadcode_sentinal (Int Normalized);
  if debug ()
  then (
    Var.ISet.iter
      (fun x ->
        match state.defs.(Var.idx x) with
        | Expr _ -> ()
        | Phi _ ->
            let t = Var.Tbl.get typ x in
            if not (Domain.equal t Top)
            then Format.eprintf "%a: %a@." Var.print x Domain.print t)
      state.vars;
    Print.program
      Format.err_formatter
      (fun _ i ->
        match i with
        | Instr (Let (x, _)) -> Format.asprintf "{%a}" Domain.print (Var.Tbl.get typ x)
        | _ -> "")
      p);
  typ
