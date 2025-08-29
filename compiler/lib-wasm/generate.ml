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

open! Stdlib
open Code
module W = Wasm_ast
open Code_generation

let times = Debug.find "times"

let effects_cps () =
  match Config.effects () with
  | `Cps -> true
  | `Disabled | `Jspi -> false
  | `Double_translation -> assert false

module Generate (Target : Target_sig.S) = struct
  open Target

  type ctx =
    { live : int array
    ; in_cps : Effects.in_cps
    ; global_flow_info : Global_flow.info
    ; fun_info : Call_graph_analysis.t
    ; types : Typing.t
    ; raising_funcs : unit Var.Hashtbl.t
    ; blocks : block Addr.Map.t
    ; closures : Closure_conversion.closure Var.Map.t
    ; global_context : Code_generation.context
    }

  let label_index context pc =
    let rec index_rec context pc i =
      match context with
      | `Block pc' :: _ when pc = pc' -> i
      | (`Block _ | `Skip | `Catch) :: rem -> index_rec rem pc (i + 1)
      | [] -> assert false
    in
    index_rec context pc 0

  let catch_index context =
    let rec index_rec context i =
      match context with
      | `Catch :: _ -> Some i
      | (`Block _ | `Skip | `Return) :: rem -> index_rec rem (i + 1)
      | [] -> None
    in
    index_rec context 0

  let bound_error_pc = -1

  let zero_divide_pc = -2

  type repr =
    | Value
    | Float
    | Int
    | Int32
    | Nativeint
    | Int64

  let repr_type r =
    match r with
    | Value -> Type.value
    | Float -> F64
    | Int | Int32 | Nativeint -> I32
    | Int64 -> I64

  let specialized_primitive_type (_, params, result) =
    { W.params = List.map ~f:repr_type params; result = [ repr_type result ] }

  let specialized_primitives =
    let h = String.Hashtbl.create 18 in
    List.iter
      ~f:(fun (nm, typ) -> String.Hashtbl.add h nm typ)
      [ "caml_int32_bswap", (`Pure, [ Int32 ], Int32)
      ; "caml_nativeint_bswap", (`Pure, [ Nativeint ], Nativeint)
      ; "caml_int64_bswap", (`Pure, [ Int64 ], Int64)
      ; "caml_int32_compare", (`Pure, [ Int32; Int32 ], Int)
      ; "caml_nativeint_compare", (`Pure, [ Nativeint; Nativeint ], Int)
      ; "caml_int64_compare", (`Pure, [ Int64; Int64 ], Int)
      ; "caml_string_get16", (`Mutator, [ Value; Int ], Int)
      ; "caml_string_get32", (`Mutator, [ Value; Int ], Int32)
      ; "caml_string_get64", (`Mutator, [ Value; Int ], Int64)
      ; "caml_bytes_get16", (`Mutator, [ Value; Int ], Int)
      ; "caml_bytes_get32", (`Mutator, [ Value; Int ], Int32)
      ; "caml_bytes_get64", (`Mutator, [ Value; Int ], Int64)
      ; "caml_bytes_set16", (`Mutator, [ Value; Int; Int ], Value)
      ; "caml_bytes_set32", (`Mutator, [ Value; Int; Int32 ], Value)
      ; "caml_bytes_set64", (`Mutator, [ Value; Int; Int64 ], Value)
      ; "caml_lxm_next", (`Mutable, [ Value ], Int64)
      ; "caml_ba_uint8_get16", (`Mutator, [ Value; Int ], Int)
      ; "caml_ba_uint8_get32", (`Mutator, [ Value; Int ], Int32)
      ; "caml_ba_uint8_get64", (`Mutator, [ Value; Int ], Int64)
      ; "caml_ba_uint8_set16", (`Mutator, [ Value; Int; Int ], Value)
      ; "caml_ba_uint8_set32", (`Mutator, [ Value; Int; Int32 ], Value)
      ; "caml_ba_uint8_set64", (`Mutator, [ Value; Int; Int64 ], Value)
      ; "caml_nextafter_float", (`Pure, [ Float; Float ], Float)
      ; "caml_classify_float", (`Pure, [ Float ], Value)
      ; "caml_ldexp_float", (`Pure, [ Float; Int ], Float)
      ; "caml_erf_float", (`Pure, [ Float ], Float)
      ; "caml_erfc_float", (`Pure, [ Float ], Float)
      ; "caml_float_compare", (`Pure, [ Float; Float ], Int)
      ];
    h

  let float_bin_op op f g =
    let* f = f in
    let* g = g in
    return (W.BinOp (F64 op, f, g))

  let float_un_op op f =
    let* f = f in
    return (W.UnOp (F64 op, f))

  let int32_bin_op op f g =
    let* f = f in
    let* g = g in
    return (W.BinOp (I32 op, f, g))

  let int64_bin_op op f g =
    let* f = f in
    let* g = g in
    return (W.BinOp (I64 op, f, g))

  let int64_shift_op op f g =
    let* f = f in
    let* g = g in
    return (W.BinOp (I64 op, f, I64ExtendI32 (S, g)))

  let nativeint_bin_op op f g =
    let* f = f in
    let* g = g in
    return (W.BinOp (I32 op, f, g))

  let get_type ctx p =
    match p with
    | Pv x -> Typing.var_type ctx.types x
    | Pc c -> Typing.constant_type c

  let convert ~(from : Typing.typ) ~(into : Typing.typ) e =
    match from, into with
    | Int Unnormalized, Int Normalized -> Arith.((e lsl const 1l) asr const 1l)
    | Int (Normalized | Unnormalized), Int (Normalized | Unnormalized) -> e
    (* Dummy value *)
    | Int (Unnormalized | Normalized), Number ((Int32 | Nativeint), Unboxed) ->
        return (W.Const (I32 0l))
    | Int (Unnormalized | Normalized), Number (Int64, Unboxed) ->
        return (W.Const (I64 0L))
    | Int (Unnormalized | Normalized), Number (Float, Unboxed) ->
        return (W.Const (F64 0.))
    | _, Int (Normalized | Unnormalized) -> Value.int_val e
    | Int (Unnormalized | Normalized), _ -> Value.val_int e
    | Number (_, Unboxed), Number (_, Unboxed) -> e
    | _, Number (Int32, Unboxed) -> Memory.unbox_int32 e
    | _, Number (Int64, Unboxed) -> Memory.unbox_int64 e
    | _, Number (Nativeint, Unboxed) -> Memory.unbox_nativeint e
    | _, Number (Float, Unboxed) -> Memory.unbox_float e
    | Number (Int32, Unboxed), _ -> Memory.box_int32 e
    | Number (Int64, Unboxed), _ -> Memory.box_int64 e
    | Number (Nativeint, Unboxed), _ -> Memory.box_nativeint e
    | Number (Float, Unboxed), _ -> Memory.box_float e
    | _ -> e

  let load_and_box ctx x = convert ~from:(Typing.var_type ctx.types x) ~into:Top (load x)

  let transl_prim_arg ctx ?(typ = Typing.Top) x =
    convert
      ~from:(get_type ctx x)
      ~into:typ
      (match x with
      | Pv x -> load x
      | Pc c -> Constant.translate ~unboxed:false c)

  let translate_int_comparison ctx op x y =
    match get_type ctx x, get_type ctx y with
    | Int Unnormalized, Int Unnormalized
    | Int Normalized, Int Unnormalized
    | Int Unnormalized, Int Normalized ->
        op
          Arith.(transl_prim_arg ctx ~typ:(Int Unnormalized) x lsl const 1l)
          Arith.(transl_prim_arg ctx ~typ:(Int Unnormalized) y lsl const 1l)
    | _ ->
        op
          (transl_prim_arg ctx ~typ:(Int Normalized) x)
          (transl_prim_arg ctx ~typ:(Int Normalized) y)

  let translate_int_equality ctx ~negate x y =
    match get_type ctx x, get_type ctx y with
    | (Int Normalized as typ), Int Normalized ->
        (if negate then Arith.( <> ) else Arith.( = ))
          (transl_prim_arg ctx ~typ x)
          (transl_prim_arg ctx ~typ y)
    | Int (Normalized | Unnormalized), Int (Normalized | Unnormalized) ->
        (if negate then Arith.( <> ) else Arith.( = ))
          Arith.(transl_prim_arg ctx ~typ:(Int Unnormalized) x lsl const 1l)
          Arith.(transl_prim_arg ctx ~typ:(Int Unnormalized) y lsl const 1l)
    | Top, Top ->
        Value.js_eqeqeq
          ~negate
          (transl_prim_arg ctx ~typ:Top x)
          (transl_prim_arg ctx ~typ:Top y)
    | Bot, _ | _, Bot ->
        (* this is deadcode *)
        (if negate then Value.phys_neq else Value.phys_eq)
          (transl_prim_arg ctx ~typ:Top x)
          (transl_prim_arg ctx ~typ:Top y)
    | (Int _ | Number _ | Tuple _ | Bigarray _), _
    | _, (Int _ | Number _ | Tuple _ | Bigarray _) ->
        (* Only Top may contain JavaScript values *)
        (if negate then Value.phys_neq else Value.phys_eq)
          (transl_prim_arg ctx ~typ:Top x)
          (transl_prim_arg ctx ~typ:Top y)

  let internal_primitives =
    let h = String.Hashtbl.create 128 in
    List.iter
      ~f:(fun (nm, k, f) ->
        String.Hashtbl.add h nm (k, fun ctx _ l -> f (fun x -> transl_prim_arg ctx x) l))
      internal_primitives;
    h

  let register_prim name k f = String.Hashtbl.add internal_primitives name (k, f)

  let invalid_arity name l ~expected =
    failwith
      (Printf.sprintf
         "Invalid arity for primitive %s. Expecting %d but used with %d."
         name
         expected
         (List.length l))

  let register_un_prim name k ?typ f =
    register_prim name k (fun ctx _ l ->
        match l with
        | [ x ] -> f (transl_prim_arg ctx ?typ x)
        | l -> invalid_arity name l ~expected:1)

  let register_bin_prim name k ?tx ?ty f =
    register_prim name k (fun ctx _ l ->
        match l with
        | [ x; y ] -> f (transl_prim_arg ctx ?typ:tx x) (transl_prim_arg ctx ?typ:ty y)
        | _ -> invalid_arity name l ~expected:2)

  let register_bin_prim_ctx name ?tx ?ty f =
    register_prim name `Mutator (fun ctx context l ->
        match l with
        | [ x; y ] ->
            f context (transl_prim_arg ctx ?typ:tx x) (transl_prim_arg ctx ?typ:ty y)
        | _ -> invalid_arity name l ~expected:2)

  let register_tern_prim name ?ty ?tz f =
    register_prim name `Mutator (fun ctx _ l ->
        match l with
        | [ x; y; z ] ->
            f
              (transl_prim_arg ctx x)
              (transl_prim_arg ctx ?typ:ty y)
              (transl_prim_arg ctx ?typ:tz z)
        | _ -> invalid_arity name l ~expected:3)

  let register_tern_prim_ctx name ?ty ?tz f =
    register_prim name `Mutator (fun ctx context l ->
        match l with
        | [ x; y; z ] ->
            f
              context
              (transl_prim_arg ctx x)
              (transl_prim_arg ctx ?typ:ty y)
              (transl_prim_arg ctx ?typ:tz z)
        | _ -> invalid_arity name l ~expected:3)

  let register_comparison name cmp_int cmp_boxed_int cmp_float =
    register_prim name `Mutator (fun ctx _ l ->
        match l with
        | [ x; y ] -> (
            match get_type ctx x, get_type ctx y with
            | Int _, Int _ -> cmp_int ctx x y
            | Number (Int32, _), Number (Int32, _) ->
                let x = transl_prim_arg ctx ~typ:(Number (Int32, Unboxed)) x in
                let y = transl_prim_arg ctx ~typ:(Number (Int32, Unboxed)) y in
                int32_bin_op cmp_boxed_int x y
            | Number (Nativeint, _), Number (Nativeint, _) ->
                let x = transl_prim_arg ctx ~typ:(Number (Nativeint, Unboxed)) x in
                let y = transl_prim_arg ctx ~typ:(Number (Nativeint, Unboxed)) y in
                nativeint_bin_op cmp_boxed_int x y
            | Number (Int64, _), Number (Int64, _) ->
                let x = transl_prim_arg ctx ~typ:(Number (Int64, Unboxed)) x in
                let y = transl_prim_arg ctx ~typ:(Number (Int64, Unboxed)) y in
                int64_bin_op cmp_boxed_int x y
            | Number (Float, _), Number (Float, _) ->
                let x = transl_prim_arg ctx ~typ:(Number (Float, Unboxed)) x in
                let y = transl_prim_arg ctx ~typ:(Number (Float, Unboxed)) y in
                float_bin_op cmp_float x y
            | _ ->
                let* f =
                  register_import
                    ~name
                    (Fun { W.params = [ Type.value; Type.value ]; result = [ I32 ] })
                in
                let* x = transl_prim_arg ctx x in
                let* y = transl_prim_arg ctx y in
                return (W.Call (f, [ x; y ])))
        | _ -> invalid_arity name l ~expected:2)

  let () =
    register_bin_prim
      "caml_floatarray_unsafe_get"
      `Mutable
      ~ty:(Int Normalized)
      Memory.float_array_get;
    register_tern_prim "caml_array_unsafe_set" ~ty:(Int Normalized) (fun x y z ->
        seq (Memory.gen_array_set x y z) Value.unit);
    register_tern_prim "caml_array_unsafe_set_addr" ~ty:(Int Normalized) (fun x y z ->
        seq (Memory.array_set x y z) Value.unit);
    register_tern_prim
      "caml_floatarray_unsafe_set"
      ~ty:(Int Normalized)
      ~tz:(Number (Float, Unboxed))
      (fun x y z -> seq (Memory.float_array_set x y z) Value.unit);
    register_bin_prim "caml_string_unsafe_get" `Pure ~ty:(Int Normalized) Memory.bytes_get;
    register_bin_prim
      "caml_bytes_unsafe_get"
      `Mutable
      ~ty:(Int Normalized)
      Memory.bytes_get;
    register_tern_prim
      "caml_string_unsafe_set"
      ~ty:(Int Normalized)
      ~tz:(Int Unnormalized)
      (fun x y z -> seq (Memory.bytes_set x y z) Value.unit);
    register_tern_prim
      "caml_bytes_unsafe_set"
      ~ty:(Int Normalized)
      ~tz:(Int Unnormalized)
      (fun x y z -> seq (Memory.bytes_set x y z) Value.unit);
    let bytes_get context x y =
      seq
        (let* cond = Arith.uge y (Memory.bytes_length x) in
         instr (W.Br_if (label_index context bound_error_pc, cond)))
        (Memory.bytes_get x y)
    in
    register_bin_prim_ctx "caml_string_get" ~ty:(Int Normalized) bytes_get;
    register_bin_prim_ctx "caml_bytes_get" ~ty:(Int Normalized) bytes_get;
    let bytes_set context x y z =
      seq
        (let* cond = Arith.uge y (Memory.bytes_length x) in
         let* () = instr (W.Br_if (label_index context bound_error_pc, cond)) in
         Memory.bytes_set x y z)
        Value.unit
    in
    register_tern_prim_ctx
      "caml_string_set"
      ~ty:(Int Normalized)
      ~tz:(Int Unnormalized)
      bytes_set;
    register_tern_prim_ctx
      "caml_bytes_set"
      ~ty:(Int Normalized)
      ~tz:(Int Unnormalized)
      bytes_set;
    register_un_prim "caml_ml_string_length" `Pure (fun x -> Memory.bytes_length x);
    register_un_prim "caml_ml_bytes_length" `Pure (fun x -> Memory.bytes_length x);
    register_bin_prim
      "%int_add"
      `Pure
      ~tx:(Int Unnormalized)
      ~ty:(Int Unnormalized)
      Value.int_add;
    register_bin_prim
      "%int_sub"
      `Pure
      ~tx:(Int Unnormalized)
      ~ty:(Int Unnormalized)
      Value.int_sub;
    register_bin_prim
      "%int_mul"
      `Pure
      ~tx:(Int Unnormalized)
      ~ty:(Int Unnormalized)
      Value.int_mul;
    register_bin_prim
      "%direct_int_mul"
      `Pure
      ~tx:(Int Unnormalized)
      ~ty:(Int Unnormalized)
      Value.int_mul;
    register_bin_prim
      "%direct_int_div"
      `Pure
      ~tx:(Int Normalized)
      ~ty:(Int Normalized)
      Value.int_div;
    register_bin_prim_ctx
      "%int_div"
      ~tx:(Int Normalized)
      ~ty:(Int Normalized)
      (fun context x y ->
        seq
          (let* cond = Arith.eqz y in
           instr (W.Br_if (label_index context zero_divide_pc, cond)))
          (Value.int_div x y));
    register_bin_prim
      "%direct_int_mod"
      `Pure
      ~tx:(Int Normalized)
      ~ty:(Int Normalized)
      Value.int_mod;
    register_bin_prim_ctx
      "%int_mod"
      ~tx:(Int Normalized)
      ~ty:(Int Normalized)
      (fun context x y ->
        seq
          (let* cond = Arith.eqz y in
           instr (W.Br_if (label_index context zero_divide_pc, cond)))
          (Value.int_mod x y));
    register_un_prim "%int_neg" `Pure ~typ:(Int Unnormalized) Value.int_neg;
    register_bin_prim
      "%int_or"
      `Pure
      ~tx:(Int Unnormalized)
      ~ty:(Int Unnormalized)
      Value.int_or;
    register_bin_prim
      "%int_and"
      `Pure
      ~tx:(Int Unnormalized)
      ~ty:(Int Unnormalized)
      Value.int_and;
    register_bin_prim
      "%int_xor"
      `Pure
      ~tx:(Int Unnormalized)
      ~ty:(Int Unnormalized)
      Value.int_xor;
    register_bin_prim
      "%int_lsl"
      `Pure
      ~tx:(Int Unnormalized)
      ~ty:(Int Unnormalized)
      Value.int_lsl;
    register_bin_prim
      "%int_lsr"
      `Pure
      ~tx:(Int Unnormalized)
      ~ty:(Int Unnormalized)
      Value.int_lsr;
    register_bin_prim
      "%int_asr"
      `Pure
      ~tx:(Int Normalized)
      ~ty:(Int Unnormalized)
      Value.int_asr;
    register_un_prim "%direct_obj_tag" `Pure Memory.tag;
    register_bin_prim_ctx "caml_check_bound" ~ty:(Int Normalized) (fun context x y ->
        seq
          (let* cond = Arith.uge y (Memory.array_length x) in
           instr (W.Br_if (label_index context bound_error_pc, cond)))
          x);
    register_bin_prim_ctx "caml_check_bound_gen" ~ty:(Int Normalized) (fun context x y ->
        seq
          (let* cond = Arith.uge y (Memory.gen_array_length x) in
           instr (W.Br_if (label_index context bound_error_pc, cond)))
          x);
    register_bin_prim_ctx
      "caml_check_bound_float"
      ~ty:(Int Normalized)
      (fun context x y ->
        seq
          (let a = Code.Var.fresh () in
           let* () = store a x in
           let label = label_index context bound_error_pc in
           (* If this is not a float array, it must be the
                      empty array, and the bound check should fail. *)
           let* cond = Arith.eqz (Memory.check_is_float_array (load a)) in
           let* () = instr (W.Br_if (label, cond)) in
           let* cond = Arith.uge y (Memory.float_array_length (load a)) in
           instr (W.Br_if (label, cond)))
          x);
    register_bin_prim
      "caml_add_float"
      `Pure
      ~tx:(Number (Float, Unboxed))
      ~ty:(Number (Float, Unboxed))
      (fun f g -> float_bin_op Add f g);
    register_bin_prim
      "caml_sub_float"
      ~tx:(Number (Float, Unboxed))
      ~ty:(Number (Float, Unboxed))
      `Pure
      (fun f g -> float_bin_op Sub f g);
    register_bin_prim
      "caml_mul_float"
      ~tx:(Number (Float, Unboxed))
      ~ty:(Number (Float, Unboxed))
      `Pure
      (fun f g -> float_bin_op Mul f g);
    register_bin_prim
      "caml_div_float"
      ~tx:(Number (Float, Unboxed))
      ~ty:(Number (Float, Unboxed))
      `Pure
      (fun f g -> float_bin_op Div f g);
    register_bin_prim
      "caml_copysign_float"
      `Pure
      ~tx:(Number (Float, Unboxed))
      ~ty:(Number (Float, Unboxed))
      (fun f g -> float_bin_op CopySign f g);
    register_un_prim
      "caml_signbit_float"
      `Pure
      ~typ:(Number (Float, Unboxed))
      (fun f ->
        let* f = f in
        let sign = W.BinOp (F64 CopySign, Const (F64 1.), f) in
        return (W.BinOp (F64 Lt, sign, Const (F64 0.))));
    register_un_prim
      "caml_neg_float"
      `Pure
      ~typ:(Number (Float, Unboxed))
      (fun f -> float_un_op Neg f);
    register_un_prim
      "caml_abs_float"
      `Pure
      ~typ:(Number (Float, Unboxed))
      (fun f -> float_un_op Abs f);
    register_un_prim
      "caml_ceil_float"
      `Pure
      ~typ:(Number (Float, Unboxed))
      (fun f -> float_un_op Ceil f);
    register_un_prim
      "caml_floor_float"
      `Pure
      ~typ:(Number (Float, Unboxed))
      (fun f -> float_un_op Floor f);
    register_un_prim
      "caml_trunc_float"
      `Pure
      ~typ:(Number (Float, Unboxed))
      (fun f -> float_un_op Trunc f);
    register_un_prim "caml_round_float" `Pure ~typ:(Number (Float, Unboxed)) Math.round;
    register_un_prim
      "caml_sqrt_float"
      `Pure
      ~typ:(Number (Float, Unboxed))
      (fun f -> float_un_op Sqrt f);
    register_bin_prim
      "caml_eq_float"
      `Pure
      ~tx:(Number (Float, Unboxed))
      ~ty:(Number (Float, Unboxed))
      (fun f g -> float_bin_op Eq f g);
    register_bin_prim
      "caml_neq_float"
      `Pure
      ~tx:(Number (Float, Unboxed))
      ~ty:(Number (Float, Unboxed))
      (fun f g -> float_bin_op Ne f g);
    register_bin_prim
      "caml_ge_float"
      `Pure
      ~tx:(Number (Float, Unboxed))
      ~ty:(Number (Float, Unboxed))
      (fun f g -> float_bin_op Ge f g);
    register_bin_prim
      "caml_le_float"
      `Pure
      ~tx:(Number (Float, Unboxed))
      ~ty:(Number (Float, Unboxed))
      (fun f g -> float_bin_op Le f g);
    register_bin_prim
      "caml_gt_float"
      `Pure
      ~tx:(Number (Float, Unboxed))
      ~ty:(Number (Float, Unboxed))
      (fun f g -> float_bin_op Gt f g);
    register_bin_prim
      "caml_lt_float"
      ~tx:(Number (Float, Unboxed))
      ~ty:(Number (Float, Unboxed))
      `Pure
      (fun f g -> float_bin_op Lt f g);
    register_un_prim
      "caml_int_of_float"
      `Pure
      ~typ:(Number (Float, Unboxed))
      (fun f ->
        let* f = f in
        return (W.UnOp (I32 (TruncSatF64 S), f)));
    register_un_prim "caml_float_of_int" `Pure ~typ:(Int Normalized) (fun n ->
        let* n = n in
        return (W.UnOp (F64 (Convert (`I32, S)), n)));
    register_un_prim "caml_cos_float" `Pure ~typ:(Number (Float, Unboxed)) Math.cos;
    register_un_prim "caml_sin_float" `Pure ~typ:(Number (Float, Unboxed)) Math.sin;
    register_un_prim "caml_tan_float" `Pure ~typ:(Number (Float, Unboxed)) Math.tan;
    register_un_prim "caml_acos_float" `Pure ~typ:(Number (Float, Unboxed)) Math.acos;
    register_un_prim "caml_asin_float" `Pure ~typ:(Number (Float, Unboxed)) Math.asin;
    register_un_prim "caml_atan_float" `Pure ~typ:(Number (Float, Unboxed)) Math.atan;
    register_bin_prim
      "caml_atan2_float"
      `Pure
      ~tx:(Number (Float, Unboxed))
      ~ty:(Number (Float, Unboxed))
      Math.atan2;
    register_un_prim "caml_cosh_float" `Pure ~typ:(Number (Float, Unboxed)) Math.cosh;
    register_un_prim "caml_sinh_float" `Pure ~typ:(Number (Float, Unboxed)) Math.sinh;
    register_un_prim "caml_tanh_float" `Pure ~typ:(Number (Float, Unboxed)) Math.tanh;
    register_un_prim "caml_acosh_float" `Pure ~typ:(Number (Float, Unboxed)) Math.acosh;
    register_un_prim "caml_asinh_float" `Pure ~typ:(Number (Float, Unboxed)) Math.asinh;
    register_un_prim "caml_atanh_float" `Pure ~typ:(Number (Float, Unboxed)) Math.atanh;
    register_un_prim "caml_cbrt_float" `Pure ~typ:(Number (Float, Unboxed)) Math.cbrt;
    register_un_prim "caml_exp_float" `Pure ~typ:(Number (Float, Unboxed)) Math.exp;
    register_un_prim "caml_exp2_float" `Pure ~typ:(Number (Float, Unboxed)) Math.exp2;
    register_un_prim "caml_log_float" `Pure ~typ:(Number (Float, Unboxed)) Math.log;
    register_un_prim "caml_expm1_float" `Pure ~typ:(Number (Float, Unboxed)) Math.expm1;
    register_un_prim "caml_log1p_float" `Pure ~typ:(Number (Float, Unboxed)) Math.log1p;
    register_un_prim "caml_log2_float" `Pure ~typ:(Number (Float, Unboxed)) Math.log2;
    register_un_prim "caml_log10_float" `Pure ~typ:(Number (Float, Unboxed)) Math.log10;
    register_bin_prim
      "caml_power_float"
      `Pure
      ~tx:(Number (Float, Unboxed))
      ~ty:(Number (Float, Unboxed))
      Math.power;
    register_bin_prim
      "caml_hypot_float"
      `Pure
      ~tx:(Number (Float, Unboxed))
      ~ty:(Number (Float, Unboxed))
      Math.hypot;
    register_bin_prim
      "caml_fmod_float"
      `Pure
      ~tx:(Number (Float, Unboxed))
      ~ty:(Number (Float, Unboxed))
      Math.fmod;
    register_un_prim
      "caml_int32_bits_of_float"
      `Pure
      ~typ:(Number (Float, Unboxed))
      (fun f ->
        let* f = f in
        return (W.UnOp (I32 ReinterpretF, F32DemoteF64 f)));
    register_un_prim
      "caml_int32_float_of_bits"
      `Pure
      ~typ:(Number (Int32, Unboxed))
      (fun i ->
        let* i = i in
        return (W.F64PromoteF32 (UnOp (F32 ReinterpretI, i))));
    register_un_prim
      "caml_int32_of_float"
      `Pure
      ~typ:(Number (Float, Unboxed))
      (fun f ->
        let* f = f in
        return (W.UnOp (I32 (TruncSatF64 S), f)));
    register_un_prim
      "caml_int32_to_float"
      `Pure
      ~typ:(Number (Int32, Unboxed))
      (fun n ->
        let* n = n in
        return (W.UnOp (F64 (Convert (`I32, S)), n)));
    register_un_prim
      "caml_int32_neg"
      `Pure
      ~typ:(Number (Int32, Unboxed))
      (fun i ->
        let* i = i in
        return (W.BinOp (I32 Sub, Const (I32 0l), i)));
    register_bin_prim
      "caml_int32_add"
      `Pure
      ~tx:(Number (Int32, Unboxed))
      ~ty:(Number (Int32, Unboxed))
      (fun i j -> int32_bin_op Add i j);
    register_bin_prim
      "caml_int32_sub"
      `Pure
      ~tx:(Number (Int32, Unboxed))
      ~ty:(Number (Int32, Unboxed))
      (fun i j -> int32_bin_op Sub i j);
    register_bin_prim
      "caml_int32_mul"
      `Pure
      ~tx:(Number (Int32, Unboxed))
      ~ty:(Number (Int32, Unboxed))
      (fun i j -> int32_bin_op Mul i j);
    register_bin_prim
      "caml_int32_and"
      `Pure
      ~tx:(Number (Int32, Unboxed))
      ~ty:(Number (Int32, Unboxed))
      (fun i j -> int32_bin_op And i j);
    register_bin_prim
      "caml_int32_or"
      `Pure
      ~tx:(Number (Int32, Unboxed))
      ~ty:(Number (Int32, Unboxed))
      (fun i j -> int32_bin_op Or i j);
    register_bin_prim
      "caml_int32_xor"
      `Pure
      ~tx:(Number (Int32, Unboxed))
      ~ty:(Number (Int32, Unboxed))
      (fun i j -> int32_bin_op Xor i j);
    register_bin_prim_ctx
      "caml_int32_div"
      ~tx:(Number (Int32, Unboxed))
      ~ty:(Number (Int32, Unboxed))
      (fun context i j ->
        let res = Var.fresh () in
        (*ZZZ Can we do better?*)
        let i' = Var.fresh () in
        let j' = Var.fresh () in
        seq
          (let* () = store ~typ:I32 j' j in
           let* () =
             let* j = load j' in
             instr (W.Br_if (label_index context zero_divide_pc, W.UnOp (I32 Eqz, j)))
           in
           let* () = store ~typ:I32 i' i in
           if_
             { params = []; result = [] }
             Arith.(
               (let* j = load j' in
                return (W.BinOp (I32 Eq, j, Const (I32 (-1l)))))
               land let* i = load i' in
                    return (W.BinOp (I32 Eq, i, Const (I32 Int32.min_int))))
             (store ~always:true ~typ:I32 res (return (W.Const (I32 Int32.min_int))))
             (store
                ~always:true
                ~typ:I32
                res
                (let* i = load i' in
                 let* j = load j' in
                 return (W.BinOp (I32 (Div S), i, j)))))
          (load res));
    register_bin_prim_ctx
      "caml_int32_mod"
      ~tx:(Number (Int32, Unboxed))
      ~ty:(Number (Int32, Unboxed))
      (fun context i j ->
        let j' = Var.fresh () in
        seq
          (let* () = store ~typ:I32 j' j in
           let* j = load j' in
           instr (W.Br_if (label_index context zero_divide_pc, W.UnOp (I32 Eqz, j))))
          (let* i = i in
           let* j = load j' in
           return (W.BinOp (I32 (Rem S), i, j))));
    register_bin_prim
      "caml_int32_shift_left"
      `Pure
      ~tx:(Number (Int32, Unboxed))
      ~ty:(Int Unnormalized)
      (fun i j -> int32_bin_op Shl i j);
    register_bin_prim
      "caml_int32_shift_right"
      `Pure
      ~tx:(Number (Int32, Unboxed))
      ~ty:(Int Unnormalized)
      (fun i j -> int32_bin_op (Shr S) i j);
    register_bin_prim
      "caml_int32_shift_right_unsigned"
      `Pure
      ~tx:(Number (Int32, Unboxed))
      ~ty:(Int Unnormalized)
      (fun i j -> int32_bin_op (Shr U) i j);
    register_un_prim "caml_int32_to_int" `Pure ~typ:(Number (Int32, Unboxed)) (fun i -> i);
    register_un_prim "caml_int32_of_int" `Pure ~typ:(Int Normalized) (fun i -> i);
    register_un_prim
      "caml_nativeint_of_int32"
      `Pure
      ~typ:(Number (Int32, Unboxed))
      (fun i -> i);
    register_un_prim
      "caml_nativeint_to_int32"
      `Pure
      ~typ:(Number (Nativeint, Unboxed))
      (fun i -> i);
    register_un_prim
      "caml_int64_bits_of_float"
      `Pure
      ~typ:(Number (Float, Unboxed))
      (fun f ->
        let* f = f in
        return (W.UnOp (I64 ReinterpretF, f)));
    register_un_prim
      "caml_int64_float_of_bits"
      `Pure
      ~typ:(Number (Int64, Unboxed))
      (fun i ->
        let* i = i in
        return (W.UnOp (F64 ReinterpretI, i)));
    register_un_prim
      "caml_int64_of_float"
      `Pure
      ~typ:(Number (Float, Unboxed))
      (fun f ->
        let* f = f in
        return (W.UnOp (I64 (TruncSatF64 S), f)));
    register_un_prim
      "caml_int64_to_float"
      `Pure
      ~typ:(Number (Int64, Unboxed))
      (fun n ->
        let* n = n in
        return (W.UnOp (F64 (Convert (`I64, S)), n)));
    register_un_prim
      "caml_int64_neg"
      `Pure
      ~typ:(Number (Int64, Unboxed))
      (fun i ->
        let* i = i in
        return (W.BinOp (I64 Sub, Const (I64 0L), i)));
    register_bin_prim
      "caml_int64_add"
      `Pure
      ~tx:(Number (Int64, Unboxed))
      ~ty:(Number (Int64, Unboxed))
      (fun i j -> int64_bin_op Add i j);
    register_bin_prim
      "caml_int64_sub"
      `Pure
      ~tx:(Number (Int64, Unboxed))
      ~ty:(Number (Int64, Unboxed))
      (fun i j -> int64_bin_op Sub i j);
    register_bin_prim
      "caml_int64_mul"
      `Pure
      ~tx:(Number (Int64, Unboxed))
      ~ty:(Number (Int64, Unboxed))
      (fun i j -> int64_bin_op Mul i j);
    register_bin_prim
      "caml_int64_and"
      `Pure
      ~tx:(Number (Int64, Unboxed))
      ~ty:(Number (Int64, Unboxed))
      (fun i j -> int64_bin_op And i j);
    register_bin_prim
      "caml_int64_or"
      `Pure
      ~tx:(Number (Int64, Unboxed))
      ~ty:(Number (Int64, Unboxed))
      (fun i j -> int64_bin_op Or i j);
    register_bin_prim
      "caml_int64_xor"
      `Pure
      ~tx:(Number (Int64, Unboxed))
      ~ty:(Number (Int64, Unboxed))
      (fun i j -> int64_bin_op Xor i j);
    register_bin_prim_ctx
      "caml_int64_div"
      ~tx:(Number (Int64, Unboxed))
      ~ty:(Number (Int64, Unboxed))
      (fun context i j ->
        let res = Var.fresh () in
        (*ZZZ Can we do better?*)
        let i' = Var.fresh () in
        let j' = Var.fresh () in
        seq
          (let* () = store ~typ:I64 j' j in
           let* () =
             let* j = load j' in
             instr (W.Br_if (label_index context zero_divide_pc, W.UnOp (I64 Eqz, j)))
           in
           let* () = store ~typ:I64 i' i in
           if_
             { params = []; result = [] }
             Arith.(
               (let* j = load j' in
                return (W.BinOp (I64 Eq, j, Const (I64 (-1L)))))
               land let* i = load i' in
                    return (W.BinOp (I64 Eq, i, Const (I64 Int64.min_int))))
             (store ~always:true ~typ:I64 res (return (W.Const (I64 Int64.min_int))))
             (store
                ~always:true
                ~typ:I64
                res
                (let* i = load i' in
                 let* j = load j' in
                 return (W.BinOp (I64 (Div S), i, j)))))
          (load res));
    register_bin_prim_ctx
      "caml_int64_mod"
      ~tx:(Number (Int64, Unboxed))
      ~ty:(Number (Int64, Unboxed))
      (fun context i j ->
        let j' = Var.fresh () in
        seq
          (let* () = store ~typ:I64 j' j in
           let* j = load j' in
           instr (W.Br_if (label_index context zero_divide_pc, W.UnOp (I64 Eqz, j))))
          (let* i = i in
           let* j = load j' in
           return (W.BinOp (I64 (Rem S), i, j))));
    register_bin_prim
      "caml_int64_shift_left"
      `Pure
      ~tx:(Number (Int64, Unboxed))
      ~ty:(Int Unnormalized)
      (fun i j -> int64_shift_op Shl i j);
    register_bin_prim
      "caml_int64_shift_right"
      `Pure
      ~tx:(Number (Int64, Unboxed))
      ~ty:(Int Unnormalized)
      (fun i j -> int64_shift_op (Shr S) i j);
    register_bin_prim
      "caml_int64_shift_right_unsigned"
      ~tx:(Number (Int64, Unboxed))
      ~ty:(Int Unnormalized)
      `Pure
      (fun i j -> int64_shift_op (Shr U) i j);
    register_un_prim
      "caml_int64_to_int"
      `Pure
      ~typ:(Number (Int64, Unboxed))
      (fun i ->
        let* i = i in
        return (W.I32WrapI64 i));
    register_un_prim "caml_int64_of_int" `Pure ~typ:(Int Normalized) (fun i ->
        let* i = i in
        return
          (match i with
          | Const (I32 i) -> W.Const (I64 (Int64.of_int32 i))
          | _ -> W.I64ExtendI32 (S, i)));
    register_un_prim
      "caml_int64_to_int32"
      `Pure
      ~typ:(Number (Int64, Unboxed))
      (fun i ->
        let* i = i in
        return (W.I32WrapI64 i));
    register_un_prim
      "caml_int64_of_int32"
      `Pure
      ~typ:(Number (Int32, Unboxed))
      (fun i ->
        let* i = i in
        return (W.I64ExtendI32 (S, i)));
    register_un_prim
      "caml_int64_to_nativeint"
      `Pure
      ~typ:(Number (Int64, Unboxed))
      (fun i ->
        let* i = i in
        return (W.I32WrapI64 i));
    register_un_prim
      "caml_int64_of_nativeint"
      `Pure
      ~typ:(Number (Nativeint, Unboxed))
      (fun i ->
        let* i = i in
        return (W.I64ExtendI32 (S, i)));
    register_un_prim
      "caml_nativeint_bits_of_float"
      `Pure
      ~typ:(Number (Float, Unboxed))
      (fun f ->
        let* f = f in
        return (W.UnOp (I32 ReinterpretF, F32DemoteF64 f)));
    register_un_prim
      "caml_nativeint_float_of_bits"
      `Pure
      ~typ:(Number (Nativeint, Unboxed))
      (fun i ->
        let* i = i in
        return (W.F64PromoteF32 (UnOp (I32 ReinterpretF, i))));
    register_un_prim
      "caml_nativeint_of_float"
      `Pure
      ~typ:(Number (Float, Unboxed))
      (fun f ->
        let* f = f in
        return (W.UnOp (I32 (TruncSatF64 S), f)));
    register_un_prim
      "caml_nativeint_to_float"
      `Pure
      ~typ:(Number (Nativeint, Unboxed))
      (fun n ->
        let* n = n in
        return (W.UnOp (F64 (Convert (`I32, S)), n)));
    register_un_prim
      "caml_nativeint_neg"
      `Pure
      ~typ:(Number (Nativeint, Unboxed))
      (fun i ->
        let* i = i in
        return (W.BinOp (I32 Sub, Const (I32 0l), i)));
    register_bin_prim
      "caml_nativeint_add"
      `Pure
      ~tx:(Number (Nativeint, Unboxed))
      ~ty:(Number (Nativeint, Unboxed))
      (fun i j -> nativeint_bin_op Add i j);
    register_bin_prim
      "caml_nativeint_sub"
      `Pure
      ~tx:(Number (Nativeint, Unboxed))
      ~ty:(Number (Nativeint, Unboxed))
      (fun i j -> nativeint_bin_op Sub i j);
    register_bin_prim
      "caml_nativeint_mul"
      `Pure
      ~tx:(Number (Nativeint, Unboxed))
      ~ty:(Number (Nativeint, Unboxed))
      (fun i j -> nativeint_bin_op Mul i j);
    register_bin_prim
      "caml_nativeint_and"
      `Pure
      ~tx:(Number (Nativeint, Unboxed))
      ~ty:(Number (Nativeint, Unboxed))
      (fun i j -> nativeint_bin_op And i j);
    register_bin_prim
      "caml_nativeint_or"
      `Pure
      ~tx:(Number (Nativeint, Unboxed))
      ~ty:(Number (Nativeint, Unboxed))
      (fun i j -> nativeint_bin_op Or i j);
    register_bin_prim
      "caml_nativeint_xor"
      `Pure
      ~tx:(Number (Nativeint, Unboxed))
      ~ty:(Number (Nativeint, Unboxed))
      (fun i j -> nativeint_bin_op Xor i j);
    register_bin_prim_ctx
      "caml_nativeint_div"
      ~tx:(Number (Nativeint, Unboxed))
      ~ty:(Number (Nativeint, Unboxed))
      (fun context i j ->
        let res = Var.fresh () in
        (*ZZZ Can we do better?*)
        let i' = Var.fresh () in
        let j' = Var.fresh () in
        seq
          (let* () = store ~typ:I32 j' j in
           let* () =
             let* j = load j' in
             instr (W.Br_if (label_index context zero_divide_pc, W.UnOp (I32 Eqz, j)))
           in
           let* () = store ~typ:I32 i' i in
           if_
             { params = []; result = [] }
             Arith.(
               (let* j = load j' in
                return (W.BinOp (I32 Eq, j, Const (I32 (-1l)))))
               land let* i = load i' in
                    return (W.BinOp (I32 Eq, i, Const (I32 Int32.min_int))))
             (store ~always:true ~typ:I32 res (return (W.Const (I32 Int32.min_int))))
             (store
                ~always:true
                ~typ:I32
                res
                (let* i = load i' in
                 let* j = load j' in
                 return (W.BinOp (I32 (Div S), i, j)))))
          (load res));
    register_bin_prim_ctx
      "caml_nativeint_mod"
      ~tx:(Number (Nativeint, Unboxed))
      ~ty:(Number (Nativeint, Unboxed))
      (fun context i j ->
        let j' = Var.fresh () in
        seq
          (let* () = store ~typ:I32 j' j in
           let* j = load j' in
           instr (W.Br_if (label_index context zero_divide_pc, W.UnOp (I32 Eqz, j))))
          (let* i = i in
           let* j = load j' in
           return (W.BinOp (I32 (Rem S), i, j))));
    register_bin_prim
      "caml_nativeint_shift_left"
      `Pure
      ~tx:(Number (Nativeint, Unboxed))
      ~ty:(Int Unnormalized)
      (fun i j -> nativeint_bin_op Shl i j);
    register_bin_prim
      "caml_nativeint_shift_right"
      `Pure
      ~tx:(Number (Nativeint, Unboxed))
      ~ty:(Int Unnormalized)
      (fun i j -> nativeint_bin_op (Shr S) i j);
    register_bin_prim
      "caml_nativeint_shift_right_unsigned"
      `Pure
      ~tx:(Number (Nativeint, Unboxed))
      ~ty:(Int Unnormalized)
      (fun i j -> nativeint_bin_op (Shr U) i j);
    register_un_prim
      "caml_nativeint_to_int"
      `Pure
      ~typ:(Number (Nativeint, Unboxed))
      (fun i -> i);
    register_un_prim "caml_nativeint_of_int" `Pure ~typ:(Int Normalized) (fun i -> i);
    register_bin_prim
      "caml_int_compare"
      `Pure
      ~tx:(Int Normalized)
      ~ty:(Int Normalized)
      (fun i j -> Arith.((j < i) - (i < j)));
    register_prim "%js_array" `Pure (fun ctx _ l ->
        Memory.allocate ~tag:0 (expression_list (fun x -> transl_prim_arg ctx x) l));
    register_comparison
      "caml_greaterthan"
      (fun ctx x y -> translate_int_comparison ctx (fun y x -> Arith.(x < y)) x y)
      (Gt S)
      Gt;
    register_comparison
      "caml_greaterequal"
      (fun ctx x y -> translate_int_comparison ctx (fun y x -> Arith.(x <= y)) x y)
      (Ge S)
      Ge;
    register_comparison
      "caml_lessthan"
      (fun ctx x y -> translate_int_comparison ctx Arith.( < ) x y)
      (Lt S)
      Lt;
    register_comparison
      "caml_lessequal"
      (fun ctx x y -> translate_int_comparison ctx Arith.( <= ) x y)
      (Le S)
      Le;
    register_comparison
      "caml_equal"
      (fun ctx x y -> translate_int_equality ctx ~negate:false x y)
      Eq
      Eq;
    register_comparison
      "caml_notequal"
      (fun ctx x y -> translate_int_equality ctx ~negate:true x y)
      Ne
      Ne;
    register_prim "caml_compare" `Mutator (fun ctx _ l ->
        match l with
        | [ x; y ] -> (
            match get_type ctx x, get_type ctx y with
            | Int _, Int _ ->
                let x' = transl_prim_arg ctx ~typ:(Int Normalized) x in
                let y' = transl_prim_arg ctx ~typ:(Int Normalized) y in
                Arith.((y' < x') - (x' < y'))
            | Number (Int32, _), Number (Int32, _)
            | Number (Nativeint, _), Number (Nativeint, _) ->
                let* f =
                  register_import
                    ~name:"caml_int32_compare"
                    (Fun { W.params = [ I32; I32 ]; result = [ I32 ] })
                in
                let* x' = transl_prim_arg ctx ~typ:(Number (Int32, Unboxed)) x in
                let* y' = transl_prim_arg ctx ~typ:(Number (Int32, Unboxed)) y in
                return (W.Call (f, [ x'; y' ]))
            | Number (Int64, _), Number (Int64, _) ->
                let* f =
                  register_import
                    ~name:"caml_int64_compare"
                    (Fun { W.params = [ I64; I64 ]; result = [ I32 ] })
                in
                let* x' = transl_prim_arg ctx ~typ:(Number (Int64, Unboxed)) x in
                let* y' = transl_prim_arg ctx ~typ:(Number (Int64, Unboxed)) y in
                return (W.Call (f, [ x'; y' ]))
            | Number (Float, _), Number (Float, _) ->
                let* f =
                  register_import
                    ~name:"caml_float_compare"
                    (Fun { W.params = [ F64; F64 ]; result = [ I32 ] })
                in
                let* x' = transl_prim_arg ctx ~typ:(Number (Float, Unboxed)) x in
                let* y' = transl_prim_arg ctx ~typ:(Number (Float, Unboxed)) y in
                return (W.Call (f, [ x'; y' ]))
            | _ ->
                let* f =
                  register_import
                    ~name:"caml_compare"
                    (Fun { W.params = [ Type.value; Type.value ]; result = [ I32 ] })
                in
                let* x' = transl_prim_arg ctx x in
                let* y' = transl_prim_arg ctx y in
                return (W.Call (f, [ x'; y' ])))
        | _ -> invalid_arity "caml_compare" l ~expected:2);
    let bigarray_generic_access ~ctx ta indices =
      match
        ( get_type ctx ta
        , match indices with
          | Pv indices -> Some (indices, ctx.global_flow_info.info_defs.(Var.idx indices))
          | Pc _ -> None )
      with
      | Bigarray { kind; layout }, Some (indices, Expr (Block (_, l, _, _))) ->
          Some
            ( kind
            , layout
            , List.mapi
                ~f:(fun i _ ->
                  Value.int_val
                    (Memory.array_get (load indices) (Arith.const (Int32.of_int (i + 1)))))
                (Array.to_list l) )
      | _, None | _, Some (_, (Expr _ | Phi _)) -> None
    in
    let caml_ba_get ~ctx ~context ~kind ~layout ta indices =
      let ta' = transl_prim_arg ctx ta in
      Bigarray.get
        ~bound_error_index:(label_index context bound_error_pc)
        ~kind
        ~layout
        ta'
        ~indices
    in
    let caml_ba_get_n ~ctx ~context ta indices =
      match get_type ctx ta with
      | Bigarray { kind; layout } ->
          let indices =
            List.map ~f:(fun i -> transl_prim_arg ctx ~typ:(Int Normalized) i) indices
          in
          caml_ba_get ~ctx ~context ~kind ~layout ta indices
      | _ ->
          let n = List.length indices in
          let* f =
            register_import
              ~name:(Printf.sprintf "caml_ba_get_%d" n)
              (Fun (Type.primitive_type (n + 1)))
          in
          let* ta' = transl_prim_arg ctx ta in
          let* indices' = expression_list (transl_prim_arg ctx) indices in
          return (W.Call (f, ta' :: indices'))
    in
    register_prim "caml_ba_get_1" `Mutator (fun ctx context l ->
        match l with
        | [ ta; i ] -> caml_ba_get_n ~ctx ~context ta [ i ]
        | _ -> invalid_arity "caml_ba_get_1" l ~expected:2);
    register_prim "caml_ba_get_2" `Mutator (fun ctx context l ->
        match l with
        | [ ta; i; j ] -> caml_ba_get_n ~ctx ~context ta [ i; j ]
        | _ -> invalid_arity "caml_ba_get_2" l ~expected:3);
    register_prim "caml_ba_get_3" `Mutator (fun ctx context l ->
        match l with
        | [ ta; i; j; k ] -> caml_ba_get_n ~ctx ~context ta [ i; j; k ]
        | _ -> invalid_arity "caml_ba_get_3" l ~expected:4);
    register_prim "caml_ba_get_generic" `Mutator (fun ctx context l ->
        match l with
        | [ ta; indices ] -> (
            match bigarray_generic_access ~ctx ta indices with
            | Some (kind, layout, indices) ->
                caml_ba_get ~ctx ~context ~kind ~layout ta indices
            | _ ->
                let* f =
                  register_import
                    ~name:"caml_ba_get_generic"
                    (Fun (Type.primitive_type 2))
                in
                let* ta' = transl_prim_arg ctx ta in
                let* indices' = transl_prim_arg ctx indices in
                return (W.Call (f, [ ta'; indices' ])))
        | _ -> invalid_arity "caml_ba_get_generic" l ~expected:2);
    let caml_ba_set ~ctx ~context ~kind ~layout ta indices v =
      let ta' = transl_prim_arg ctx ta in
      let v' = transl_prim_arg ctx ~typ:(Typing.bigarray_element_type kind) v in
      Bigarray.set
        ~bound_error_index:(label_index context bound_error_pc)
        ~kind
        ~layout
        ta'
        ~indices
        v'
    in
    let caml_ba_set_n ~ctx ~context ta indices v =
      match get_type ctx ta with
      | Bigarray { kind; layout } ->
          let indices =
            List.map ~f:(fun i -> transl_prim_arg ctx ~typ:(Int Normalized) i) indices
          in
          caml_ba_set ~ctx ~context ~kind ~layout ta indices v
      | _ ->
          let n = List.length indices in
          let* f =
            register_import
              ~name:(Printf.sprintf "caml_ba_set_%d" n)
              (Fun (Type.primitive_type (n + 2)))
          in
          let* ta' = transl_prim_arg ctx ta in
          let* indices' = expression_list (transl_prim_arg ctx) indices in
          let* v' = transl_prim_arg ctx v in
          return (W.Call (f, ta' :: (indices' @ [ v' ])))
    in
    register_prim "caml_ba_set_1" `Mutator (fun ctx context l ->
        match l with
        | [ ta; i; v ] -> caml_ba_set_n ~ctx ~context ta [ i ] v
        | _ -> invalid_arity "caml_ba_set_1" l ~expected:3);
    register_prim "caml_ba_set_2" `Mutator (fun ctx context l ->
        match l with
        | [ ta; i; j; v ] -> caml_ba_set_n ~ctx ~context ta [ i; j ] v
        | _ -> invalid_arity "caml_ba_set_2" l ~expected:4);
    register_prim "caml_ba_set_3" `Mutator (fun ctx context l ->
        match l with
        | [ ta; i; j; k; v ] -> caml_ba_set_n ~ctx ~context ta [ i; j; k ] v
        | _ -> invalid_arity "caml_ba_set_3" l ~expected:5);
    register_prim "caml_ba_set_generic" `Mutator (fun ctx context l ->
        match l with
        | [ ta; indices; v ] -> (
            match bigarray_generic_access ~ctx ta indices with
            | Some (kind, layout, indices) ->
                caml_ba_set ~ctx ~context ~kind ~layout ta indices v
            | _ ->
                let* f =
                  register_import
                    ~name:"caml_ba_set_generic"
                    (Fun (Type.primitive_type 3))
                in
                let* ta' = transl_prim_arg ctx ta in
                let* indices' = transl_prim_arg ctx indices in
                let* v' = transl_prim_arg ctx v in
                return (W.Call (f, [ ta'; indices'; v' ])))
        | _ -> invalid_arity "caml_ba_set_generic" l ~expected:3)

  let unboxed_type ty : W.value_type option =
    match ty with
    | Typing.Int (Normalized | Unnormalized) | Number ((Int32 | Nativeint), Unboxed) ->
        Some I32
    | Number (Int64, Unboxed) -> Some I64
    | Number (Float, Unboxed) -> Some F64
    | _ -> None

  let box_number_if_needed ctx x e =
    match Typing.var_type ctx.types x with
    | Number (n, Boxed) as into -> convert ~from:(Number (n, Unboxed)) ~into e
    | _ -> e

  let exception_handler_pc = -3

  let direct_call ctx context f args closure =
    let e = W.Call (f, args @ [ closure ]) in
    let e =
      if Var.Hashtbl.mem ctx.raising_funcs f
      then
        let label = label_index context exception_handler_pc in
        W.Br_on_null (label, e)
      else e
    in
    return e

  let rec translate_expr ctx context x e =
    match e with
    | Apply { f; args; exact; _ } ->
        let* closure = load f in
        if exact || List.length args = if Var.Set.mem x ctx.in_cps then 2 else 1
        then
          match
            if exact then Global_flow.get_unique_closure ctx.global_flow_info f else None
          with
          | Some (g, params) ->
              let* cl =
                (* Functions with constant closures ignore their environment. *)
                match closure with
                | GlobalGet global ->
                    let* init = get_global global in
                    if Option.is_some init then Value.unit else return closure
                | _ -> return closure
              in
              let* args =
                expression_list
                  Fun.id
                  (List.map2
                     ~f:(fun a p ->
                       convert
                         ~from:(Typing.var_type ctx.types a)
                         ~into:(Typing.var_type ctx.types p)
                         (load a))
                     args
                     params)
              in
              convert
                ~from:(Typing.return_type ctx.types g)
                ~into:(Typing.var_type ctx.types x)
                (direct_call ctx context g args cl)
          | None -> (
              let funct = Var.fresh () in
              let* closure = tee funct (return closure) in
              let* ty, funct =
                Memory.load_function_pointer
                  ~cps:(Var.Set.mem x ctx.in_cps)
                  ~arity:(List.length args)
                  (load funct)
              in
              let* args = expression_list (fun x -> load_and_box ctx x) args in
              match funct with
              | W.RefFunc g -> direct_call ctx context g args closure
              | _ -> return (W.Call_ref (ty, funct, args @ [ closure ])))
        else
          let* apply =
            need_apply_fun ~cps:(Var.Set.mem x ctx.in_cps) ~arity:(List.length args)
          in
          let* args = expression_list (fun x -> load_and_box ctx x) args in
          return (W.Call (apply, args @ [ closure ]))
    | Block (tag, a, _, _) ->
        if tag = 254
        then
          Memory.allocate_float_array
            (expression_list
               (fun x ->
                 convert
                   ~from:(Typing.var_type ctx.types x)
                   ~into:(Number (Float, Unboxed))
                   (load x))
               (Array.to_list a))
        else
          Memory.allocate
            ~tag
            (expression_list (fun x -> load_and_box ctx x) (Array.to_list a))
    | Field (y, n, Non_float) -> Memory.field (load_and_box ctx y) n
    | Field (y, n, Float) ->
        Memory.float_array_get
          (load_and_box ctx y)
          (return (W.Const (I32 (Int32.of_int n))))
        |> box_number_if_needed ctx x
    | Closure _ ->
        Closure.translate
          ~context:ctx.global_context
          ~closures:ctx.closures
          ~cps:(Var.Set.mem x ctx.in_cps)
          ~no_code_pointer:(Call_graph_analysis.direct_calls_only ctx.fun_info x)
          x
    | Constant c ->
        Constant.translate
          ~unboxed:
            (match Typing.var_type ctx.types x with
            | Number (_, Unboxed) -> true
            | _ -> false)
          c
    | Special (Alias_prim _) -> assert false
    | Prim (Extern "caml_alloc_dummy_function", [ _; Pc (Int arity) ]) ->
        (* Removed in OCaml 5.2 *)
        Closure.dummy ~cps:(effects_cps ()) ~arity:(Targetint.to_int_exn arity)
    | Prim (Extern "caml_alloc_dummy_infix", _) ->
        Closure.dummy ~cps:(effects_cps ()) ~arity:1
    | Prim (Extern "caml_get_global", [ Pc (String name) ]) ->
        let* x =
          let* context = get_context in
          match
            List.find_map
              ~f:(fun f ->
                match f with
                | W.Global { name = name'; exported_name = Some exported_name; _ }
                  when String.equal exported_name name -> Some name'
                | _ -> None)
              context.other_fields
          with
          | Some x -> return x
          | _ ->
              let* typ = Value.block_type in
              register_import ~import_module:"OCaml" ~name (Global { mut = true; typ })
        in
        return (W.GlobalGet x)
    | Prim (Extern "caml_set_global", [ Pc (String name); v ]) ->
        let v = transl_prim_arg ctx v in
        let x = Var.fresh_n name in
        let* () =
          let* typ = Value.block_type in
          let* dummy = Value.dummy_block in
          register_global x ~exported_name:name { mut = true; typ } dummy
        in
        seq
          (let* v = Value.as_block v in
           instr (W.GlobalSet (x, v)))
          Value.unit
    | Prim (Not, [ x ]) -> Value.not (transl_prim_arg ctx ~typ:(Int Unnormalized) x)
    | Prim (Lt, [ x; y ]) -> translate_int_comparison ctx Arith.( < ) x y
    | Prim (Le, [ x; y ]) -> translate_int_comparison ctx Arith.( <= ) x y
    | Prim (Ult, [ x; y ]) -> translate_int_comparison ctx Arith.ult x y
    | Prim (Eq, [ x; y ]) -> translate_int_equality ctx ~negate:false x y
    | Prim (Neq, [ x; y ]) -> translate_int_equality ctx ~negate:true x y
    | Prim (Array_get, [ x; y ]) ->
        Memory.array_get
          (transl_prim_arg ctx x)
          (transl_prim_arg ctx ~typ:(Int Normalized) y)
    | Prim (Extern "caml_array_unsafe_get", [ x; y ]) ->
        Memory.gen_array_get
          (transl_prim_arg ctx x)
          (transl_prim_arg ctx ~typ:(Int Normalized) y)
    | Prim (p, l) -> (
        match p with
        | Extern name when String.Hashtbl.mem internal_primitives name ->
            snd (String.Hashtbl.find internal_primitives name) ctx context l
            |> box_number_if_needed ctx x
        | Extern name when String.Hashtbl.mem specialized_primitives name ->
            let ((_, arg_typ, _) as typ) =
              String.Hashtbl.find specialized_primitives name
            in
            let* f = register_import ~name (Fun (specialized_primitive_type typ)) in
            let rec loop acc arg_typ l =
              match arg_typ, l with
              | [], [] -> return (W.Call (f, List.rev acc))
              | repr :: rem, x :: r ->
                  let* x =
                    transl_prim_arg
                      ctx
                      ?typ:
                        (match repr with
                        | Value -> None
                        | Float -> Some (Number (Float, Unboxed))
                        | Int -> Some (Int Normalized)
                        | Int32 -> Some (Number (Int32, Unboxed))
                        | Nativeint -> Some (Number (Nativeint, Unboxed))
                        | Int64 -> Some (Number (Int64, Unboxed)))
                      x
                  in
                  loop (x :: acc) rem r
              | [], _ :: _ | _ :: _, [] -> assert false
            in
            loop [] arg_typ l |> box_number_if_needed ctx x
        | _ -> (
            let l = List.map ~f:(fun x -> transl_prim_arg ctx x) l in
            match p, l with
            | Extern name, l ->
                let* f =
                  register_import ~name (Fun (Type.primitive_type (List.length l)))
                in
                let rec loop acc l =
                  match l with
                  | [] -> return (W.Call (f, List.rev acc))
                  | x :: r ->
                      let* x = x in
                      loop (x :: acc) r
                in
                loop [] l
            | IsInt, [ x ] -> Value.is_int x
            | Vectlength, [ x ] -> Memory.gen_array_length x
            | (Not | Lt | Le | Eq | Neq | Ult | Array_get | IsInt | Vectlength), _ ->
                assert false))

  and translate_instr ctx context i =
    match i with
    | Assign (x, y) ->
        assign
          x
          (convert
             ~from:(Typing.var_type ctx.types y)
             ~into:(Typing.var_type ctx.types x)
             (load y))
    | Let (x, e) ->
        if ctx.live.(Var.idx x) = 0
        then drop (translate_expr ctx context x e)
        else
          store
            ?typ:(unboxed_type (Typing.var_type ctx.types x))
            x
            (translate_expr ctx context x e)
    | Set_field (x, n, Non_float, y) ->
        Memory.set_field (load_and_box ctx x) n (load_and_box ctx y)
    | Set_field (x, n, Float, y) ->
        Memory.float_array_set
          (load_and_box ctx x)
          (return (W.Const (I32 (Int32.of_int n))))
          (convert
             ~from:(Typing.var_type ctx.types y)
             ~into:(Number (Float, Unboxed))
             (load y))
    | Offset_ref (x, n) ->
        Memory.set_field
          (load x)
          0
          (Value.val_int
             Arith.(Value.int_val (Memory.field (load x) 0) + const (Int32.of_int n)))
    | Array_set (x, y, z) ->
        Memory.array_set
          (load x)
          (convert ~from:(Typing.var_type ctx.types y) ~into:(Int Normalized) (load y))
          (load_and_box ctx z)
    | Event loc -> event loc

  and translate_instrs ctx context l =
    match l with
    | [] -> return ()
    | i :: rem ->
        let* () = translate_instr ctx context i in
        translate_instrs ctx context rem

  let parallel_renaming ~ctx params args =
    let rec visit visited prev s m x l =
      if not (Var.Set.mem x visited)
      then
        let visited = Var.Set.add x visited in
        let y = Var.Map.find x m in
        if Code.Var.compare x y = 0
        then visited, None, l
        else
          let tx = Typing.var_type ctx.types x in
          let ty = Typing.var_type ctx.types y in
          if Var.Set.mem y prev
          then
            let t = Code.Var.fresh () in
            visited, Some (y, ty, t, tx), (x, tx, t, tx) :: l
          else if Var.Set.mem y s
          then
            let visited, aliases, l = visit visited (Var.Set.add x prev) s m y l in
            match aliases with
            | Some (a, ta, b, tb) when Code.Var.compare a x = 0 ->
                visited, None, (b, tb, a, ta) :: (x, tx, y, ty) :: l
            | _ -> visited, aliases, (x, tx, y, ty) :: l
          else visited, None, (x, tx, y, ty) :: l
      else visited, None, l
    in
    let visit_all params args =
      let m = Subst.build_mapping params args in
      let s = List.fold_left params ~init:Var.Set.empty ~f:(fun s x -> Var.Set.add x s) in
      let _, l =
        Var.Set.fold
          (fun x (visited, l) ->
            let visited, _, l = visit visited Var.Set.empty s m x l in
            visited, l)
          s
          (Var.Set.empty, [])
      in
      l
    in
    let l = visit_all params args in
    List.fold_left
      l
      ~f:(fun continuation (y, ty, x, tx) ->
        let* () = continuation in
        store ~always:true ?typ:(unboxed_type ty) y (convert ~from:tx ~into:ty (load x)))
      ~init:(return ())

  let exception_name = "ocaml_exception"

  let extend_context fall_through context =
    match fall_through with
    | (`Block _ | `Catch | `Skip) as b -> b :: context
    | `Return -> `Skip :: context

  let needed_handlers (p : program) pc =
    Code.traverse
      { fold = fold_children_skip_try_body }
      (fun pc n ->
        let block = Addr.Map.find pc p.blocks in
        List.fold_left
          ~f:(fun n i ->
            match i with
            | Let
                ( _
                , Prim
                    ( Extern
                        ( "caml_string_get"
                        | "caml_bytes_get"
                        | "caml_string_set"
                        | "caml_bytes_set"
                        | "caml_check_bound"
                        | "caml_check_bound_gen"
                        | "caml_check_bound_float"
                        | "caml_ba_get_1"
                        | "caml_ba_get_2"
                        | "caml_ba_get_3"
                        | "caml_ba_get_generic"
                        | "caml_ba_set_1"
                        | "caml_ba_set_2"
                        | "caml_ba_set_3"
                        | "caml_ba_set_generic" )
                    , _ ) ) -> fst n, true
            | Let
                ( _
                , Prim
                    ( Extern
                        ( "%int_div"
                        | "%int_mod"
                        | "caml_int32_div"
                        | "caml_int32_mod"
                        | "caml_int64_div"
                        | "caml_int64_mod"
                        | "caml_nativeint_div"
                        | "caml_nativeint_mod" )
                    , _ ) ) -> true, snd n
            | _ -> n)
          ~init:n
          block.body)
      pc
      p.blocks
      (false, false)

  let wrap_with_handler needed pc handler ~result_typ ~fall_through ~context body =
    if needed
    then
      let* () =
        block
          { params = []; result = [] }
          (body ~result_typ:[] ~fall_through:(`Block pc) ~context:(`Block pc :: context))
      in
      if List.is_empty result_typ
      then handler
      else
        let* () = handler in
        instr W.Unreachable
    else body ~result_typ ~fall_through ~context

  let wrap_with_handlers ~location p pc ~result_typ ~fall_through ~context body =
    let need_zero_divide_handler, need_bound_error_handler = needed_handlers p pc in
    wrap_with_handler
      true
      exception_handler_pc
      (match location with
      | `Toplevel ->
          let* exn =
            register_import
              ~import_module:"env"
              ~name:"caml_exception"
              (Global { mut = true; typ = Type.value })
          in
          let* tag = register_import ~name:exception_name (Tag Type.value) in
          instr (Throw (tag, GlobalGet exn))
      | `Exception_handler ->
          let* exn =
            register_import
              ~import_module:"env"
              ~name:"caml_exception"
              (Global { mut = true; typ = Type.value })
          in
          instr (Br (2, Some (GlobalGet exn)))
      | `Function -> instr (Return (Some (RefNull Any))))
      (wrap_with_handler
         need_bound_error_handler
         bound_error_pc
         (let* f =
            register_import ~name:"caml_bound_error" (Fun { params = []; result = [] })
          in
          instr (CallInstr (f, [])))
         (wrap_with_handler
            need_zero_divide_handler
            zero_divide_pc
            (let* f =
               register_import
                 ~name:"caml_raise_zero_divide"
                 (Fun { params = []; result = [] })
             in
             instr (CallInstr (f, [])))
            body))
      ~result_typ
      ~fall_through
      ~context

  let translate_function
      p
      ctx
      name_opt
      ~toplevel_name
      ~unit_name
      params
      ((pc, _) as cont)
      cloc
      acc =
    let return_type =
      match name_opt with
      | Some f -> Typing.return_type ctx.types f
      | _ -> Typing.Top
    in
    let return_exn =
      match name_opt with
      | Some f -> Var.Hashtbl.mem ctx.raising_funcs f
      | _ -> false
    in
    let g = Structure.build_graph ctx.blocks pc in
    let dom = Structure.dominator_tree g in
    let rec translate_tree result_typ fall_through pc context =
      let block = Addr.Map.find pc ctx.blocks in
      let keep_ouside pc' =
        match block.branch with
        | Switch _ -> true
        | Cond (_, (pc1, _), (pc2, _)) when pc' = pc1 && pc' = pc2 -> true
        | _ -> Structure.is_merge_node g pc'
      in
      let code ~context =
        let block = Addr.Map.find pc ctx.blocks in
        let* () = translate_instrs ctx context block.body in
        translate_node_within
          ~result_typ
          ~fall_through
          ~pc
          ~l:
            (pc
            |> Structure.get_edges dom
            |> Addr.Set.elements
            |> List.filter ~f:keep_ouside
            |> Structure.sort_in_post_order g)
          ~context
      in
      if Structure.is_loop_header g pc
      then
        loop { params = []; result = result_typ } (code ~context:(`Block pc :: context))
      else code ~context
    and translate_node_within ~result_typ ~fall_through ~pc ~l ~context =
      match l with
      | pc' :: rem ->
          let* () =
            let code ~context =
              translate_node_within
                ~result_typ:[]
                ~fall_through:(`Block pc')
                ~pc
                ~l:rem
                ~context
            in
            (* Do not insert a block if the inner code contains a
               structured control flow instruction ([if] or [try] *)
            if
              (not (List.is_empty rem))
              ||
              let block = Addr.Map.find pc ctx.blocks in
              match block.branch with
              | Cond _ | Pushtrap _ -> false (*ZZZ also some Switch*)
              | _ -> true
            then
              block { params = []; result = [] } (code ~context:(`Block pc' :: context))
            else code ~context
          in
          translate_tree result_typ fall_through pc' context
      | [] -> (
          let block = Addr.Map.find pc ctx.blocks in
          let branch = block.branch in
          match branch with
          | Branch cont -> translate_branch result_typ fall_through pc cont context
          | Return x -> (
              let* e =
                convert ~from:(Typing.var_type ctx.types x) ~into:return_type (load x)
              in
              match fall_through with
              | `Return -> instr (Push e)
              | `Block _ | `Catch | `Skip -> instr (Return (Some e)))
          | Cond (x, cont1, cont2) ->
              let context' = extend_context fall_through context in
              if_
                { params = []; result = result_typ }
                (match Typing.var_type ctx.types x with
                | Int Normalized -> load x
                | Int Unnormalized -> Arith.(load x lsl const 1l)
                | _ -> Value.check_is_not_zero (load x))
                (translate_branch result_typ fall_through pc cont1 context')
                (translate_branch result_typ fall_through pc cont2 context')
          | Stop -> (
              let* e = Value.unit in
              match fall_through with
              | `Return -> instr (Push e)
              | `Block _ | `Catch | `Skip -> instr (Return (Some e)))
          | Switch (x, a) ->
              let len = Array.length a in
              let l = Array.to_list (Array.sub a ~pos:0 ~len:(len - 1)) in
              let dest (pc, args) =
                assert (List.is_empty args);
                label_index context pc
              in
              let* e =
                convert
                  ~from:(Typing.var_type ctx.types x)
                  ~into:(Int Normalized)
                  (load x)
              in
              instr (Br_table (e, List.map ~f:dest l, dest a.(len - 1)))
          | Raise (x, _) -> (
              let* e = load x in
              match fall_through with
              | `Catch -> instr (Push e)
              | `Block _ | `Return | `Skip -> (
                  match catch_index context with
                  | Some i -> instr (Br (i, Some e))
                  | None ->
                      if return_exn
                      then
                        let* exn =
                          register_import
                            ~import_module:"env"
                            ~name:"caml_exception"
                            (Global { mut = true; typ = Type.value })
                        in
                        let* () = instr (GlobalSet (exn, e)) in
                        instr (Return (Some (RefNull Any)))
                      else
                        let* tag =
                          register_import ~name:exception_name (Tag Type.value)
                        in
                        instr (Throw (tag, e))))
          | Pushtrap (cont, x, cont') ->
              handle_exceptions
                ~result_typ
                ~fall_through
                ~context:(extend_context fall_through context)
                (wrap_with_handlers
                   ~location:`Exception_handler
                   p
                   (fst cont)
                   (fun ~result_typ ~fall_through ~context ->
                     translate_branch result_typ fall_through pc cont context))
                x
                (fun ~result_typ ~fall_through ~context ->
                  translate_branch result_typ fall_through pc cont' context)
          | Poptrap cont -> translate_branch result_typ fall_through pc cont context)
    and translate_branch result_typ fall_through src (dst, args) context =
      let* () =
        if List.is_empty args
        then return ()
        else
          let block = Addr.Map.find dst ctx.blocks in
          parallel_renaming ~ctx block.params args
      in
      match fall_through with
      | `Block dst' when dst = dst' -> return ()
      | _ ->
          if
            (src >= 0 && Structure.is_backward g src dst) || Structure.is_merge_node g dst
          then instr (Br (label_index context dst, None))
          else translate_tree result_typ fall_through dst context
    in
    let bind_parameters =
      List.fold_left
        ~f:(fun l x ->
          let* _ = l in
          let* _ = add_var ?typ:(unboxed_type (Typing.var_type ctx.types x)) x in
          return ())
        ~init:(return ())
        params
    in
    let build_initial_env =
      let* () = bind_parameters in
      match name_opt with
      | Some f ->
          Closure.bind_environment
            ~context:ctx.global_context
            ~closures:ctx.closures
            ~cps:(Var.Set.mem f ctx.in_cps)
            ~no_code_pointer:(Call_graph_analysis.direct_calls_only ctx.fun_info f)
            f
      | None -> return ()
    in
    (*
  Format.eprintf "=== %d ===@." pc;
*)
    let param_names =
      match name_opt with
      | None -> []
      | Some f -> params @ [ f ]
    in
    let param_count = List.length param_names in
    (match name_opt with
    | None -> ctx.global_context.globalized_variables <- Globalize.f p g ctx.closures
    | Some _ -> ());
    let locals, body =
      function_body
        ~context:ctx.global_context
        ~param_names
        ~body:
          (let* () =
             let block = Addr.Map.find pc ctx.blocks in
             match block.body with
             | Event start_loc :: _ -> event start_loc
             | _ -> no_event
           in
           let* () = build_initial_env in
           let* () =
             wrap_with_handlers
               ~location:(if return_exn then `Function else `Toplevel)
               p
               pc
               ~result_typ:[ Option.value ~default:Type.value (unboxed_type return_type) ]
               ~fall_through:`Return
               ~context:[]
               (fun ~result_typ ~fall_through ~context ->
                 translate_branch result_typ fall_through (-1) cont context)
           in
           match cloc with
           | Some loc -> event loc
           | None -> return ())
    in
    let locals, body = post_process_function_body ~param_names ~locals body in
    W.Function
      { name =
          (match name_opt with
          | None -> toplevel_name
          | Some x -> x)
      ; exported_name =
          (match name_opt with
          | None -> Option.map ~f:(fun name -> name ^ ".init") unit_name
          | Some _ -> None)
      ; typ = None
      ; signature =
          (match name_opt with
          | None -> Type.primitive_type param_count
          | Some f ->
              if Typing.can_unbox_parameters ctx.fun_info f
              then
                { W.params =
                    List.map
                      ~f:(fun x ->
                        Option.value
                          ~default:Type.value
                          (unboxed_type (Typing.var_type ctx.types x)))
                      params
                    @ [ Type.value ]
                ; result =
                    [ Option.value
                        ~default:(if return_exn then Type.value_or_exn else Type.value)
                        (unboxed_type return_type)
                    ]
                }
              else Type.func_type (param_count - 1))
      ; param_names
      ; locals
      ; body
      }
    :: acc

  let init_function ~context ~to_link =
    let name = Code.Var.fresh_n "initialize" in
    let signature = { W.params = []; result = [ Type.value ] } in
    let locals, body =
      function_body
        ~context
        ~param_names:[]
        ~body:
          (List.fold_right
             ~f:(fun name cont ->
               let* f =
                 register_import
                   ~import_module:"OCaml"
                   ~name:(name ^ ".init")
                   (Fun signature)
               in
               let* () = instr (Drop (Call (f, []))) in
               cont)
             ~init:(instr (Push (RefI31 (Const (I32 0l)))))
             to_link)
    in
    context.other_fields <-
      W.Function
        { name
        ; exported_name = None
        ; typ = None
        ; signature
        ; param_names = []
        ; locals
        ; body
        }
      :: context.other_fields;
    name

  let entry_point context toplevel_fun entry_name =
    let signature, param_names, body = entry_point ~toplevel_fun in
    let locals, body = function_body ~context ~param_names ~body in
    W.Function
      { name = Var.fresh_n "entry_point"
      ; exported_name = Some entry_name
      ; typ = None
      ; signature
      ; param_names
      ; locals
      ; body
      }

  module Curry = Curry.Make (Target)

  let add_start_function ~context toplevel_name =
    context.other_fields <-
      entry_point context toplevel_name "_initialize" :: context.other_fields

  let add_init_function ~context ~to_link =
    add_start_function ~context (init_function ~context ~to_link)

  let f
      ~context:global_context
      ~unit_name
      (p : Code.program)
      ~live_vars
      ~in_cps (*
    ~should_export
*)
      ~global_flow_info
      ~fun_info
      ~types
      ~raising_funcs =
    global_context.unit_name <- unit_name;
    let p, closures = Closure_conversion.f p in
    (*
  Code.Print.program (fun _ _ -> "") p;
*)
    let ctx =
      { live = live_vars
      ; in_cps
      ; global_flow_info
      ; fun_info
      ; types
      ; raising_funcs
      ; blocks = p.blocks
      ; closures
      ; global_context
      }
    in
    let toplevel_name = Var.fresh_n "toplevel" in
    let functions =
      Code.fold_closures_outermost_first
        p
        (fun name_opt params cont cloc ->
          translate_function p ctx name_opt ~toplevel_name ~unit_name params cont cloc)
        []
    in
    let functions =
      List.map
        ~f:(fun f ->
          match f with
          | W.Function ({ name; _ } as f) when Code.Var.equal name toplevel_name ->
              W.Function { f with body = global_context.init_code @ f.body }
          | _ -> f)
        functions
    in
    global_context.init_code <- [];
    global_context.other_fields <- List.rev_append functions global_context.other_fields;
    let js_code = StringMap.bindings global_context.fragments in
    global_context.fragments <- StringMap.empty;
    Curry.f ~context:global_context;
    toplevel_name, js_code

  let output ~context =
    let imports =
      List.concat
        (List.map
           ~f:(fun (import_module, m) ->
             List.map
               ~f:(fun (import_name, (name, desc)) ->
                 W.Import { import_module; import_name; name; desc })
               (StringMap.bindings m))
           (StringMap.bindings context.imports))
    in
    let constant_data =
      List.map
        ~f:(fun (name, contents) -> W.Data { name; contents })
        (Var.Map.bindings context.data_segments)
    in
    List.rev_append context.other_fields (imports @ constant_data)

  let init () =
    Primitive.register "caml_make_array" `Mutable None None;
    Primitive.register "caml_array_of_uniform_array" `Mutable None None;
    String.Hashtbl.iter
      (fun name (k, _) -> Primitive.register name k None None)
      internal_primitives;
    String.Hashtbl.iter
      (fun name (k, _, _) -> Primitive.register name k None None)
      specialized_primitives
end

(* Make sure we can use [br_table] for switches *)
let fix_switch_branches p =
  let p' = ref p in
  let updates = ref Addr.Map.empty in
  let fix_branches l =
    Array.iteri
      ~f:(fun i ((pc, args) as cont) ->
        if not (List.is_empty args)
        then
          l.(i) <-
            ( (let l = try Addr.Map.find pc !updates with Not_found -> [] in
               match
                 List.find_map
                   ~f:(fun (args', pc') ->
                     if List.equal ~eq:Var.equal args' args then Some pc' else None)
                   l
               with
               | Some x -> x
               | None ->
                   let pc' = !p'.free_pc in
                   p' :=
                     { !p' with
                       blocks =
                         Addr.Map.add
                           pc'
                           { params = []; body = []; branch = Branch cont }
                           !p'.blocks
                     ; free_pc = pc' + 1
                     };
                   updates := Addr.Map.add pc ((args, pc') :: l) !updates;
                   pc')
            , [] ))
      l
  in
  Addr.Map.iter
    (fun _ block ->
      match block.branch with
      | Switch (_, l) -> fix_branches l
      | _ -> ())
    p.blocks;
  !p'

module G = Generate (Gc_target)

let init = G.init

let start () = make_context ~value_type:Gc_target.Type.value

let f ~context ~unit_name p ~live_vars ~in_cps ~deadcode_sentinal ~global_flow_data =
  let global_flow_state, global_flow_info = global_flow_data in
  let fun_info = Call_graph_analysis.f p global_flow_info in
  let types =
    Typing.f ~global_flow_state ~global_flow_info ~fun_info ~deadcode_sentinal p
  in
  let raising_funcs =
    Call_graph_analysis.raising_functions p global_flow_info fun_info (fun f ->
        match Typing.return_type types f with
        | Int (Normalized | Unnormalized) | Number (_, Unboxed) -> false
        | Int Ref | Number (_, Boxed) | Top | Bot | Tuple _ | Bigarray _ -> true)
  in
  let t = Timer.make () in
  let p = Structure.norm p in
  let p = fix_switch_branches p in
  let res =
    G.f
      ~context
      ~unit_name
      ~live_vars
      ~in_cps
      ~global_flow_info
      ~fun_info
      ~types
      ~raising_funcs
      p
  in
  if times () then Format.eprintf "  code gen.: %a@." Timer.print t;
  res

let add_start_function = G.add_start_function

let add_init_function = G.add_init_function

let output ch ~context =
  let t = Timer.make () in
  let fields = G.output ~context in
  if times () then Format.eprintf "    fields: %a@." Timer.print t;
  Wat_output.f ch fields;
  if times () then Format.eprintf "  output: %a@." Timer.print t

let wasm_output ch ~opt_source_map_file ~context =
  let t = Timer.make () in
  let fields = G.output ~context in
  if times () then Format.eprintf "    fields: %a@." Timer.print t;
  Wasm_output.f ch ~opt_source_map_file fields;
  if times () then Format.eprintf "  output: %a@." Timer.print t
