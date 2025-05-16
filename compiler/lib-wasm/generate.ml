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
  | `Cps | `Double_translation -> true
  | `Jspi -> false
  | `Disabled -> assert false

module Generate (Target : Target_sig.S) = struct
  open Target

  type ctx =
    { live : int array
    ; in_cps : Effects.in_cps
    ; deadcode_sentinal : Var.t
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
    | Int32
    | Nativeint
    | Int64

  let repr_type r =
    match r with
    | Value -> Type.value
    | Float -> F64
    | Int32 -> I32
    | Nativeint -> I32
    | Int64 -> I64

  let specialized_primitive_type (_, params, result) =
    { W.params = List.map ~f:repr_type params; result = [ repr_type result ] }

  let box_value r e =
    match r with
    | Value -> e
    | Float -> Memory.box_float e
    | Int32 -> Memory.box_int32 e
    | Nativeint -> Memory.box_nativeint e
    | Int64 -> Memory.box_int64 e

  let unbox_value r e =
    match r with
    | Value -> e
    | Float -> Memory.unbox_float e
    | Int32 -> Memory.unbox_int32 e
    | Nativeint -> Memory.unbox_nativeint e
    | Int64 -> Memory.unbox_int64 e

  let specialized_primitives =
    let h = String.Hashtbl.create 18 in
    List.iter
      ~f:(fun (nm, typ) -> String.Hashtbl.add h nm typ)
      [ "caml_int32_bswap", (`Pure, [ Int32 ], Int32)
      ; "caml_nativeint_bswap", (`Pure, [ Nativeint ], Nativeint)
      ; "caml_int64_bswap", (`Pure, [ Int64 ], Int64)
      ; "caml_int32_compare", (`Pure, [ Int32; Int32 ], Value)
      ; "caml_nativeint_compare", (`Pure, [ Nativeint; Nativeint ], Value)
      ; "caml_int64_compare", (`Pure, [ Int64; Int64 ], Value)
      ; "caml_string_get32", (`Mutator, [ Value; Value ], Int32)
      ; "caml_string_get64", (`Mutator, [ Value; Value ], Int64)
      ; "caml_bytes_get32", (`Mutator, [ Value; Value ], Int32)
      ; "caml_bytes_get64", (`Mutator, [ Value; Value ], Int64)
      ; "caml_bytes_set32", (`Mutator, [ Value; Value; Int32 ], Value)
      ; "caml_bytes_set64", (`Mutator, [ Value; Value; Int64 ], Value)
      ; "caml_lxm_next", (`Pure, [ Value ], Int64)
      ; "caml_ba_uint8_get32", (`Mutator, [ Value; Value ], Int32)
      ; "caml_ba_uint8_get64", (`Mutator, [ Value; Value ], Int64)
      ; "caml_ba_uint8_set32", (`Mutator, [ Value; Value; Int32 ], Value)
      ; "caml_ba_uint8_set64", (`Mutator, [ Value; Value; Int64 ], Value)
      ; "caml_nextafter_float", (`Pure, [ Float; Float ], Float)
      ; "caml_classify_float", (`Pure, [ Float ], Value)
      ; "caml_ldexp_float", (`Pure, [ Float; Value ], Float)
      ; "caml_erf_float", (`Pure, [ Float ], Float)
      ; "caml_erfc_float", (`Pure, [ Float ], Float)
      ; "caml_float_compare", (`Pure, [ Float; Float ], Value)
      ];
    h

  let float_bin_op' op f g =
    Memory.box_float (op (Memory.unbox_float f) (Memory.unbox_float g))

  let float_bin_op op f g =
    let* f = Memory.unbox_float f in
    let* g = Memory.unbox_float g in
    Memory.box_float (return (W.BinOp (F64 op, f, g)))

  let float_un_op' op f = Memory.box_float (op (Memory.unbox_float f))

  let float_un_op op f =
    let* f = Memory.unbox_float f in
    Memory.box_float (return (W.UnOp (F64 op, f)))

  let float_comparison op f g =
    let* f = Memory.unbox_float f in
    let* g = Memory.unbox_float g in
    Value.val_int (return (W.BinOp (F64 op, f, g)))

  let int32_bin_op op f g =
    let* f = Memory.unbox_int32 f in
    let* g = Memory.unbox_int32 g in
    Memory.box_int32 (return (W.BinOp (I32 op, f, g)))

  let int32_shift_op op f g =
    let* f = Memory.unbox_int32 f in
    let* g = Value.int_val g in
    Memory.box_int32 (return (W.BinOp (I32 op, f, g)))

  let int64_bin_op op f g =
    let* f = Memory.unbox_int64 f in
    let* g = Memory.unbox_int64 g in
    Memory.box_int64 (return (W.BinOp (I64 op, f, g)))

  let int64_shift_op op f g =
    let* f = Memory.unbox_int64 f in
    let* g = Value.int_val g in
    Memory.box_int64 (return (W.BinOp (I64 op, f, I64ExtendI32 (S, g))))

  let nativeint_bin_op op f g =
    let* f = Memory.unbox_nativeint f in
    let* g = Memory.unbox_nativeint g in
    Memory.box_nativeint (return (W.BinOp (I32 op, f, g)))

  let nativeint_shift_op op f g =
    let* f = Memory.unbox_nativeint f in
    let* g = Value.int_val g in
    Memory.box_nativeint (return (W.BinOp (I32 op, f, g)))

  let transl_prim_arg x =
    match x with
    | Pv x -> load x
    | Pc c -> Constant.translate c

  let internal_primitives =
    let h = String.Hashtbl.create 128 in
    List.iter
      ~f:(fun (nm, k, f) ->
        String.Hashtbl.add h nm (k, fun _ _ transl_prim_arg l -> f transl_prim_arg l))
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

  let register_un_prim name k f =
    register_prim name k (fun _ _ transl_prim_arg l ->
        match l with
        | [ x ] -> f (transl_prim_arg x)
        | l -> invalid_arity name l ~expected:1)

  let register_bin_prim name k f =
    register_prim name k (fun _ _ transl_prim_arg l ->
        match l with
        | [ x; y ] -> f (transl_prim_arg x) (transl_prim_arg y)
        | _ -> invalid_arity name l ~expected:2)

  let register_bin_prim_ctx name f =
    register_prim name `Mutator (fun _ context transl_prim_arg l ->
        match l with
        | [ x; y ] -> f context (transl_prim_arg x) (transl_prim_arg y)
        | _ -> invalid_arity name l ~expected:2)

  let register_tern_prim name f =
    register_prim name `Mutator (fun _ _ transl_prim_arg l ->
        match l with
        | [ x; y; z ] -> f (transl_prim_arg x) (transl_prim_arg y) (transl_prim_arg z)
        | _ -> invalid_arity name l ~expected:3)

  let register_tern_prim_ctx name f =
    register_prim name `Mutator (fun _ context transl_prim_arg l ->
        match l with
        | [ x; y; z ] ->
            f context (transl_prim_arg x) (transl_prim_arg y) (transl_prim_arg z)
        | _ -> invalid_arity name l ~expected:3)

  let () =
    register_bin_prim "caml_array_unsafe_get" `Mutable Memory.gen_array_get;
    register_bin_prim "caml_floatarray_unsafe_get" `Mutable Memory.float_array_get;
    register_tern_prim "caml_array_unsafe_set" (fun x y z ->
        seq (Memory.gen_array_set x y z) Value.unit);
    register_tern_prim "caml_array_unsafe_set_addr" (fun x y z ->
        seq (Memory.array_set x y z) Value.unit);
    register_tern_prim "caml_floatarray_unsafe_set" (fun x y z ->
        seq (Memory.float_array_set x y z) Value.unit);
    register_bin_prim "caml_string_unsafe_get" `Pure Memory.bytes_get;
    register_bin_prim "caml_bytes_unsafe_get" `Mutable Memory.bytes_get;
    register_tern_prim "caml_string_unsafe_set" (fun x y z ->
        seq (Memory.bytes_set x y z) Value.unit);
    register_tern_prim "caml_bytes_unsafe_set" (fun x y z ->
        seq (Memory.bytes_set x y z) Value.unit);
    let bytes_get context x y =
      seq
        (let* cond = Arith.uge (Value.int_val y) (Memory.bytes_length x) in
         instr (W.Br_if (label_index context bound_error_pc, cond)))
        (Memory.bytes_get x y)
    in
    register_bin_prim_ctx "caml_string_get" bytes_get;
    register_bin_prim_ctx "caml_bytes_get" bytes_get;
    let bytes_set context x y z =
      seq
        (let* cond = Arith.uge (Value.int_val y) (Memory.bytes_length x) in
         let* () = instr (W.Br_if (label_index context bound_error_pc, cond)) in
         Memory.bytes_set x y z)
        Value.unit
    in
    register_tern_prim_ctx "caml_string_set" bytes_set;
    register_tern_prim_ctx "caml_bytes_set" bytes_set;
    register_un_prim "caml_ml_string_length" `Pure (fun x ->
        Value.val_int (Memory.bytes_length x));
    register_un_prim "caml_ml_bytes_length" `Pure (fun x ->
        Value.val_int (Memory.bytes_length x));
    register_bin_prim "%int_add" `Pure Value.int_add;
    register_bin_prim "%int_sub" `Pure Value.int_sub;
    register_bin_prim "%int_mul" `Pure Value.int_mul;
    register_bin_prim "%direct_int_mul" `Pure Value.int_mul;
    register_bin_prim "%direct_int_div" `Pure Value.int_div;
    register_bin_prim_ctx "%int_div" (fun context x y ->
        seq
          (let* cond = Arith.eqz (Value.int_val y) in
           instr (W.Br_if (label_index context zero_divide_pc, cond)))
          (Value.int_div x y));
    register_bin_prim "%direct_int_mod" `Pure Value.int_mod;
    register_bin_prim_ctx "%int_mod" (fun context x y ->
        seq
          (let* cond = Arith.eqz (Value.int_val y) in
           instr (W.Br_if (label_index context zero_divide_pc, cond)))
          (Value.int_mod x y));
    register_un_prim "%int_neg" `Pure Value.int_neg;
    register_bin_prim "%int_or" `Pure Value.int_or;
    register_bin_prim "%int_and" `Pure Value.int_and;
    register_bin_prim "%int_xor" `Pure Value.int_xor;
    register_bin_prim "%int_lsl" `Pure Value.int_lsl;
    register_bin_prim "%int_lsr" `Pure Value.int_lsr;
    register_bin_prim "%int_asr" `Pure Value.int_asr;
    register_un_prim "%direct_obj_tag" `Pure Memory.tag;
    register_bin_prim_ctx "caml_check_bound" (fun context x y ->
        seq
          (let* cond = Arith.uge (Value.int_val y) (Memory.array_length x) in
           instr (W.Br_if (label_index context bound_error_pc, cond)))
          x);
    register_bin_prim_ctx "caml_check_bound_gen" (fun context x y ->
        seq
          (let* cond = Arith.uge (Value.int_val y) (Memory.gen_array_length x) in
           instr (W.Br_if (label_index context bound_error_pc, cond)))
          x);
    register_bin_prim_ctx "caml_check_bound_float" (fun context x y ->
        seq
          (let a = Code.Var.fresh () in
           let* () = store a x in
           let label = label_index context bound_error_pc in
           (* If this is not a float array, it must be the
                      empty array, and the bound check should fail. *)
           let* cond = Arith.eqz (Memory.check_is_float_array (load a)) in
           let* () = instr (W.Br_if (label, cond)) in
           let* cond = Arith.uge (Value.int_val y) (Memory.float_array_length (load a)) in
           instr (W.Br_if (label, cond)))
          x);
    register_bin_prim "caml_add_float" `Pure (fun f g -> float_bin_op Add f g);
    register_bin_prim "caml_sub_float" `Pure (fun f g -> float_bin_op Sub f g);
    register_bin_prim "caml_mul_float" `Pure (fun f g -> float_bin_op Mul f g);
    register_bin_prim "caml_div_float" `Pure (fun f g -> float_bin_op Div f g);
    register_bin_prim "caml_copysign_float" `Pure (fun f g -> float_bin_op CopySign f g);
    register_un_prim "caml_signbit_float" `Pure (fun f ->
        let* f = Memory.unbox_float f in
        let sign = W.BinOp (F64 CopySign, Const (F64 1.), f) in
        Value.val_int (return (W.BinOp (F64 Lt, sign, Const (F64 0.)))));
    register_un_prim "caml_neg_float" `Pure (fun f -> float_un_op Neg f);
    register_un_prim "caml_abs_float" `Pure (fun f -> float_un_op Abs f);
    register_un_prim "caml_ceil_float" `Pure (fun f -> float_un_op Ceil f);
    register_un_prim "caml_floor_float" `Pure (fun f -> float_un_op Floor f);
    register_un_prim "caml_trunc_float" `Pure (fun f -> float_un_op Trunc f);
    register_un_prim "caml_round_float" `Pure (fun f -> float_un_op' Math.round f);
    register_un_prim "caml_sqrt_float" `Pure (fun f -> float_un_op Sqrt f);
    register_bin_prim "caml_eq_float" `Pure (fun f g -> float_comparison Eq f g);
    register_bin_prim "caml_neq_float" `Pure (fun f g -> float_comparison Ne f g);
    register_bin_prim "caml_ge_float" `Pure (fun f g -> float_comparison Ge f g);
    register_bin_prim "caml_le_float" `Pure (fun f g -> float_comparison Le f g);
    register_bin_prim "caml_gt_float" `Pure (fun f g -> float_comparison Gt f g);
    register_bin_prim "caml_lt_float" `Pure (fun f g -> float_comparison Lt f g);
    register_un_prim "caml_int_of_float" `Pure (fun f ->
        let* f = Memory.unbox_float f in
        Value.val_int (return (W.UnOp (I32 (TruncSatF64 S), f))));
    register_un_prim "caml_float_of_int" `Pure (fun n ->
        let* n = Value.int_val n in
        Memory.box_float (return (W.UnOp (F64 (Convert (`I32, S)), n))));
    register_un_prim "caml_cos_float" `Pure (fun f -> float_un_op' Math.cos f);
    register_un_prim "caml_sin_float" `Pure (fun f -> float_un_op' Math.sin f);
    register_un_prim "caml_tan_float" `Pure (fun f -> float_un_op' Math.tan f);
    register_un_prim "caml_acos_float" `Pure (fun f -> float_un_op' Math.acos f);
    register_un_prim "caml_asin_float" `Pure (fun f -> float_un_op' Math.asin f);
    register_un_prim "caml_atan_float" `Pure (fun f -> float_un_op' Math.atan f);
    register_bin_prim "caml_atan2_float" `Pure (fun f g -> float_bin_op' Math.atan2 f g);
    register_un_prim "caml_cosh_float" `Pure (fun f -> float_un_op' Math.cosh f);
    register_un_prim "caml_sinh_float" `Pure (fun f -> float_un_op' Math.sinh f);
    register_un_prim "caml_tanh_float" `Pure (fun f -> float_un_op' Math.tanh f);
    register_un_prim "caml_acosh_float" `Pure (fun f -> float_un_op' Math.acosh f);
    register_un_prim "caml_asinh_float" `Pure (fun f -> float_un_op' Math.asinh f);
    register_un_prim "caml_atanh_float" `Pure (fun f -> float_un_op' Math.atanh f);
    register_un_prim "caml_cbrt_float" `Pure (fun f -> float_un_op' Math.cbrt f);
    register_un_prim "caml_exp_float" `Pure (fun f -> float_un_op' Math.exp f);
    register_un_prim "caml_exp2_float" `Pure (fun f -> float_un_op' Math.exp2 f);
    register_un_prim "caml_log_float" `Pure (fun f -> float_un_op' Math.log f);
    register_un_prim "caml_expm1_float" `Pure (fun f -> float_un_op' Math.expm1 f);
    register_un_prim "caml_log1p_float" `Pure (fun f -> float_un_op' Math.log1p f);
    register_un_prim "caml_log2_float" `Pure (fun f -> float_un_op' Math.log2 f);
    register_un_prim "caml_log10_float" `Pure (fun f -> float_un_op' Math.log10 f);
    register_bin_prim "caml_power_float" `Pure (fun f g -> float_bin_op' Math.power f g);
    register_bin_prim "caml_hypot_float" `Pure (fun f g -> float_bin_op' Math.hypot f g);
    register_bin_prim "caml_fmod_float" `Pure (fun f g -> float_bin_op' Math.fmod f g);
    register_un_prim "caml_int32_bits_of_float" `Pure (fun f ->
        let* f = Memory.unbox_float f in
        Memory.box_int32 (return (W.UnOp (I32 ReinterpretF, F32DemoteF64 f))));
    register_un_prim "caml_int32_float_of_bits" `Pure (fun i ->
        let* i = Memory.unbox_int32 i in
        Memory.box_float (return (W.F64PromoteF32 (UnOp (F32 ReinterpretI, i)))));
    register_un_prim "caml_int32_of_float" `Pure (fun f ->
        let* f = Memory.unbox_float f in
        Memory.box_int32 (return (W.UnOp (I32 (TruncSatF64 S), f))));
    register_un_prim "caml_int32_to_float" `Pure (fun n ->
        let* n = Memory.unbox_int32 n in
        Memory.box_float (return (W.UnOp (F64 (Convert (`I32, S)), n))));
    register_un_prim "caml_int32_neg" `Pure (fun i ->
        let* i = Memory.unbox_int32 i in
        Memory.box_int32 (return (W.BinOp (I32 Sub, Const (I32 0l), i))));
    register_bin_prim "caml_int32_add" `Pure (fun i j -> int32_bin_op Add i j);
    register_bin_prim "caml_int32_sub" `Pure (fun i j -> int32_bin_op Sub i j);
    register_bin_prim "caml_int32_mul" `Pure (fun i j -> int32_bin_op Mul i j);
    register_bin_prim "caml_int32_and" `Pure (fun i j -> int32_bin_op And i j);
    register_bin_prim "caml_int32_or" `Pure (fun i j -> int32_bin_op Or i j);
    register_bin_prim "caml_int32_xor" `Pure (fun i j -> int32_bin_op Xor i j);
    register_bin_prim_ctx "caml_int32_div" (fun context i j ->
        let res = Var.fresh () in
        (*ZZZ Can we do better?*)
        let i' = Var.fresh () in
        let j' = Var.fresh () in
        seq
          (let* () = store ~typ:I32 j' (Memory.unbox_int32 j) in
           let* () =
             let* j = load j' in
             instr (W.Br_if (label_index context zero_divide_pc, W.UnOp (I32 Eqz, j)))
           in
           let* () = store ~typ:I32 i' (Memory.unbox_int32 i) in
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
          (Memory.box_int32 (load res)));
    register_bin_prim_ctx "caml_int32_mod" (fun context i j ->
        let j' = Var.fresh () in
        seq
          (let* () = store ~typ:I32 j' (Memory.unbox_int32 j) in
           let* j = load j' in
           instr (W.Br_if (label_index context zero_divide_pc, W.UnOp (I32 Eqz, j))))
          (let* i = Memory.unbox_int32 i in
           let* j = load j' in
           Memory.box_int32 (return (W.BinOp (I32 (Rem S), i, j)))));
    register_bin_prim "caml_int32_shift_left" `Pure (fun i j -> int32_shift_op Shl i j);
    register_bin_prim "caml_int32_shift_right" `Pure (fun i j ->
        int32_shift_op (Shr S) i j);
    register_bin_prim "caml_int32_shift_right_unsigned" `Pure (fun i j ->
        int32_shift_op (Shr U) i j);
    register_un_prim "caml_int32_to_int" `Pure (fun i ->
        Value.val_int (Memory.unbox_int32 i));
    register_un_prim "caml_int32_of_int" `Pure (fun i ->
        Memory.box_int32 (Value.int_val i));
    register_un_prim "caml_nativeint_of_int32" `Pure (fun i ->
        Memory.box_nativeint (Memory.unbox_int32 i));
    register_un_prim "caml_nativeint_to_int32" `Pure (fun i ->
        Memory.box_int32 (Memory.unbox_nativeint i));
    register_un_prim "caml_int64_bits_of_float" `Pure (fun f ->
        let* f = Memory.unbox_float f in
        Memory.box_int64 (return (W.UnOp (I64 ReinterpretF, f))));
    register_un_prim "caml_int64_float_of_bits" `Pure (fun i ->
        let* i = Memory.unbox_int64 i in
        Memory.box_float (return (W.UnOp (F64 ReinterpretI, i))));
    register_un_prim "caml_int64_of_float" `Pure (fun f ->
        let* f = Memory.unbox_float f in
        Memory.box_int64 (return (W.UnOp (I64 (TruncSatF64 S), f))));
    register_un_prim "caml_int64_to_float" `Pure (fun n ->
        let* n = Memory.unbox_int64 n in
        Memory.box_float (return (W.UnOp (F64 (Convert (`I64, S)), n))));
    register_un_prim "caml_int64_neg" `Pure (fun i ->
        let* i = Memory.unbox_int64 i in
        Memory.box_int64 (return (W.BinOp (I64 Sub, Const (I64 0L), i))));
    register_bin_prim "caml_int64_add" `Pure (fun i j -> int64_bin_op Add i j);
    register_bin_prim "caml_int64_sub" `Pure (fun i j -> int64_bin_op Sub i j);
    register_bin_prim "caml_int64_mul" `Pure (fun i j -> int64_bin_op Mul i j);
    register_bin_prim "caml_int64_and" `Pure (fun i j -> int64_bin_op And i j);
    register_bin_prim "caml_int64_or" `Pure (fun i j -> int64_bin_op Or i j);
    register_bin_prim "caml_int64_xor" `Pure (fun i j -> int64_bin_op Xor i j);
    register_bin_prim_ctx "caml_int64_div" (fun context i j ->
        let res = Var.fresh () in
        (*ZZZ Can we do better?*)
        let i' = Var.fresh () in
        let j' = Var.fresh () in
        seq
          (let* () = store ~typ:I64 j' (Memory.unbox_int64 j) in
           let* () =
             let* j = load j' in
             instr (W.Br_if (label_index context zero_divide_pc, W.UnOp (I64 Eqz, j)))
           in
           let* () = store ~typ:I64 i' (Memory.unbox_int64 i) in
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
          (Memory.box_int64 (load res)));
    register_bin_prim_ctx "caml_int64_mod" (fun context i j ->
        let j' = Var.fresh () in
        seq
          (let* () = store ~typ:I64 j' (Memory.unbox_int64 j) in
           let* j = load j' in
           instr (W.Br_if (label_index context zero_divide_pc, W.UnOp (I64 Eqz, j))))
          (let* i = Memory.unbox_int64 i in
           let* j = load j' in
           Memory.box_int64 (return (W.BinOp (I64 (Rem S), i, j)))));
    register_bin_prim "caml_int64_shift_left" `Pure (fun i j -> int64_shift_op Shl i j);
    register_bin_prim "caml_int64_shift_right" `Pure (fun i j ->
        int64_shift_op (Shr S) i j);
    register_bin_prim "caml_int64_shift_right_unsigned" `Pure (fun i j ->
        int64_shift_op (Shr U) i j);
    register_un_prim "caml_int64_to_int" `Pure (fun i ->
        let* i = Memory.unbox_int64 i in
        Value.val_int (return (W.I32WrapI64 i)));
    register_un_prim "caml_int64_of_int" `Pure (fun i ->
        let* i = Value.int_val i in
        Memory.box_int64
          (return
             (match i with
             | Const (I32 i) -> W.Const (I64 (Int64.of_int32 i))
             | _ -> W.I64ExtendI32 (S, i))));
    register_un_prim "caml_int64_to_int32" `Pure (fun i ->
        let* i = Memory.unbox_int64 i in
        Memory.box_int32 (return (W.I32WrapI64 i)));
    register_un_prim "caml_int64_of_int32" `Pure (fun i ->
        let* i = Memory.unbox_int32 i in
        Memory.box_int64 (return (W.I64ExtendI32 (S, i))));
    register_un_prim "caml_int64_to_nativeint" `Pure (fun i ->
        let* i = Memory.unbox_int64 i in
        Memory.box_nativeint (return (W.I32WrapI64 i)));
    register_un_prim "caml_int64_of_nativeint" `Pure (fun i ->
        let* i = Memory.unbox_nativeint i in
        Memory.box_int64 (return (W.I64ExtendI32 (S, i))));
    register_un_prim "caml_nativeint_bits_of_float" `Pure (fun f ->
        let* f = Memory.unbox_float f in
        Memory.box_nativeint (return (W.UnOp (I32 ReinterpretF, F32DemoteF64 f))));
    register_un_prim "caml_nativeint_float_of_bits" `Pure (fun i ->
        let* i = Memory.unbox_int64 i in
        Memory.box_float (return (W.F64PromoteF32 (UnOp (I32 ReinterpretF, i)))));
    register_un_prim "caml_nativeint_of_float" `Pure (fun f ->
        let* f = Memory.unbox_float f in
        Memory.box_nativeint (return (W.UnOp (I32 (TruncSatF64 S), f))));
    register_un_prim "caml_nativeint_to_float" `Pure (fun n ->
        let* n = Memory.unbox_nativeint n in
        Memory.box_float (return (W.UnOp (F64 (Convert (`I32, S)), n))));
    register_un_prim "caml_nativeint_neg" `Pure (fun i ->
        let* i = Memory.unbox_nativeint i in
        Memory.box_nativeint (return (W.BinOp (I32 Sub, Const (I32 0l), i))));
    register_bin_prim "caml_nativeint_add" `Pure (fun i j -> nativeint_bin_op Add i j);
    register_bin_prim "caml_nativeint_sub" `Pure (fun i j -> nativeint_bin_op Sub i j);
    register_bin_prim "caml_nativeint_mul" `Pure (fun i j -> nativeint_bin_op Mul i j);
    register_bin_prim "caml_nativeint_and" `Pure (fun i j -> nativeint_bin_op And i j);
    register_bin_prim "caml_nativeint_or" `Pure (fun i j -> nativeint_bin_op Or i j);
    register_bin_prim "caml_nativeint_xor" `Pure (fun i j -> nativeint_bin_op Xor i j);
    register_bin_prim_ctx "caml_nativeint_div" (fun context i j ->
        let res = Var.fresh () in
        (*ZZZ Can we do better?*)
        let i' = Var.fresh () in
        let j' = Var.fresh () in
        seq
          (let* () = store ~typ:I32 j' (Memory.unbox_nativeint j) in
           let* () =
             let* j = load j' in
             instr (W.Br_if (label_index context zero_divide_pc, W.UnOp (I32 Eqz, j)))
           in
           let* () = store ~typ:I32 i' (Memory.unbox_nativeint i) in
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
          (Memory.box_nativeint (load res)));
    register_bin_prim_ctx "caml_nativeint_mod" (fun context i j ->
        let j' = Var.fresh () in
        seq
          (let* () = store ~typ:I32 j' (Memory.unbox_nativeint j) in
           let* j = load j' in
           instr (W.Br_if (label_index context zero_divide_pc, W.UnOp (I32 Eqz, j))))
          (let* i = Memory.unbox_nativeint i in
           let* j = load j' in
           Memory.box_nativeint (return (W.BinOp (I32 (Rem S), i, j)))));
    register_bin_prim "caml_nativeint_shift_left" `Pure (fun i j ->
        nativeint_shift_op Shl i j);
    register_bin_prim "caml_nativeint_shift_right" `Pure (fun i j ->
        nativeint_shift_op (Shr S) i j);
    register_bin_prim "caml_nativeint_shift_right_unsigned" `Pure (fun i j ->
        nativeint_shift_op (Shr U) i j);
    register_un_prim "caml_nativeint_to_int" `Pure (fun i ->
        Value.val_int (Memory.unbox_nativeint i));
    register_un_prim "caml_nativeint_of_int" `Pure (fun i ->
        Memory.box_nativeint (Value.int_val i));
    register_bin_prim "caml_int_compare" `Pure (fun i j ->
        Value.val_int
          Arith.(
            (Value.int_val j < Value.int_val i) - (Value.int_val i < Value.int_val j)));
    register_prim "%js_array" `Pure (fun ctx _ transl_prim_arg l ->
        let* l =
          List.fold_right
            ~f:(fun x acc ->
              let* x = transl_prim_arg x in
              let* acc = acc in
              return (`Expr x :: acc))
            l
            ~init:(return [])
        in
        Memory.allocate ~tag:0 ~deadcode_sentinal:ctx.deadcode_sentinal l)

  let rec translate_expr ctx context x e =
    match e with
    | Apply { f; args; exact }
      when exact || List.length args = if Var.Set.mem x ctx.in_cps then 2 else 1 ->
        let rec loop acc l =
          match l with
          | [] -> (
              let arity = List.length args in
              let funct = Var.fresh () in
              let* closure = tee funct (load f) in
              let* ty, funct =
                Memory.load_function_pointer
                  ~cps:(Var.Set.mem x ctx.in_cps)
                  ~arity
                  (load funct)
              in
              let* b = is_closure f in
              if b
              then return (W.Call (f, List.rev (closure :: acc)))
              else
                match funct with
                | W.RefFunc g ->
                    (* Functions with constant closures ignore their
                       environment. In case of partial application, we
                       still need the closure. *)
                    let* cl = if exact then Value.unit else return closure in
                    return (W.Call (g, List.rev (cl :: acc)))
                | _ -> return (W.Call_ref (ty, funct, List.rev (closure :: acc))))
          | x :: r ->
              let* x = load x in
              loop (x :: acc) r
        in
        loop [] args
    | Apply { f; args; _ } ->
        let* apply =
          need_apply_fun ~cps:(Var.Set.mem x ctx.in_cps) ~arity:(List.length args)
        in
        let* args = expression_list load args in
        let* closure = load f in
        return (W.Call (apply, args @ [ closure ]))
    | Block (tag, a, _, _) ->
        Memory.allocate
          ~deadcode_sentinal:ctx.deadcode_sentinal
          ~tag
          (List.map ~f:(fun x -> `Var x) (Array.to_list a))
    | Field (x, n, Non_float) -> Memory.field (load x) n
    | Field (x, n, Float) ->
        Memory.float_array_get
          (load x)
          (Constant.translate (Int (Targetint.of_int_warning_on_overflow n)))
    | Closure _ ->
        Closure.translate
          ~context:ctx.global_context
          ~closures:ctx.closures
          ~cps:(Var.Set.mem x ctx.in_cps)
          x
    | Constant c -> Constant.translate c
    | Special (Alias_prim _) -> assert false
    | Prim (Extern "caml_alloc_dummy_function", [ _; Pc (Int arity) ]) ->
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
        let v = transl_prim_arg v in
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
    | Prim (p, l) -> (
        match p with
        | Extern name when String.Hashtbl.mem internal_primitives name ->
            snd
              (String.Hashtbl.find internal_primitives name)
              ctx
              context
              transl_prim_arg
              l
        | _ -> (
            let l = List.map ~f:transl_prim_arg l in
            match p, l with
            | Extern name, l -> (
                try
                  let ((_, arg_typ, res_typ) as typ) =
                    String.Hashtbl.find specialized_primitives name
                  in
                  let* f = register_import ~name (Fun (specialized_primitive_type typ)) in
                  let rec loop acc arg_typ l =
                    match arg_typ, l with
                    | [], [] -> box_value res_typ (return (W.Call (f, List.rev acc)))
                    | repr :: rem, x :: r ->
                        let* x = unbox_value repr x in
                        loop (x :: acc) rem r
                    | [], _ :: _ | _ :: _, [] -> assert false
                  in
                  loop [] arg_typ l
                with Not_found ->
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
                  loop [] l)
            | Not, [ x ] -> Value.not x
            | Lt, [ x; y ] -> Value.lt x y
            | Le, [ x; y ] -> Value.le x y
            | Eq, [ x; y ] -> Value.eq x y
            | Neq, [ x; y ] -> Value.neq x y
            | Ult, [ x; y ] -> Value.ult x y
            | Array_get, [ x; y ] -> Memory.array_get x y
            | IsInt, [ x ] -> Value.is_int x
            | Vectlength, [ x ] -> Value.val_int (Memory.gen_array_length x)
            | (Not | Lt | Le | Eq | Neq | Ult | Array_get | IsInt | Vectlength), _ ->
                assert false))

  and translate_instr ctx context i =
    match i with
    | Assign (x, y) -> assign x (load y)
    | Let (x, e) ->
        if ctx.live.(Var.idx x) = 0
        then drop (translate_expr ctx context x e)
        else store x (translate_expr ctx context x e)
    | Set_field (x, n, Non_float, y) -> Memory.set_field (load x) n (load y)
    | Set_field (x, n, Float, y) ->
        Memory.float_array_set
          (load x)
          (Constant.translate (Int (Targetint.of_int_warning_on_overflow n)))
          (load y)
    | Offset_ref (x, n) ->
        Memory.set_field
          (load x)
          0
          (Value.val_int
             Arith.(Value.int_val (Memory.field (load x) 0) + const (Int32.of_int n)))
    | Array_set (x, y, z) -> Memory.array_set (load x) (load y) (load z)
    | Event loc -> event loc

  and translate_instrs ctx context l =
    match l with
    | [] -> return ()
    | i :: rem ->
        let* () = translate_instr ctx context i in
        translate_instrs ctx context rem

  let parallel_renaming params args =
    let rec visit visited prev s m x l =
      if not (Var.Set.mem x visited)
      then
        let visited = Var.Set.add x visited in
        let y = Var.Map.find x m in
        if Code.Var.compare x y = 0
        then visited, None, l
        else if Var.Set.mem y prev
        then
          let t = Code.Var.fresh () in
          visited, Some (y, t), (x, t) :: l
        else if Var.Set.mem y s
        then
          let visited, aliases, l = visit visited (Var.Set.add x prev) s m y l in
          match aliases with
          | Some (a, b) when Code.Var.compare a x = 0 ->
              visited, None, (b, a) :: (x, y) :: l
          | _ -> visited, aliases, (x, y) :: l
        else visited, None, (x, y) :: l
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
      ~f:(fun continuation (y, x) ->
        let* () = continuation in
        store ~always:true y (load x))
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
                        | "caml_check_bound_float" )
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
        instr (W.Return (Some (RefI31 (Const (I32 0l)))))
    else body ~result_typ ~fall_through ~context

  let wrap_with_handlers p pc ~result_typ ~fall_through ~context body =
    let need_zero_divide_handler, need_bound_error_handler = needed_handlers p pc in
    wrap_with_handler
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
         body)
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
              let* e = load x in
              match fall_through with
              | `Return -> instr (Push e)
              | `Block _ | `Catch | `Skip -> instr (Return (Some e)))
          | Cond (x, cont1, cont2) ->
              let context' = extend_context fall_through context in
              if_
                { params = []; result = result_typ }
                (Value.check_is_not_zero (load x))
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
              let* e = Value.int_val (load x) in
              instr (Br_table (e, List.map ~f:dest l, dest a.(len - 1)))
          | Raise (x, _) -> (
              let* e = load x in
              let* tag = register_import ~name:exception_name (Tag Type.value) in
              match fall_through with
              | `Catch -> instr (Push e)
              | `Block _ | `Return | `Skip -> (
                  match catch_index context with
                  | Some i -> instr (Br (i, Some e))
                  | None -> instr (Throw (tag, e))))
          | Pushtrap (cont, x, cont') ->
              handle_exceptions
                ~result_typ
                ~fall_through
                ~context:(extend_context fall_through context)
                (wrap_with_handlers
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
          parallel_renaming block.params args
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
          let* _ = add_var x in
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
               p
               pc
               ~result_typ:[ Type.value ]
               ~fall_through:`Return
               ~context:[]
               (fun ~result_typ ~fall_through ~context ->
                 translate_branch result_typ fall_through (-1) cont context)
           in
           match cloc with
           | Some loc -> event loc
           | None -> return ())
    in
    let body = post_process_function_body ~param_names ~locals body in
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
          | Some _ -> Type.func_type (param_count - 1))
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
    ~warn_on_unhandled_effect
*)
      ~deadcode_sentinal =
    global_context.unit_name <- unit_name;
    let p, closures = Closure_conversion.f p in
    (*
  Code.Print.program (fun _ _ -> "") p;
*)
    let ctx =
      { live = live_vars
      ; in_cps
      ; deadcode_sentinal
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
    let js_code =
      List.rev global_context.strings, StringMap.bindings global_context.fragments
    in
    global_context.string_count <- 0;
    global_context.strings <- [];
    global_context.string_index <- StringMap.empty;
    global_context.fragments <- StringMap.empty;
    toplevel_name, js_code

  let output ~context =
    Curry.f ~context;
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

let f ~context ~unit_name p ~live_vars ~in_cps ~deadcode_sentinal =
  let t = Timer.make () in
  let p = fix_switch_branches p in
  let res = G.f ~context ~unit_name ~live_vars ~in_cps ~deadcode_sentinal p in
  if times () then Format.eprintf "  code gen.: %a@." Timer.print t;
  res

let add_start_function = G.add_start_function

let add_init_function = G.add_init_function

let output ch ~context =
  let t = Timer.make () in
  let fields = G.output ~context in
  Wat_output.f ch fields;
  if times () then Format.eprintf "  output: %a@." Timer.print t

let wasm_output ch ~opt_source_map_file ~context =
  let t = Timer.make () in
  let fields = G.output ~context in
  Wasm_output.f ch ~opt_source_map_file fields;
  if times () then Format.eprintf "  output: %a@." Timer.print t
