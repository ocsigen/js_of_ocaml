open! Stdlib
open Code
module W = Wa_ast
open Wa_code_generation

let target = `GC (*`Core*)

module Generate (Target : Wa_target_sig.S) = struct
  open Target

  let transl_prim_arg x =
    match x with
    | Pv x -> load x
    | Pc c -> Constant.translate c

  type ctx =
    { live : int array
    ; blocks : block Addr.Map.t
    ; closures : Wa_closure_conversion.closure Var.Map.t
    ; global_context : Wa_code_generation.context
    }

  let func_type n =
    { W.params = List.init ~len:n ~f:(fun _ -> Value.value); result = [ Value.value ] }

  let float_bin_op' stack_ctx x op f g =
    Memory.box_float stack_ctx x (op (Memory.unbox_float f) (Memory.unbox_float g))

  let float_bin_op stack_ctx x op f g =
    let* f = Memory.unbox_float f in
    let* g = Memory.unbox_float g in
    Memory.box_float stack_ctx x (return (W.BinOp (F64 op, f, g)))

  let float_un_op' stack_ctx x op f =
    Memory.box_float stack_ctx x (op (Memory.unbox_float f))

  let float_un_op stack_ctx x op f =
    let* f = Memory.unbox_float f in
    Memory.box_float stack_ctx x (return (W.UnOp (F64 op, f)))

  let float_comparison op f g =
    let* f = Memory.unbox_float f in
    let* g = Memory.unbox_float g in
    Value.val_int (return (W.BinOp (F64 op, f, g)))

  let int32_bin_op stack_ctx x op f g =
    let* f = Memory.unbox_int32 f in
    let* g = Memory.unbox_int32 g in
    Memory.box_int32 stack_ctx x (return (W.BinOp (I32 op, f, g)))

  let int32_shift_op stack_ctx x op f g =
    let* f = Memory.unbox_int32 f in
    let* g = Value.int_val g in
    Memory.box_int32 stack_ctx x (return (W.BinOp (I32 op, f, g)))

  let int64_bin_op stack_ctx x op f g =
    let* f = Memory.unbox_int64 f in
    let* g = Memory.unbox_int64 g in
    Memory.box_int64 stack_ctx x (return (W.BinOp (I64 op, f, g)))

  let int64_shift_op stack_ctx x op f g =
    let* f = Memory.unbox_int64 f in
    let* g = Value.int_val g in
    Memory.box_int64 stack_ctx x (return (W.BinOp (I64 op, f, I64ExtendI32 (S, g))))

  let nativeint_bin_op stack_ctx x op f g =
    let* f = Memory.unbox_nativeint f in
    let* g = Memory.unbox_nativeint g in
    Memory.box_nativeint stack_ctx x (return (W.BinOp (I32 op, f, g)))

  let nativeint_shift_op stack_ctx x op f g =
    let* f = Memory.unbox_nativeint f in
    let* g = Value.int_val g in
    Memory.box_nativeint stack_ctx x (return (W.BinOp (I32 op, f, g)))

  let rec translate_expr ctx stack_ctx x e =
    match e with
    | Apply { f; args; exact } when exact || List.length args = 1 ->
        let* () = Stack.perform_spilling stack_ctx (`Instr x) in
        let rec loop acc l =
          match l with
          | [] -> (
              let arity = List.length args in
              let funct = Var.fresh () in
              let* closure = tee funct (load f) in
              let* kind, funct = Memory.load_function_pointer ~arity (load funct) in
              Stack.kill_variables stack_ctx;
              let* b = is_closure f in
              if b
              then return (W.Call (f, List.rev (closure :: acc)))
              else
                match kind, funct with
                | `Index, W.ConstSym (V g, 0) | `Ref _, W.RefFunc g ->
                    (* Functions with constant closures ignore their
                       environment *)
                    let* unit = Value.unit in
                    return (W.Call (g, List.rev (unit :: acc)))
                | `Index, _ ->
                    return
                      (W.Call_indirect
                         (func_type (arity + 1), funct, List.rev (closure :: acc)))
                | `Ref ty, _ -> return (W.Call_ref (ty, funct, List.rev (closure :: acc)))
              )
          | x :: r ->
              let* x = load x in
              loop (x :: acc) r
        in
        loop [] args
    | Apply { f; args; _ } ->
        let* () = Stack.perform_spilling stack_ctx (`Instr x) in
        let* apply = need_apply_fun ~arity:(List.length args) in
        let* args = expression_list load args in
        let* closure = load f in
        Stack.kill_variables stack_ctx;
        return (W.Call (apply, args @ [ closure ]))
    | Block (tag, a, _) ->
        Memory.allocate stack_ctx x ~tag (List.map ~f:(fun x -> `Var x) (Array.to_list a))
    | Field (x, n) -> Memory.field (load x) n
    | Closure _ ->
        Closure.translate ~context:ctx.global_context ~closures:ctx.closures ~stack_ctx x
    | Constant c -> Constant.translate c
    | Prim (Extern "caml_alloc_dummy_function", [ _; Pc (Int (_, arity)) ])
      when Poly.(target = `GC) -> Closure.dummy ~arity:(Int32.to_int arity)
    | Prim (Extern "caml_alloc_dummy_infix", _) when Poly.(target = `GC) ->
        Closure.dummy ~arity:1
    | Prim (p, l) -> (
        let l = List.map ~f:transl_prim_arg l in
        match p, l with
        (*ZZZ array operations need to deal with array of unboxed floats *)
        | Extern ("caml_array_unsafe_get" | "caml_floatarray_unsafe_get"), [ x; y ] ->
            Memory.array_get x y
        | Extern ("caml_array_unsafe_set" | "caml_floatarray_unsafe_set"), [ x; y; z ] ->
            seq (Memory.array_set x y z) Value.unit
        | Extern ("caml_string_unsafe_get" | "caml_bytes_unsafe_get"), [ x; y ] ->
            Memory.bytes_get x y
        | Extern ("caml_string_unsafe_set" | "caml_bytes_unsafe_set"), [ x; y; z ] ->
            seq (Memory.bytes_set x y z) Value.unit
        | Extern ("caml_string_get" | "caml_bytes_get"), [ x; y ] ->
            seq
              (let* f =
                 register_import
                   ~name:"caml_bound_error"
                   (Fun { params = []; result = [] })
               in
               if_
                 { params = []; result = [] }
                 (Arith.uge (Value.int_val y) (Memory.bytes_length x))
                 (instr (CallInstr (f, [])))
                 (return ()))
              (Memory.bytes_get x y)
        | Extern ("caml_string_set" | "caml_bytes_set"), [ x; y; z ] ->
            seq
              (let* f =
                 register_import
                   ~name:"caml_bound_error"
                   (Fun { params = []; result = [] })
               in
               let* () =
                 if_
                   { params = []; result = [] }
                   (Arith.uge (Value.int_val y) (Memory.bytes_length x))
                   (instr (CallInstr (f, [])))
                   (return ())
               in
               Memory.bytes_set x y z)
              Value.unit
        | Extern ("caml_ml_string_length" | "caml_ml_bytes_length"), [ x ] ->
            Value.val_int (Memory.bytes_length x)
        | Extern "%int_add", [ x; y ] -> Value.int_add x y
        | Extern "%int_sub", [ x; y ] -> Value.int_sub x y
        | Extern ("%int_mul" | "%direct_int_mul"), [ x; y ] -> Value.int_mul x y
        | Extern "%direct_int_div", [ x; y ] -> Value.int_div x y
        | Extern "%int_div", [ x; y ] ->
            let* f =
              register_import
                ~name:"caml_raise_zero_divide"
                (Fun { params = []; result = [] })
            in
            seq
              (if_
                 { params = []; result = [] }
                 (Arith.eqz (Value.int_val y))
                 (instr (CallInstr (f, [])))
                 (return ()))
              (Value.int_div x y)
        | Extern "%int_mod", [ x; y ] ->
            let* f =
              register_import
                ~name:"caml_raise_zero_divide"
                (Fun { params = []; result = [] })
            in
            seq
              (if_
                 { params = []; result = [] }
                 (Arith.eqz (Value.int_val y))
                 (instr (CallInstr (f, [])))
                 (return ()))
              (Value.int_mod x y)
        | Extern "%direct_int_mod", [ x; y ] -> Value.int_mod x y
        | Extern "%int_neg", [ x ] -> Value.int_neg x
        | Extern "%int_or", [ x; y ] -> Value.int_or x y
        | Extern "%int_and", [ x; y ] -> Value.int_and x y
        | Extern "%int_xor", [ x; y ] -> Value.int_xor x y
        | Extern "%int_lsl", [ x; y ] -> Value.int_lsl x y
        | Extern "%int_lsr", [ x; y ] -> Value.int_lsr x y
        | Extern "%int_asr", [ x; y ] -> Value.int_asr x y
        | Extern "caml_check_bound", [ x; y ] ->
            let* f =
              register_import ~name:"caml_bound_error" (Fun { params = []; result = [] })
            in
            seq
              (if_
                 { params = []; result = [] }
                 (Arith.uge (Value.int_val y) (Memory.block_length x))
                 (instr (CallInstr (f, [])))
                 (return ()))
              x
        | Extern "caml_add_float", [ f; g ] -> float_bin_op stack_ctx x Add f g
        | Extern "caml_sub_float", [ f; g ] -> float_bin_op stack_ctx x Sub f g
        | Extern "caml_mul_float", [ f; g ] -> float_bin_op stack_ctx x Mul f g
        | Extern "caml_div_float", [ f; g ] -> float_bin_op stack_ctx x Div f g
        | Extern "caml_copysign_float", [ f; g ] -> float_bin_op stack_ctx x CopySign f g
        | Extern "caml_signbit_float", [ f ] ->
            let* f = Memory.unbox_float f in
            let sign = W.BinOp (F64 CopySign, Const (F64 1.), f) in
            Value.val_int (return (W.BinOp (F64 Lt, sign, Const (F64 0.))))
        | Extern "caml_neg_float", [ f ] -> float_un_op stack_ctx x Neg f
        | Extern "caml_abs_float", [ f ] -> float_un_op stack_ctx x Abs f
        | Extern "caml_ceil_float", [ f ] -> float_un_op stack_ctx x Ceil f
        | Extern "caml_floor_float", [ f ] -> float_un_op stack_ctx x Floor f
        | Extern "caml_trunc_float", [ f ] -> float_un_op stack_ctx x Trunc f
        | Extern "caml_round_float", [ f ] -> float_un_op' stack_ctx x Math.round f
        | Extern "caml_sqrt_float", [ f ] -> float_un_op stack_ctx x Sqrt f
        | Extern "caml_eq_float", [ f; g ] -> float_comparison Eq f g
        | Extern "caml_neq_float", [ f; g ] -> float_comparison Ne f g
        | Extern "caml_ge_float", [ f; g ] -> float_comparison Ge f g
        | Extern "caml_le_float", [ f; g ] -> float_comparison Le f g
        | Extern "caml_gt_float", [ f; g ] -> float_comparison Gt f g
        | Extern "caml_lt_float", [ f; g ] -> float_comparison Lt f g
        | Extern "caml_int_of_float", [ f ] ->
            let* f = Memory.unbox_float f in
            Value.val_int (return (W.UnOp (I32 (TruncSatF64 S), f)))
        | Extern "caml_float_of_int", [ n ] ->
            let* n = Value.int_val n in
            Memory.box_float stack_ctx x (return (W.UnOp (F64 (Convert (`I32, S)), n)))
        | Extern "caml_cos_float", [ f ] -> float_un_op' stack_ctx x Math.cos f
        | Extern "caml_sin_float", [ f ] -> float_un_op' stack_ctx x Math.sin f
        | Extern "caml_tan_float", [ f ] -> float_un_op' stack_ctx x Math.tan f
        | Extern "caml_acos_float", [ f ] -> float_un_op' stack_ctx x Math.acos f
        | Extern "caml_asin_float", [ f ] -> float_un_op' stack_ctx x Math.asin f
        | Extern "caml_atan_float", [ f ] -> float_un_op' stack_ctx x Math.atan f
        | Extern "caml_atan2_float", [ f; g ] -> float_bin_op' stack_ctx x Math.atan2 f g
        | Extern "caml_cosh_float", [ f ] -> float_un_op' stack_ctx x Math.cosh f
        | Extern "caml_sinh_float", [ f ] -> float_un_op' stack_ctx x Math.sinh f
        | Extern "caml_tanh_float", [ f ] -> float_un_op' stack_ctx x Math.tanh f
        | Extern "caml_acosh_float", [ f ] -> float_un_op' stack_ctx x Math.acosh f
        | Extern "caml_asinh_float", [ f ] -> float_un_op' stack_ctx x Math.asinh f
        | Extern "caml_atanh_float", [ f ] -> float_un_op' stack_ctx x Math.atanh f
        | Extern "caml_cbrt_float", [ f ] -> float_un_op' stack_ctx x Math.cbrt f
        | Extern "caml_exp_float", [ f ] -> float_un_op' stack_ctx x Math.exp f
        | Extern "caml_exp2_float", [ f ] -> float_un_op' stack_ctx x Math.exp2 f
        | Extern "caml_log_float", [ f ] -> float_un_op' stack_ctx x Math.log f
        | Extern "caml_expm1_float", [ f ] -> float_un_op' stack_ctx x Math.expm1 f
        | Extern "caml_log1p_float", [ f ] -> float_un_op' stack_ctx x Math.log1p f
        | Extern "caml_log2_float", [ f ] -> float_un_op' stack_ctx x Math.log2 f
        | Extern "caml_log10_float", [ f ] -> float_un_op' stack_ctx x Math.log10 f
        | Extern "caml_power_float", [ f; g ] -> float_bin_op' stack_ctx x Math.power f g
        | Extern "caml_hypot_float", [ f; g ] -> float_bin_op' stack_ctx x Math.hypot f g
        | Extern "caml_fmod_float", [ f; g ] -> float_bin_op' stack_ctx x Math.fmod f g
        | Extern "caml_int32_bits_of_float", [ f ] ->
            let* f = Memory.unbox_float f in
            Memory.box_int32
              stack_ctx
              x
              (return (W.UnOp (I32 ReinterpretF, F32DemoteF64 f)))
        | Extern "caml_int32_float_of_bits", [ i ] ->
            let* i = Memory.unbox_int64 i in
            Memory.box_float
              stack_ctx
              x
              (return (W.F64PromoteF32 (UnOp (I32 ReinterpretF, i))))
        | Extern "caml_int32_of_float", [ f ] ->
            let* f = Memory.unbox_float f in
            Memory.box_int32 stack_ctx x (return (W.UnOp (I32 (TruncSatF64 S), f)))
        | Extern "caml_int32_to_float", [ n ] ->
            let* n = Memory.unbox_int32 n in
            Memory.box_float stack_ctx x (return (W.UnOp (F64 (Convert (`I32, S)), n)))
        | Extern "caml_int32_neg", [ i ] ->
            let* i = Memory.unbox_int32 i in
            Memory.box_int32 stack_ctx x (return (W.BinOp (I32 Sub, Const (I32 0l), i)))
        | Extern "caml_int32_add", [ i; j ] -> int32_bin_op stack_ctx x Add i j
        | Extern "caml_int32_sub", [ i; j ] -> int32_bin_op stack_ctx x Sub i j
        | Extern "caml_int32_mul", [ i; j ] -> int32_bin_op stack_ctx x Mul i j
        | Extern "caml_int32_and", [ i; j ] -> int32_bin_op stack_ctx x And i j
        | Extern "caml_int32_or", [ i; j ] -> int32_bin_op stack_ctx x Or i j
        | Extern "caml_int32_xor", [ i; j ] -> int32_bin_op stack_ctx x Xor i j
        | Extern "caml_int32_div", [ i; j ] ->
            let* f =
              register_import
                ~name:"caml_raise_zero_divide"
                (Fun { params = []; result = [] })
            in
            let res = Var.fresh () in
            (*ZZZ Can we do better?*)
            let i' = Var.fresh () in
            let j' = Var.fresh () in
            seq
              (let* () = store ~typ:I32 j' (Memory.unbox_int32 j) in
               let* () =
                 if_
                   { params = []; result = [] }
                   (let* j = load j' in
                    return (W.UnOp (I32 Eqz, j)))
                   (instr (CallInstr (f, [])))
                   (return ())
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
              (Memory.box_int32 stack_ctx x (load res))
        | Extern "caml_int32_mod", [ i; j ] ->
            let* f =
              register_import
                ~name:"caml_raise_zero_divide"
                (Fun { params = []; result = [] })
            in
            let j' = Var.fresh () in
            seq
              (let* () = store ~typ:I32 j' (Memory.unbox_int32 j) in
               if_
                 { params = []; result = [] }
                 (let* j = load j' in
                  return (W.UnOp (I32 Eqz, j)))
                 (instr (CallInstr (f, [])))
                 (return ()))
              (let* i = Memory.unbox_int32 i in
               let* j = load j' in
               Memory.box_int32 stack_ctx x (return (W.BinOp (I32 (Rem S), i, j))))
        | Extern "caml_int32_shift_left", [ i; j ] -> int32_shift_op stack_ctx x Shl i j
        | Extern "caml_int32_shift_right", [ i; j ] ->
            int32_shift_op stack_ctx x (Shr S) i j
        | Extern "caml_int32_shift_right_unsigned", [ i; j ] ->
            int32_shift_op stack_ctx x (Shr U) i j
        | Extern "caml_int32_to_int", [ i ] -> Value.val_int (Memory.unbox_int32 i)
        | Extern "caml_int32_of_int", [ i ] ->
            Memory.box_int32 stack_ctx x (Value.int_val i)
        | Extern "caml_int64_bits_of_float", [ f ] ->
            let* f = Memory.unbox_float f in
            Memory.box_int64 stack_ctx x (return (W.UnOp (I64 ReinterpretF, f)))
        | Extern "caml_int64_float_of_bits", [ i ] ->
            let* i = Memory.unbox_int64 i in
            Memory.box_float stack_ctx x (return (W.UnOp (F64 ReinterpretI, i)))
        | Extern "caml_int64_of_float", [ f ] ->
            let* f = Memory.unbox_float f in
            Memory.box_int64 stack_ctx x (return (W.UnOp (I64 (TruncSatF64 S), f)))
        | Extern "caml_int64_to_float", [ n ] ->
            let* n = Memory.unbox_int64 n in
            Memory.box_float stack_ctx x (return (W.UnOp (F64 (Convert (`I64, S)), n)))
        | Extern "caml_int64_neg", [ i ] ->
            let* i = Memory.unbox_int64 i in
            Memory.box_int64 stack_ctx x (return (W.BinOp (I64 Sub, Const (I64 0L), i)))
        | Extern "caml_int64_add", [ i; j ] -> int64_bin_op stack_ctx x Add i j
        | Extern "caml_int64_sub", [ i; j ] -> int64_bin_op stack_ctx x Sub i j
        | Extern "caml_int64_mul", [ i; j ] -> int64_bin_op stack_ctx x Mul i j
        | Extern "caml_int64_and", [ i; j ] -> int64_bin_op stack_ctx x And i j
        | Extern "caml_int64_or", [ i; j ] -> int64_bin_op stack_ctx x Or i j
        | Extern "caml_int64_xor", [ i; j ] -> int64_bin_op stack_ctx x Xor i j
        | Extern "caml_int64_div", [ i; j ] ->
            let* f =
              register_import
                ~name:"caml_raise_zero_divide"
                (Fun { params = []; result = [] })
            in
            let res = Var.fresh () in
            (*ZZZ Can we do better?*)
            let i' = Var.fresh () in
            let j' = Var.fresh () in
            seq
              (let* () = store ~typ:I64 j' (Memory.unbox_int64 j) in
               let* () =
                 if_
                   { params = []; result = [] }
                   (let* j = load j' in
                    return (W.UnOp (I64 Eqz, j)))
                   (instr (CallInstr (f, [])))
                   (return ())
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
              (Memory.box_int64 stack_ctx x (load res))
        | Extern "caml_int64_mod", [ i; j ] ->
            let* f =
              register_import
                ~name:"caml_raise_zero_divide"
                (Fun { params = []; result = [] })
            in
            let j' = Var.fresh () in
            seq
              (let* () = store ~typ:I64 j' (Memory.unbox_int64 j) in
               if_
                 { params = []; result = [] }
                 (let* j = load j' in
                  return (W.UnOp (I64 Eqz, j)))
                 (instr (CallInstr (f, [])))
                 (return ()))
              (let* i = Memory.unbox_int64 i in
               let* j = load j' in
               Memory.box_int64 stack_ctx x (return (W.BinOp (I64 (Rem S), i, j))))
        | Extern "caml_int64_shift_left", [ i; j ] -> int64_shift_op stack_ctx x Shl i j
        | Extern "caml_int64_shift_right", [ i; j ] ->
            int64_shift_op stack_ctx x (Shr S) i j
        | Extern "caml_int64_shift_right_unsigned", [ i; j ] ->
            int64_shift_op stack_ctx x (Shr U) i j
        | Extern "caml_int64_to_int", [ i ] ->
            let* i = Memory.unbox_int64 i in
            Value.val_int (return (W.I32WrapI64 i))
        | Extern "caml_int64_of_int", [ i ] ->
            let* i = Value.int_val i in
            Memory.box_int64
              stack_ctx
              x
              (return
                 (match i with
                 | Const (I32 i) -> W.Const (I64 (Int64.of_int32 i))
                 | _ -> W.I64ExtendI32 (S, i)))
        | Extern "caml_int64_to_int32", [ i ] ->
            let* i = Memory.unbox_int64 i in
            Memory.box_int32 stack_ctx x (return (W.I32WrapI64 i))
        | Extern "caml_int64_of_int32", [ i ] ->
            let* i = Memory.unbox_int32 i in
            Memory.box_int64 stack_ctx x (return (W.I64ExtendI32 (S, i)))
        | Extern "caml_int64_to_nativeint", [ i ] ->
            let* i = Memory.unbox_int64 i in
            Memory.box_nativeint stack_ctx x (return (W.I32WrapI64 i))
        | Extern "caml_int64_of_nativeint", [ i ] ->
            let* i = Memory.unbox_nativeint i in
            Memory.box_int64 stack_ctx x (return (W.I64ExtendI32 (S, i)))
        | Extern "caml_nativeint_bits_of_float", [ f ] ->
            let* f = Memory.unbox_float f in
            Memory.box_nativeint
              stack_ctx
              x
              (return (W.UnOp (I32 ReinterpretF, F32DemoteF64 f)))
        | Extern "caml_nativeint_float_of_bits", [ i ] ->
            let* i = Memory.unbox_int64 i in
            Memory.box_float
              stack_ctx
              x
              (return (W.F64PromoteF32 (UnOp (I32 ReinterpretF, i))))
        | Extern "caml_nativeint_of_float", [ f ] ->
            let* f = Memory.unbox_float f in
            Memory.box_nativeint stack_ctx x (return (W.UnOp (I32 (TruncSatF64 S), f)))
        | Extern "caml_nativeint_to_float", [ n ] ->
            let* n = Memory.unbox_nativeint n in
            Memory.box_float stack_ctx x (return (W.UnOp (F64 (Convert (`I32, S)), n)))
        | Extern "caml_nativeint_neg", [ i ] ->
            let* i = Memory.unbox_nativeint i in
            Memory.box_nativeint
              stack_ctx
              x
              (return (W.BinOp (I32 Sub, Const (I32 0l), i)))
        | Extern "caml_nativeint_add", [ i; j ] -> nativeint_bin_op stack_ctx x Add i j
        | Extern "caml_nativeint_sub", [ i; j ] -> nativeint_bin_op stack_ctx x Sub i j
        | Extern "caml_nativeint_mul", [ i; j ] -> nativeint_bin_op stack_ctx x Mul i j
        | Extern "caml_nativeint_and", [ i; j ] -> nativeint_bin_op stack_ctx x And i j
        | Extern "caml_nativeint_or", [ i; j ] -> nativeint_bin_op stack_ctx x Or i j
        | Extern "caml_nativeint_xor", [ i; j ] -> nativeint_bin_op stack_ctx x Xor i j
        | Extern "caml_nativeint_div", [ i; j ] ->
            let* f =
              register_import
                ~name:"caml_raise_zero_divide"
                (Fun { params = []; result = [] })
            in
            let res = Var.fresh () in
            (*ZZZ Can we do better?*)
            let i' = Var.fresh () in
            let j' = Var.fresh () in
            seq
              (let* () = store ~typ:I32 j' (Memory.unbox_nativeint j) in
               let* () =
                 if_
                   { params = []; result = [] }
                   (let* j = load j' in
                    return (W.UnOp (I32 Eqz, j)))
                   (instr (CallInstr (f, [])))
                   (return ())
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
              (Memory.box_nativeint stack_ctx x (load res))
        | Extern "caml_nativeint_mod", [ i; j ] ->
            let* f =
              register_import
                ~name:"caml_raise_zero_divide"
                (Fun { params = []; result = [] })
            in
            let j' = Var.fresh () in
            seq
              (let* () = store ~typ:I32 j' (Memory.unbox_nativeint j) in
               if_
                 { params = []; result = [] }
                 (let* j = load j' in
                  return (W.UnOp (I32 Eqz, j)))
                 (instr (CallInstr (f, [])))
                 (return ()))
              (let* i = Memory.unbox_nativeint i in
               let* j = load j' in
               Memory.box_nativeint stack_ctx x (return (W.BinOp (I32 (Rem S), i, j))))
        | Extern "caml_nativeint_shift_left", [ i; j ] ->
            nativeint_shift_op stack_ctx x Shl i j
        | Extern "caml_nativeint_shift_right", [ i; j ] ->
            nativeint_shift_op stack_ctx x (Shr S) i j
        | Extern "caml_nativeint_shift_right_unsigned", [ i; j ] ->
            nativeint_shift_op stack_ctx x (Shr U) i j
        | Extern "caml_nativeint_to_int", [ i ] ->
            Value.val_int (Memory.unbox_nativeint i)
        | Extern "caml_nativeint_of_int", [ i ] ->
            Memory.box_nativeint stack_ctx x (Value.int_val i)
        | Extern "caml_int_compare", [ i; j ] ->
            Value.val_int
              Arith.(
                (Value.int_val j < Value.int_val i) - (Value.int_val i < Value.int_val j))
        | Extern name, l ->
            (*ZZZ Different calling convention when large number of parameters *)
            let* f = register_import ~name (Fun (func_type (List.length l))) in
            let* () = Stack.perform_spilling stack_ctx (`Instr x) in
            let rec loop acc l =
              match l with
              | [] ->
                  Stack.kill_variables stack_ctx;
                  return (W.Call (f, List.rev acc))
              | x :: r ->
                  let* x = x in
                  loop (x :: acc) r
            in
            loop [] l
        | Not, [ x ] -> Value.not x
        | Lt, [ x; y ] -> Value.lt x y
        | Le, [ x; y ] -> Value.le x y
        | Eq, [ x; y ] -> Value.eq x y
        | Neq, [ x; y ] -> Value.neq x y
        | Ult, [ x; y ] -> Value.ult x y
        | Array_get, [ x; y ] -> Memory.array_get x y
        | IsInt, [ x ] -> Value.is_int x
        | Vectlength, [ x ] -> Value.val_int (Memory.block_length x)
        | (Not | Lt | Le | Eq | Neq | Ult | Array_get | IsInt | Vectlength), _ ->
            assert false)

  and translate_instr ctx stack_ctx (i, _) =
    match i with
    | Assign (x, y) ->
        let* () = assign x (load y) in
        Stack.assign stack_ctx x
    | Let (x, e) ->
        if ctx.live.(Var.idx x) = 0
        then drop (translate_expr ctx stack_ctx x e)
        else store x (translate_expr ctx stack_ctx x e)
    | Set_field (x, n, y) -> Memory.set_field (load x) n (load y)
    | Offset_ref (x, n) ->
        Memory.set_field
          (load x)
          0
          (Value.val_int
             Arith.(Value.int_val (Memory.field (load x) 0) + const (Int32.of_int n)))
    | Array_set (x, y, z) -> Memory.array_set (load x) (load y) (load z)

  and translate_instrs ctx stack_ctx l =
    match l with
    | [] -> return ()
    | i :: rem ->
        let* () = Stack.perform_reloads stack_ctx (`Instr (fst i)) in
        let* () = translate_instr ctx stack_ctx i in
        translate_instrs ctx stack_ctx rem

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
    | `Block _ as b -> b :: context
    | `Return -> `Skip :: context

  let translate_function p ctx name_opt toplevel_name params ((pc, _) as cont) acc =
    let stack_info =
      Stack.generate_spilling_information
        p
        ~context:ctx.global_context
        ~closures:ctx.closures
        ~env:
          (match name_opt with
          | Some name -> name
          | None -> Var.fresh ())
        ~pc
        ~params
    in
    let g = Wa_structure.build_graph ctx.blocks pc in
    let dom = Wa_structure.dominator_tree g in
    let rec index pc i context =
      match context with
      | `Block pc' :: _ when pc = pc' -> i
      | (`Block _ | `Skip) :: rem -> index pc (i + 1) rem
      | [] -> assert false
    in
    let rec translate_tree result_typ fall_through pc context =
      let block = Addr.Map.find pc ctx.blocks in
      let is_switch =
        match fst block.branch with
        | Switch _ -> true
        | _ -> false
      in
      let code ~context =
        translate_node_within
          ~result_typ
          ~fall_through
          ~pc
          ~l:
            (pc
            |> Wa_structure.get_edges dom
            |> Addr.Set.elements
            |> List.filter ~f:(fun pc' -> is_switch || Wa_structure.is_merge_node g pc')
            |> Wa_structure.sort_in_post_order g)
          ~context
      in
      if Wa_structure.is_loop_header g pc
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
            if (not (List.is_empty rem))
               ||
               let block = Addr.Map.find pc ctx.blocks in
               match fst block.branch with
               | Cond _ | Pushtrap _ -> false (*ZZZ also some Switch*)
               | _ -> true
            then
              block { params = []; result = [] } (code ~context:(`Block pc' :: context))
            else code ~context
          in
          translate_tree result_typ fall_through pc' context
      | [] -> (
          let block = Addr.Map.find pc ctx.blocks in
          let* global_context = get_context in
          let stack_ctx = Stack.start_block ~context:global_context stack_info pc in
          let* () = translate_instrs ctx stack_ctx block.body in
          let* () = Stack.perform_reloads stack_ctx (`Branch (fst block.branch)) in
          let* () = Stack.perform_spilling stack_ctx (`Block pc) in
          match fst block.branch with
          | Branch cont ->
              translate_branch result_typ fall_through pc cont context stack_ctx
          | Return x -> (
              let* e = load x in
              match fall_through with
              | `Return -> instr (Push e)
              | `Block _ -> instr (Return (Some e)))
          | Cond (x, cont1, cont2) ->
              let context' = extend_context fall_through context in
              if_
                { params = []; result = result_typ }
                (Value.check_is_not_zero (load x))
                (translate_branch result_typ fall_through pc cont1 context' stack_ctx)
                (translate_branch result_typ fall_through pc cont2 context' stack_ctx)
          | Stop -> (
              let* e = Value.unit in
              match fall_through with
              | `Return -> instr (Push e)
              | `Block _ -> instr (Return (Some e)))
          | Switch (x, a1, a2) ->
              let l =
                List.filter
                  ~f:(fun pc' -> Stack.stack_adjustment_needed stack_ctx ~src:pc ~dst:pc')
                  (List.rev (Addr.Set.elements (Wa_structure.get_edges dom pc)))
              in
              let br_table e a context =
                let len = Array.length a in
                let l = Array.to_list (Array.sub a ~pos:0 ~len:(len - 1)) in
                let dest (pc, args) =
                  assert (List.is_empty args);
                  index pc 0 context
                in
                let* e = e in
                instr (Br_table (e, List.map ~f:dest l, dest a.(len - 1)))
              in
              let rec nest l context =
                match l with
                | pc' :: rem ->
                    let* () =
                      Wa_code_generation.block
                        { params = []; result = [] }
                        (nest rem (`Block pc' :: context))
                    in
                    let* () = Stack.adjust_stack stack_ctx ~src:pc ~dst:pc' in
                    instr (Br (index pc' 0 context, None))
                | [] -> (
                    match a1, a2 with
                    | [||], _ -> br_table (Memory.tag (load x)) a2 context
                    | _, [||] -> br_table (Value.int_val (load x)) a1 context
                    | _ ->
                        (*ZZZ Use Br_on_cast *)
                        let context' = extend_context fall_through context in
                        if_
                          { params = []; result = result_typ }
                          (Value.check_is_int (load x))
                          (br_table (Value.int_val (load x)) a1 context')
                          (br_table (Memory.tag (load x)) a2 context'))
              in
              nest l context
          | Raise (x, _) ->
              let* e = load x in
              let* tag = register_import ~name:exception_name (Tag Value.value) in
              instr (Throw (tag, e))
          | Pushtrap (cont, x, cont', _) ->
              let context' = extend_context fall_through context in
              let* tag = register_import ~name:exception_name (Tag Value.value) in
              try_
                { params = []; result = result_typ }
                (exception_handler_body
                   ~typ:result_typ
                   (translate_branch result_typ fall_through pc cont context' stack_ctx))
                tag
                (let* () = store ~always:true x (return (W.Pop Value.value)) in
                 translate_branch result_typ fall_through pc cont' context' stack_ctx)
          | Poptrap cont ->
              translate_branch result_typ fall_through pc cont context stack_ctx)
    and translate_branch result_typ fall_through src (dst, args) context stack_ctx =
      let* () =
        if List.is_empty args
        then return ()
        else
          let block = Addr.Map.find dst ctx.blocks in
          parallel_renaming block.params args
      in
      let* () = Stack.adjust_stack stack_ctx ~src ~dst in
      if (src >= 0 && Wa_structure.is_backward g src dst)
         || Wa_structure.is_merge_node g dst
      then
        match fall_through with
        | `Block dst' when dst = dst' -> return ()
        | _ -> instr (Br (index dst 0 context, None))
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
          Closure.bind_environment ~context:ctx.global_context ~closures:ctx.closures f
      | None -> return ()
    in
    (*
  Format.eprintf "=== %d ===@." pc;
*)
    let param_count =
      match name_opt with
      | None -> 0
      | Some _ -> List.length params + 1
    in
    let locals, body =
      function_body
        ~context:ctx.global_context
        ~value_type:Value.value
        ~param_count
        ~body:
          (let* () = build_initial_env in
           let stack_ctx = Stack.start_function ~context:ctx.global_context stack_info in
           let* () = Stack.perform_spilling stack_ctx `Function in
           translate_branch [ Value.value ] `Return (-1) cont [] stack_ctx)
    in
    W.Function
      { name =
          (match name_opt with
          | None -> toplevel_name
          | Some x -> x)
      ; exported_name = None
      ; typ = func_type param_count
      ; locals
      ; body
      }
    :: acc

  let entry_point ctx toplevel_fun entry_name =
    let typ, code = entry_point ~context:ctx.global_context in
    let body =
      let* () = code in
      drop (return (W.Call (toplevel_fun, [])))
    in
    let locals, body =
      function_body
        ~context:ctx.global_context
        ~value_type:Value.value
        ~param_count:(List.length typ.W.params)
        ~body
    in
    W.Function
      { name = Var.fresh_n "entry_point"
      ; exported_name = Some entry_name
      ; typ
      ; locals
      ; body
      }

  module Curry = Wa_curry.Make (Target)

  let f
      (p : Code.program)
      ~live_vars
       (*
    ~cps_calls
    ~should_export
    ~warn_on_unhandled_effect
      _debug *)
      =
    let p, closures = Wa_closure_conversion.f p in
    (*
  Code.Print.program (fun _ _ -> "") p;
*)
    let ctx =
      { live = live_vars; blocks = p.blocks; closures; global_context = make_context () }
    in
    let toplevel_name = Var.fresh_n "toplevel" in
    let functions =
      Code.fold_closures_outermost_first
        p
        (fun name_opt params cont ->
          translate_function p ctx name_opt toplevel_name params cont)
        []
    in
    Curry.f ~context:ctx.global_context;
    let start_function = entry_point ctx toplevel_name "_initialize" in
    let imports =
      List.concat
        (List.map
           ~f:(fun (import_module, m) ->
             List.map
               ~f:(fun (import_name, (name, desc)) ->
                 W.Import { import_module; import_name; name; desc })
               (StringMap.bindings m))
           (StringMap.bindings ctx.global_context.imports))
    in
    let constant_data =
      List.map
        ~f:(fun (name, (active, contents)) ->
          W.Data { name; read_only = true; active; contents })
        (Var.Map.bindings ctx.global_context.data_segments)
    in
    List.rev_append
      ctx.global_context.other_fields
      (imports @ functions @ (start_function :: constant_data))
end

let init () =
  List.iter
    ~f:(fun (nm, nm') -> Primitive.alias nm nm')
    [ "caml_alloc_dummy_float", "caml_alloc_dummy" (*ZZZ*)
    ; "caml_make_array", "%identity"
    ; "caml_ensure_stack_capacity", "%identity"
    ]

let f ch (p : Code.program) ~live_vars =
  match target with
  | `Core ->
      let module G = Generate (Wa_core_target) in
      let fields = G.f ~live_vars p in
      Wa_asm_output.f ch fields
  | `GC ->
      let module G = Generate (Wa_gc_target) in
      let fields = G.f ~live_vars p in
      Wa_wat_output.f ch fields
