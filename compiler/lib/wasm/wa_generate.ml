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
    ; in_cps : Effects.in_cps
    ; blocks : block Addr.Map.t
    ; closures : Wa_closure_conversion.closure Var.Map.t
    ; global_context : Wa_code_generation.context
    ; debug : Parse_bytecode.Debug.t
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

  let label_index context pc =
    let rec index_rec context pc i =
      match context with
      | `Block pc' :: _ when pc = pc' -> i
      | (`Block _ | `Skip) :: rem -> index_rec rem pc (i + 1)
      | [] -> assert false
    in
    index_rec context pc 0

  let bound_error_pc = -1

  let zero_divide_pc = -2

  let rec translate_expr ctx stack_ctx context x e =
    match e with
    | Apply { f; args; exact }
      when exact || List.length args = if Var.Set.mem x ctx.in_cps then 2 else 1 ->
        let* () = Stack.perform_spilling stack_ctx (`Instr x) in
        let rec loop acc l =
          match l with
          | [] -> (
              let arity = List.length args in
              let funct = Var.fresh () in
              let* closure = tee funct (load f) in
              let* kind, funct =
                Memory.load_function_pointer
                  ~cps:(Var.Set.mem x ctx.in_cps)
                  ~arity
                  (load funct)
              in
              Stack.kill_variables stack_ctx;
              let* b = is_closure f in
              if b
              then return (W.Call (f, List.rev (closure :: acc)))
              else
                match kind, funct with
                | `Index, W.ConstSym (V g, 0) | `Ref _, W.RefFunc g ->
                    (* Functions with constant closures ignore their
                       environment. In case of partial application, we
                       still need the closure. *)
                    let* cl = if exact then Value.unit else return closure in
                    return (W.Call (g, List.rev (cl :: acc)))
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
        let* apply =
          need_apply_fun ~cps:(Var.Set.mem x ctx.in_cps) ~arity:(List.length args)
        in
        let* args = expression_list load args in
        let* closure = load f in
        Stack.kill_variables stack_ctx;
        return (W.Call (apply, args @ [ closure ]))
    | Block (tag, a, _, _) ->
        Memory.allocate stack_ctx x ~tag (List.map ~f:(fun x -> `Var x) (Array.to_list a))
    | Field (x, n) -> Memory.field (load x) n
    | Closure _ ->
        Closure.translate
          ~context:ctx.global_context
          ~closures:ctx.closures
          ~stack_ctx
          ~cps:(Var.Set.mem x ctx.in_cps)
          x
    | Constant c -> Constant.translate c
    | Special Undefined -> Constant.translate (Int (Regular, 0l))
    | Special (Alias_prim _) -> assert false
    | Prim (Extern "caml_alloc_dummy_function", [ _; Pc (Int (_, arity)) ])
      when Poly.(target = `GC) ->
        Closure.dummy ~cps:(Config.Flag.effects ()) ~arity:(Int32.to_int arity)
    | Prim (Extern "caml_alloc_dummy_infix", _) when Poly.(target = `GC) ->
        Closure.dummy ~cps:(Config.Flag.effects ()) ~arity:1
    | Prim (Extern "caml_get_global", [ Pc (String name) ]) ->
        let* x =
          let* context = get_context in
          match
            List.find_map
              ~f:(fun f ->
                match f with
                | W.Global { name = V name'; exported_name = Some exported_name; _ }
                  when String.equal exported_name name -> Some name'
                | _ -> None)
              context.other_fields
          with
          | Some x -> return x
          | _ ->
              let* typ = Value.block_type in
              register_import ~import_module:"OCaml" ~name (Global { mut = true; typ })
        in
        return (W.GlobalGet (V x))
    | Prim (Extern "caml_set_global", [ Pc (String name); v ]) ->
        let v = transl_prim_arg v in
        let x = Var.fresh_n name in
        let* () =
          let* typ = Value.block_type in
          let* dummy = Value.dummy_block in
          register_global (V x) ~exported_name:name { mut = true; typ } dummy
        in
        seq
          (let* v = Value.as_block v in
           instr (W.GlobalSet (V x, v)))
          Value.unit
    | Prim (p, l) -> (
        match p with
        | Extern name when Hashtbl.mem internal_primitives name ->
            Hashtbl.find internal_primitives name transl_prim_arg l
        | _ -> (
            let l = List.map ~f:transl_prim_arg l in
            match p, l with
            | Extern "caml_array_unsafe_get", [ x; y ] -> Memory.gen_array_get x y
            | Extern "caml_floatarray_unsafe_get", [ x; y ] -> Memory.float_array_get x y
            | Extern "caml_array_unsafe_set", [ x; y; z ] ->
                seq (Memory.gen_array_set x y z) Value.unit
            | Extern "caml_array_unsafe_set_addr", [ x; y; z ] ->
                seq (Memory.array_set x y z) Value.unit
            | Extern "caml_floatarray_unsafe_set", [ x; y; z ] ->
                seq (Memory.float_array_set x y z) Value.unit
            | Extern ("caml_string_unsafe_get" | "caml_bytes_unsafe_get"), [ x; y ] ->
                Memory.bytes_get x y
            | Extern ("caml_string_unsafe_set" | "caml_bytes_unsafe_set"), [ x; y; z ] ->
                seq (Memory.bytes_set x y z) Value.unit
            | Extern ("caml_string_get" | "caml_bytes_get"), [ x; y ] ->
                seq
                  (let* cond = Arith.uge (Value.int_val y) (Memory.bytes_length x) in
                   instr (W.Br_if (label_index context bound_error_pc, cond)))
                  (Memory.bytes_get x y)
            | Extern ("caml_string_set" | "caml_bytes_set"), [ x; y; z ] ->
                seq
                  (let* cond = Arith.uge (Value.int_val y) (Memory.bytes_length x) in
                   let* () = instr (W.Br_if (label_index context bound_error_pc, cond)) in
                   Memory.bytes_set x y z)
                  Value.unit
            | Extern ("caml_ml_string_length" | "caml_ml_bytes_length"), [ x ] ->
                Value.val_int (Memory.bytes_length x)
            | Extern "%int_add", [ x; y ] -> Value.int_add x y
            | Extern "%int_sub", [ x; y ] -> Value.int_sub x y
            | Extern ("%int_mul" | "%direct_int_mul"), [ x; y ] -> Value.int_mul x y
            | Extern "%direct_int_div", [ x; y ] -> Value.int_div x y
            | Extern "%int_div", [ x; y ] ->
                seq
                  (let* cond = Arith.eqz (Value.int_val y) in
                   instr (W.Br_if (label_index context zero_divide_pc, cond)))
                  (Value.int_div x y)
            | Extern "%int_mod", [ x; y ] ->
                seq
                  (let* cond = Arith.eqz (Value.int_val y) in
                   instr (W.Br_if (label_index context zero_divide_pc, cond)))
                  (Value.int_mod x y)
            | Extern "%direct_int_mod", [ x; y ] -> Value.int_mod x y
            | Extern "%int_neg", [ x ] -> Value.int_neg x
            | Extern "%int_or", [ x; y ] -> Value.int_or x y
            | Extern "%int_and", [ x; y ] -> Value.int_and x y
            | Extern "%int_xor", [ x; y ] -> Value.int_xor x y
            | Extern "%int_lsl", [ x; y ] -> Value.int_lsl x y
            | Extern "%int_lsr", [ x; y ] -> Value.int_lsr x y
            | Extern "%int_asr", [ x; y ] -> Value.int_asr x y
            | Extern "%direct_obj_tag", [ x ] -> Memory.tag x
            | Extern "caml_check_bound", [ x; y ] ->
                seq
                  (let* cond = Arith.uge (Value.int_val y) (Memory.array_length x) in
                   instr (W.Br_if (label_index context bound_error_pc, cond)))
                  x
            | Extern "caml_check_bound_gen", [ x; y ] ->
                seq
                  (let* cond = Arith.uge (Value.int_val y) (Memory.gen_array_length x) in
                   instr (W.Br_if (label_index context bound_error_pc, cond)))
                  x
            | Extern "caml_check_bound_float", [ x; y ] ->
                seq
                  (let* cond =
                     Arith.uge (Value.int_val y) (Memory.float_array_length x)
                   in
                   instr (W.Br_if (label_index context bound_error_pc, cond)))
                  x
            | Extern "caml_add_float", [ f; g ] -> float_bin_op stack_ctx x Add f g
            | Extern "caml_sub_float", [ f; g ] -> float_bin_op stack_ctx x Sub f g
            | Extern "caml_mul_float", [ f; g ] -> float_bin_op stack_ctx x Mul f g
            | Extern "caml_div_float", [ f; g ] -> float_bin_op stack_ctx x Div f g
            | Extern "caml_copysign_float", [ f; g ] ->
                float_bin_op stack_ctx x CopySign f g
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
                Memory.box_float
                  stack_ctx
                  x
                  (return (W.UnOp (F64 (Convert (`I32, S)), n)))
            | Extern "caml_cos_float", [ f ] -> float_un_op' stack_ctx x Math.cos f
            | Extern "caml_sin_float", [ f ] -> float_un_op' stack_ctx x Math.sin f
            | Extern "caml_tan_float", [ f ] -> float_un_op' stack_ctx x Math.tan f
            | Extern "caml_acos_float", [ f ] -> float_un_op' stack_ctx x Math.acos f
            | Extern "caml_asin_float", [ f ] -> float_un_op' stack_ctx x Math.asin f
            | Extern "caml_atan_float", [ f ] -> float_un_op' stack_ctx x Math.atan f
            | Extern "caml_atan2_float", [ f; g ] ->
                float_bin_op' stack_ctx x Math.atan2 f g
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
            | Extern "caml_power_float", [ f; g ] ->
                float_bin_op' stack_ctx x Math.power f g
            | Extern "caml_hypot_float", [ f; g ] ->
                float_bin_op' stack_ctx x Math.hypot f g
            | Extern "caml_fmod_float", [ f; g ] ->
                float_bin_op' stack_ctx x Math.fmod f g
            | Extern "caml_int32_bits_of_float", [ f ] ->
                let* f = Memory.unbox_float f in
                Memory.box_int32
                  stack_ctx
                  x
                  (return (W.UnOp (I32 ReinterpretF, F32DemoteF64 f)))
            | Extern "caml_int32_float_of_bits", [ i ] ->
                let* i = Memory.unbox_int32 i in
                Memory.box_float
                  stack_ctx
                  x
                  (return (W.F64PromoteF32 (UnOp (F32 ReinterpretI, i))))
            | Extern "caml_int32_of_float", [ f ] ->
                let* f = Memory.unbox_float f in
                Memory.box_int32 stack_ctx x (return (W.UnOp (I32 (TruncSatF64 S), f)))
            | Extern "caml_int32_to_float", [ n ] ->
                let* n = Memory.unbox_int32 n in
                Memory.box_float
                  stack_ctx
                  x
                  (return (W.UnOp (F64 (Convert (`I32, S)), n)))
            | Extern "caml_int32_neg", [ i ] ->
                let* i = Memory.unbox_int32 i in
                Memory.box_int32
                  stack_ctx
                  x
                  (return (W.BinOp (I32 Sub, Const (I32 0l), i)))
            | Extern "caml_int32_add", [ i; j ] -> int32_bin_op stack_ctx x Add i j
            | Extern "caml_int32_sub", [ i; j ] -> int32_bin_op stack_ctx x Sub i j
            | Extern "caml_int32_mul", [ i; j ] -> int32_bin_op stack_ctx x Mul i j
            | Extern "caml_int32_and", [ i; j ] -> int32_bin_op stack_ctx x And i j
            | Extern "caml_int32_or", [ i; j ] -> int32_bin_op stack_ctx x Or i j
            | Extern "caml_int32_xor", [ i; j ] -> int32_bin_op stack_ctx x Xor i j
            | Extern "caml_int32_div", [ i; j ] ->
                let res = Var.fresh () in
                (*ZZZ Can we do better?*)
                let i' = Var.fresh () in
                let j' = Var.fresh () in
                seq
                  (let* () = store ~typ:I32 j' (Memory.unbox_int32 j) in
                   let* () =
                     let* j = load j' in
                     instr
                       (W.Br_if (label_index context zero_divide_pc, W.UnOp (I32 Eqz, j)))
                   in
                   let* () = store ~typ:I32 i' (Memory.unbox_int32 i) in
                   if_
                     { params = []; result = [] }
                     Arith.(
                       (let* j = load j' in
                        return (W.BinOp (I32 Eq, j, Const (I32 (-1l)))))
                       land let* i = load i' in
                            return (W.BinOp (I32 Eq, i, Const (I32 Int32.min_int))))
                     (store
                        ~always:true
                        ~typ:I32
                        res
                        (return (W.Const (I32 Int32.min_int))))
                     (store
                        ~always:true
                        ~typ:I32
                        res
                        (let* i = load i' in
                         let* j = load j' in
                         return (W.BinOp (I32 (Div S), i, j)))))
                  (Memory.box_int32 stack_ctx x (load res))
            | Extern "caml_int32_mod", [ i; j ] ->
                let j' = Var.fresh () in
                seq
                  (let* () = store ~typ:I32 j' (Memory.unbox_int32 j) in
                   let* j = load j' in
                   instr
                     (W.Br_if (label_index context zero_divide_pc, W.UnOp (I32 Eqz, j))))
                  (let* i = Memory.unbox_int32 i in
                   let* j = load j' in
                   Memory.box_int32 stack_ctx x (return (W.BinOp (I32 (Rem S), i, j))))
            | Extern "caml_int32_shift_left", [ i; j ] ->
                int32_shift_op stack_ctx x Shl i j
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
                Memory.box_float
                  stack_ctx
                  x
                  (return (W.UnOp (F64 (Convert (`I64, S)), n)))
            | Extern "caml_int64_neg", [ i ] ->
                let* i = Memory.unbox_int64 i in
                Memory.box_int64
                  stack_ctx
                  x
                  (return (W.BinOp (I64 Sub, Const (I64 0L), i)))
            | Extern "caml_int64_add", [ i; j ] -> int64_bin_op stack_ctx x Add i j
            | Extern "caml_int64_sub", [ i; j ] -> int64_bin_op stack_ctx x Sub i j
            | Extern "caml_int64_mul", [ i; j ] -> int64_bin_op stack_ctx x Mul i j
            | Extern "caml_int64_and", [ i; j ] -> int64_bin_op stack_ctx x And i j
            | Extern "caml_int64_or", [ i; j ] -> int64_bin_op stack_ctx x Or i j
            | Extern "caml_int64_xor", [ i; j ] -> int64_bin_op stack_ctx x Xor i j
            | Extern "caml_int64_div", [ i; j ] ->
                let res = Var.fresh () in
                (*ZZZ Can we do better?*)
                let i' = Var.fresh () in
                let j' = Var.fresh () in
                seq
                  (let* () = store ~typ:I64 j' (Memory.unbox_int64 j) in
                   let* () =
                     let* j = load j' in
                     instr
                       (W.Br_if (label_index context zero_divide_pc, W.UnOp (I64 Eqz, j)))
                   in
                   let* () = store ~typ:I64 i' (Memory.unbox_int64 i) in
                   if_
                     { params = []; result = [] }
                     Arith.(
                       (let* j = load j' in
                        return (W.BinOp (I64 Eq, j, Const (I64 (-1L)))))
                       land let* i = load i' in
                            return (W.BinOp (I64 Eq, i, Const (I64 Int64.min_int))))
                     (store
                        ~always:true
                        ~typ:I64
                        res
                        (return (W.Const (I64 Int64.min_int))))
                     (store
                        ~always:true
                        ~typ:I64
                        res
                        (let* i = load i' in
                         let* j = load j' in
                         return (W.BinOp (I64 (Div S), i, j)))))
                  (Memory.box_int64 stack_ctx x (load res))
            | Extern "caml_int64_mod", [ i; j ] ->
                let j' = Var.fresh () in
                seq
                  (let* () = store ~typ:I64 j' (Memory.unbox_int64 j) in
                   let* j = load j' in
                   instr
                     (W.Br_if (label_index context zero_divide_pc, W.UnOp (I64 Eqz, j))))
                  (let* i = Memory.unbox_int64 i in
                   let* j = load j' in
                   Memory.box_int64 stack_ctx x (return (W.BinOp (I64 (Rem S), i, j))))
            | Extern "caml_int64_shift_left", [ i; j ] ->
                int64_shift_op stack_ctx x Shl i j
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
                Memory.box_nativeint
                  stack_ctx
                  x
                  (return (W.UnOp (I32 (TruncSatF64 S), f)))
            | Extern "caml_nativeint_to_float", [ n ] ->
                let* n = Memory.unbox_nativeint n in
                Memory.box_float
                  stack_ctx
                  x
                  (return (W.UnOp (F64 (Convert (`I32, S)), n)))
            | Extern "caml_nativeint_neg", [ i ] ->
                let* i = Memory.unbox_nativeint i in
                Memory.box_nativeint
                  stack_ctx
                  x
                  (return (W.BinOp (I32 Sub, Const (I32 0l), i)))
            | Extern "caml_nativeint_add", [ i; j ] ->
                nativeint_bin_op stack_ctx x Add i j
            | Extern "caml_nativeint_sub", [ i; j ] ->
                nativeint_bin_op stack_ctx x Sub i j
            | Extern "caml_nativeint_mul", [ i; j ] ->
                nativeint_bin_op stack_ctx x Mul i j
            | Extern "caml_nativeint_and", [ i; j ] ->
                nativeint_bin_op stack_ctx x And i j
            | Extern "caml_nativeint_or", [ i; j ] -> nativeint_bin_op stack_ctx x Or i j
            | Extern "caml_nativeint_xor", [ i; j ] ->
                nativeint_bin_op stack_ctx x Xor i j
            | Extern "caml_nativeint_div", [ i; j ] ->
                let res = Var.fresh () in
                (*ZZZ Can we do better?*)
                let i' = Var.fresh () in
                let j' = Var.fresh () in
                seq
                  (let* () = store ~typ:I32 j' (Memory.unbox_nativeint j) in
                   let* () =
                     let* j = load j' in
                     instr
                       (W.Br_if (label_index context zero_divide_pc, W.UnOp (I32 Eqz, j)))
                   in
                   let* () = store ~typ:I32 i' (Memory.unbox_nativeint i) in
                   if_
                     { params = []; result = [] }
                     Arith.(
                       (let* j = load j' in
                        return (W.BinOp (I32 Eq, j, Const (I32 (-1l)))))
                       land let* i = load i' in
                            return (W.BinOp (I32 Eq, i, Const (I32 Int32.min_int))))
                     (store
                        ~always:true
                        ~typ:I32
                        res
                        (return (W.Const (I32 Int32.min_int))))
                     (store
                        ~always:true
                        ~typ:I32
                        res
                        (let* i = load i' in
                         let* j = load j' in
                         return (W.BinOp (I32 (Div S), i, j)))))
                  (Memory.box_nativeint stack_ctx x (load res))
            | Extern "caml_nativeint_mod", [ i; j ] ->
                let j' = Var.fresh () in
                seq
                  (let* () = store ~typ:I32 j' (Memory.unbox_nativeint j) in
                   let* j = load j' in
                   instr
                     (W.Br_if (label_index context zero_divide_pc, W.UnOp (I32 Eqz, j))))
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
                    (Value.int_val j < Value.int_val i)
                    - (Value.int_val i < Value.int_val j))
            | Extern "%js_array", l ->
                let* l =
                  List.fold_right
                    ~f:(fun x acc ->
                      let* x = x in
                      let* acc = acc in
                      return (`Expr x :: acc))
                    l
                    ~init:(return [])
                in
                Memory.allocate stack_ctx x ~tag:0 l
            | Extern name, l ->
                let name = Primitive.resolve name in
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
            | Vectlength, [ x ] -> Value.val_int (Memory.gen_array_length x)
            | (Not | Lt | Le | Eq | Neq | Ult | Array_get | IsInt | Vectlength), _ ->
                assert false))

  and translate_instr ctx stack_ctx context (i, loc) =
    with_location
      loc
      (match i with
      | Assign (x, y) ->
          let* () = assign x (load y) in
          Stack.assign stack_ctx x
      | Let (x, e) ->
          if ctx.live.(Var.idx x) = 0
          then drop (translate_expr ctx stack_ctx context x e)
          else store x (translate_expr ctx stack_ctx context x e)
      | Set_field (x, n, y) -> Memory.set_field (load x) n (load y)
      | Offset_ref (x, n) ->
          Memory.set_field
            (load x)
            0
            (Value.val_int
               Arith.(Value.int_val (Memory.field (load x) 0) + const (Int32.of_int n)))
      | Array_set (x, y, z) -> Memory.array_set (load x) (load y) (load z))

  and translate_instrs ctx stack_ctx context l =
    match l with
    | [] -> return ()
    | i :: rem ->
        let* () = Stack.perform_reloads stack_ctx (`Instr (fst i)) in
        let* () = translate_instr ctx stack_ctx context i in
        translate_instrs ctx stack_ctx context rem

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

  let needed_handlers (p : program) pc =
    Code.traverse
      { fold = fold_children_skip_try_body }
      (fun pc n ->
        let block = Addr.Map.find pc p.blocks in
        List.fold_left
          ~f:(fun n (i, _) ->
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
      acc =
    let ctx =
      let loc = Before pc in
      match Parse_bytecode.Debug.find_loc ctx.debug loc with
      | Some _ ->
          let block = Addr.Map.find pc ctx.blocks in
          let block =
            match block.body with
            | (i, _) :: rem -> { block with body = (i, loc) :: rem }
            | [] -> { block with branch = fst block.branch, loc }
          in
          let blocks = Addr.Map.add pc block ctx.blocks in
          { ctx with blocks }
      | None -> ctx
    in
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
    let rec translate_tree result_typ fall_through pc context =
      let block = Addr.Map.find pc ctx.blocks in
      let keep_ouside pc' =
        match fst block.branch with
        | Switch _ -> true
        | Cond (_, (pc1, _), (pc2, _)) when pc' = pc1 && pc' = pc2 -> true
        | _ -> Wa_structure.is_merge_node g pc'
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
            |> List.filter ~f:keep_ouside
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
      | [] ->
          let block = Addr.Map.find pc ctx.blocks in
          let* global_context = get_context in
          let stack_ctx = Stack.start_block ~context:global_context stack_info pc in
          let* () = translate_instrs ctx stack_ctx context block.body in
          let* () = Stack.perform_reloads stack_ctx (`Branch (fst block.branch)) in
          let* () = Stack.perform_spilling stack_ctx (`Block pc) in
          let branch, loc = block.branch in
          with_location
            loc
            (match branch with
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
            | Switch (x, a1) ->
                let l =
                  List.filter
                    ~f:(fun pc' ->
                      Stack.stack_adjustment_needed stack_ctx ~src:pc ~dst:pc')
                    (List.rev (Addr.Set.elements (Wa_structure.get_edges dom pc)))
                in
                let br_table e a context =
                  let len = Array.length a in
                  let l = Array.to_list (Array.sub a ~pos:0 ~len:(len - 1)) in
                  let dest (pc, args) =
                    assert (List.is_empty args);
                    label_index context pc
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
                      instr (Br (label_index context pc', None))
                  | [] -> br_table (Value.int_val (load x)) a1 context
                in
                nest l context
            | Raise (x, _) ->
                let* e = load x in
                let* tag = register_import ~name:exception_name (Tag Value.value) in
                instr (Throw (tag, e))
            | Pushtrap (cont, x, cont') ->
                handle_exceptions
                  ~result_typ
                  ~fall_through
                  ~context:(extend_context fall_through context)
                  (wrap_with_handlers
                     p
                     (fst cont)
                     (fun ~result_typ ~fall_through ~context ->
                       translate_branch result_typ fall_through pc cont context stack_ctx))
                  x
                  (fun ~result_typ ~fall_through ~context ->
                    translate_branch result_typ fall_through pc cont' context stack_ctx)
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
      match fall_through with
      | `Block dst' when dst = dst' -> return ()
      | _ ->
          if (src >= 0 && Wa_structure.is_backward g src dst)
             || Wa_structure.is_merge_node g dst
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
    | None -> ctx.global_context.globalized_variables <- Wa_globalize.f p g ctx.closures
    | Some _ -> ());
    let locals, body =
      function_body
        ~context:ctx.global_context
        ~param_names
        ~body:
          (let* () = build_initial_env in
           let stack_ctx = Stack.start_function ~context:ctx.global_context stack_info in
           let* () = Stack.perform_spilling stack_ctx `Function in
           wrap_with_handlers
             p
             pc
             ~result_typ:[ Value.value ]
             ~fall_through:`Return
             ~context:[]
             (fun ~result_typ ~fall_through ~context ->
               translate_branch result_typ fall_through (-1) cont context stack_ctx))
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
      ; param_names
      ; typ = func_type param_count
      ; locals
      ; body
      }
    :: acc

  let init_function ~context ~to_link =
    let name = Code.Var.fresh_n "initialize" in
    let typ = { W.params = []; result = [ Value.value ] } in
    let locals, body =
      function_body
        ~context
        ~param_names:[]
        ~body:
          (List.fold_right
             ~f:(fun name cont ->
               let* f =
                 register_import ~import_module:"OCaml" ~name:(name ^ ".init") (Fun typ)
               in
               let* () = instr (Drop (Call (f, []))) in
               cont)
             ~init:(instr (Push (RefI31 (Const (I32 0l)))))
             to_link)
    in
    context.other_fields <-
      W.Function { name; exported_name = None; typ; param_names = []; locals; body }
      :: context.other_fields;
    name

  let entry_point context toplevel_fun entry_name =
    let typ, param_names, body = entry_point ~toplevel_fun in
    let locals, body = function_body ~context ~param_names ~body in
    W.Function
      { name = Var.fresh_n "entry_point"
      ; exported_name = Some entry_name
      ; typ
      ; param_names
      ; locals
      ; body
      }

  module Curry = Wa_curry.Make (Target)

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
      ~debug =
    global_context.unit_name <- unit_name;
    let p, closures = Wa_closure_conversion.f p in
    (*
  Code.Print.program (fun _ _ -> "") p;
*)
    let ctx =
      { live = live_vars; in_cps; blocks = p.blocks; closures; global_context; debug }
    in
    let toplevel_name = Var.fresh_n "toplevel" in
    let functions =
      Code.fold_closures_outermost_first
        p
        (fun name_opt params cont ->
          translate_function p ctx name_opt ~toplevel_name ~unit_name params cont)
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
        ~f:(fun (name, (active, contents)) ->
          W.Data { name; read_only = true; active; contents })
        (Var.Map.bindings context.data_segments)
    in
    List.rev_append context.other_fields (imports @ constant_data)
end

let init () =
  let l =
    [ "caml_make_array", "%identity"
    ; "caml_ensure_stack_capacity", "%identity"
    ; "caml_callback", "caml_trampoline"
    ]
  in

  let l =
    if Config.Flag.effects ()
    then ("caml_alloc_stack", "caml_cps_alloc_stack") :: l
    else l
  in
  List.iter ~f:(fun (nm, nm') -> Primitive.alias nm nm') l

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
               try List.assoc args l
               with Not_found ->
                 let pc' = !p'.free_pc in
                 p' :=
                   { !p' with
                     blocks =
                       Addr.Map.add
                         pc'
                         { params = []; body = []; branch = Branch cont, No }
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
      match fst block.branch with
      | Switch (_, l) -> fix_branches l
      | _ -> ())
    p.blocks;
  !p'

let start () =
  make_context
    ~value_type:
      (match target with
      | `Core -> Wa_core_target.Value.value
      | `GC -> Wa_gc_target.Value.value)

let f ~context ~unit_name p ~live_vars ~in_cps ~debug =
  let p = if Config.Flag.effects () then fix_switch_branches p else p in
  match target with
  | `Core ->
      let module G = Generate (Wa_core_target) in
      G.f ~context ~unit_name ~live_vars ~in_cps ~debug p
  | `GC ->
      let module G = Generate (Wa_gc_target) in
      G.f ~context ~unit_name ~live_vars ~in_cps ~debug p

let add_start_function =
  match target with
  | `Core ->
      let module G = Generate (Wa_core_target) in
      G.add_start_function
  | `GC ->
      let module G = Generate (Wa_gc_target) in
      G.add_start_function

let add_init_function =
  match target with
  | `Core ->
      let module G = Generate (Wa_core_target) in
      G.add_init_function
  | `GC ->
      let module G = Generate (Wa_gc_target) in
      G.add_init_function

let output ch ~context ~debug =
  match target with
  | `Core ->
      let module G = Generate (Wa_core_target) in
      let fields = G.output ~context in
      Wa_asm_output.f ch fields
  | `GC ->
      let module G = Generate (Wa_gc_target) in
      let fields = G.output ~context in
      Wa_wat_output.f ~debug ch fields
