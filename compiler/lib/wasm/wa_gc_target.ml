open! Stdlib
module W = Wa_ast
open Wa_code_generation

type expression = Wa_ast.expression Wa_code_generation.t

let include_closure_arity = false

module Type = struct
  let value = W.Ref { nullable = false; typ = Eq }

  let block_type =
    register_type "block" (fun () ->
        return
          { supertype = None
          ; final = true
          ; typ = W.Array { mut = true; typ = Value value }
          })

  let string_type =
    register_type "string" (fun () ->
        return
          { supertype = None
          ; final = true
          ; typ = W.Array { mut = true; typ = Packed I8 }
          })

  let float_type =
    register_type "float" (fun () ->
        return
          { supertype = None
          ; final = true
          ; typ = W.Struct [ { mut = false; typ = Value F64 } ]
          })

  let float_array_type =
    register_type "float_array" (fun () ->
        return
          { supertype = None
          ; final = true
          ; typ = W.Array { mut = true; typ = Value F64 }
          })

  let js_type =
    register_type "js" (fun () ->
        return
          { supertype = None
          ; final = true
          ; typ =
              W.Struct
                [ { mut = false; typ = Value (Ref { nullable = true; typ = Any }) } ]
          })

  let compare_type =
    register_type "compare" (fun () ->
        return
          { supertype = None
          ; final = true
          ; typ = W.Func { W.params = [ value; value; I32 ]; result = [ I32 ] }
          })

  let hash_type =
    register_type "hash" (fun () ->
        return
          { supertype = None
          ; final = true
          ; typ = W.Func { W.params = [ value ]; result = [ I32 ] }
          })

  let fixed_length_type =
    register_type "fixed_length" (fun () ->
        return
          { supertype = None
          ; final = true
          ; typ =
              W.Struct
                [ { mut = false; typ = Value I32 }; { mut = false; typ = Value I32 } ]
          })

  let serialize_type =
    register_type "serialize" (fun () ->
        return
          { supertype = None
          ; final = true
          ; typ = W.Func { W.params = [ value; value ]; result = [ I32; I32 ] }
          })

  let deserialize_type =
    register_type "deserialize" (fun () ->
        return
          { supertype = None
          ; final = true
          ; typ = W.Func { W.params = [ value ]; result = [ value; I32 ] }
          })

  let dup_type =
    register_type "dup" (fun () ->
        return
          { supertype = None
          ; final = true
          ; typ = W.Func { W.params = [ value ]; result = [ value ] }
          })

  let custom_operations_type =
    register_type "custom_operations" (fun () ->
        let* string = string_type in
        let* compare = compare_type in
        let* hash = hash_type in
        let* fixed_length = fixed_length_type in
        let* serialize = serialize_type in
        let* deserialize = deserialize_type in
        let* dup = dup_type in
        return
          { supertype = None
          ; final = true
          ; typ =
              W.Struct
                [ { mut = false
                  ; typ = Value (Ref { nullable = false; typ = Type string })
                  }
                ; { mut = false
                  ; typ = Value (Ref { nullable = true; typ = Type compare })
                  }
                ; { mut = false
                  ; typ = Value (Ref { nullable = true; typ = Type compare })
                  }
                ; { mut = false; typ = Value (Ref { nullable = true; typ = Type hash }) }
                ; { mut = false
                  ; typ = Value (Ref { nullable = true; typ = Type fixed_length })
                  }
                ; { mut = false
                  ; typ = Value (Ref { nullable = true; typ = Type serialize })
                  }
                ; { mut = false
                  ; typ = Value (Ref { nullable = true; typ = Type deserialize })
                  }
                ; { mut = false; typ = Value (Ref { nullable = true; typ = Type dup }) }
                ]
          })

  let custom_type =
    register_type "custom" (fun () ->
        let* custom_operations = custom_operations_type in
        return
          { supertype = None
          ; final = false
          ; typ =
              W.Struct
                [ { mut = false
                  ; typ = Value (Ref { nullable = false; typ = Type custom_operations })
                  }
                ]
          })

  let int32_type =
    register_type "int32" (fun () ->
        let* custom_operations = custom_operations_type in
        let* custom = custom_type in
        return
          { supertype = Some custom
          ; final = true
          ; typ =
              W.Struct
                [ { mut = false
                  ; typ = Value (Ref { nullable = false; typ = Type custom_operations })
                  }
                ; { mut = false; typ = Value I32 }
                ]
          })

  let int64_type =
    register_type "int64" (fun () ->
        let* custom_operations = custom_operations_type in
        let* custom = custom_type in
        return
          { supertype = Some custom
          ; final = true
          ; typ =
              W.Struct
                [ { mut = false
                  ; typ = Value (Ref { nullable = false; typ = Type custom_operations })
                  }
                ; { mut = false; typ = Value I64 }
                ]
          })

  let func_type n =
    { W.params = List.init ~len:(n + 1) ~f:(fun _ -> value); result = [ value ] }

  let function_type ~cps n =
    let n = if cps then n + 1 else n in
    register_type (Printf.sprintf "function_%d" n) (fun () ->
        return { supertype = None; final = true; typ = W.Func (func_type n) })

  let closure_common_fields ~cps =
    let* fun_ty = function_type ~cps 1 in
    return
      (let function_pointer =
         [ { W.mut = false; typ = W.Value (Ref { nullable = false; typ = Type fun_ty }) }
         ]
       in
       if include_closure_arity
       then { W.mut = false; typ = W.Value I32 } :: function_pointer
       else function_pointer)

  let closure_type_1 ~cps =
    register_type
      (if cps then "cps_closure" else "closure")
      (fun () ->
        let* fields = closure_common_fields ~cps in
        return { supertype = None; final = false; typ = W.Struct fields })

  let closure_last_arg_type ~cps =
    register_type
      (if cps then "cps_closure_last_arg" else "closure_last_arg")
      (fun () ->
        let* cl_typ = closure_type_1 ~cps in
        let* fields = closure_common_fields ~cps in
        return { supertype = Some cl_typ; final = false; typ = W.Struct fields })

  let closure_type ~usage ~cps arity =
    if arity = 1
    then
      match usage with
      | `Alloc -> closure_last_arg_type ~cps
      | `Access -> closure_type_1 ~cps
    else if arity = 0
    then
      register_type
        (if cps then "cps_closure_0" else "closure_0")
        (fun () ->
          let* fun_ty' = function_type ~cps arity in
          return
            { supertype = None
            ; final = false
            ; typ =
                W.Struct
                  [ { mut = false
                    ; typ = Value (Ref { nullable = false; typ = Type fun_ty' })
                    }
                  ]
            })
    else
      register_type
        (if cps
         then Printf.sprintf "cps_closure_%d" arity
         else Printf.sprintf "closure_%d" arity)
        (fun () ->
          let* cl_typ = closure_type_1 ~cps in
          let* common = closure_common_fields ~cps in
          let* fun_ty' = function_type ~cps arity in
          return
            { supertype = Some cl_typ
            ; final = false
            ; typ =
                W.Struct
                  (common
                  @ [ { mut = false
                      ; typ = Value (Ref { nullable = false; typ = Type fun_ty' })
                      }
                    ])
            })

  let env_type ~cps ~arity n =
    register_type
      (if cps
       then Printf.sprintf "cps_env_%d_%d" arity n
       else Printf.sprintf "env_%d_%d" arity n)
      (fun () ->
        let* cl_typ = closure_type ~usage:`Alloc ~cps arity in
        let* common = closure_common_fields ~cps in
        let* fun_ty' = function_type ~cps arity in
        return
          { supertype = Some cl_typ
          ; final = true
          ; typ =
              W.Struct
                ((if arity = 1
                  then common
                  else if arity = 0
                  then
                    [ { mut = false
                      ; typ = Value (Ref { nullable = false; typ = Type fun_ty' })
                      }
                    ]
                  else
                    common
                    @ [ { mut = false
                        ; typ = Value (Ref { nullable = false; typ = Type fun_ty' })
                        }
                      ])
                @ List.init
                    ~f:(fun _ ->
                      { W.mut = false
                      ; typ = W.Value (Ref { nullable = false; typ = Eq })
                      })
                    ~len:n)
          })

  let rec_env_type ~function_count ~free_variable_count =
    register_type
      (Printf.sprintf "rec_env_%d_%d" function_count free_variable_count)
      (fun () ->
        return
          { supertype = None
          ; final = true
          ; typ =
              W.Struct
                (List.init
                   ~f:(fun i ->
                     { W.mut = i < function_count
                     ; typ = W.Value (Ref { nullable = false; typ = Eq })
                     })
                   ~len:(function_count + free_variable_count))
          })

  let rec_closure_type ~cps ~arity ~function_count ~free_variable_count =
    register_type
      (if cps
       then
         Printf.sprintf
           "cps_closure_rec_%d_%d_%d"
           arity
           function_count
           free_variable_count
       else Printf.sprintf "closure_rec_%d_%d_%d" arity function_count free_variable_count)
      (fun () ->
        let* cl_typ = closure_type ~usage:`Alloc ~cps arity in
        let* common = closure_common_fields ~cps in
        let* fun_ty' = function_type ~cps arity in
        let* env_ty = rec_env_type ~function_count ~free_variable_count in
        return
          { supertype = Some cl_typ
          ; final = true
          ; typ =
              W.Struct
                ((if arity = 1
                  then common
                  else
                    common
                    @ [ { mut = false
                        ; typ = Value (Ref { nullable = false; typ = Type fun_ty' })
                        }
                      ])
                @ [ { W.mut = false
                    ; typ = W.Value (Ref { nullable = false; typ = Type env_ty })
                    }
                  ])
          })

  let rec curry_type ~cps arity m =
    register_type
      (if cps
       then Printf.sprintf "cps_curry_%d_%d" arity m
       else Printf.sprintf "curry_%d_%d" arity m)
      (fun () ->
        let* cl_typ = closure_type ~usage:(if m = 2 then `Alloc else `Access) ~cps 1 in
        let* common = closure_common_fields ~cps in
        let* cl_ty =
          if m = arity
          then closure_type ~usage:`Alloc ~cps arity
          else curry_type ~cps arity (m + 1)
        in
        return
          { supertype = Some cl_typ
          ; final = true
          ; typ =
              W.Struct
                (common
                @ [ { mut = false
                    ; typ = Value (Ref { nullable = false; typ = Type cl_ty })
                    }
                  ; { W.mut = false; typ = Value value }
                  ])
          })

  let dummy_closure_type ~cps ~arity =
    register_type
      (if cps
       then Printf.sprintf "cps_dummy_closure_%d" arity
       else Printf.sprintf "dummy_closure_%d" arity)
      (fun () ->
        let* cl_typ = closure_type ~cps ~usage:`Alloc arity in
        let* cl_typ' = closure_type ~cps ~usage:`Access arity in
        let* common = closure_common_fields ~cps in
        let* fun_ty' = function_type ~cps arity in
        return
          { supertype = Some cl_typ
          ; final = true
          ; typ =
              W.Struct
                ((if arity = 1
                  then common
                  else
                    common
                    @ [ { mut = false
                        ; typ = Value (Ref { nullable = false; typ = Type fun_ty' })
                        }
                      ])
                @ [ { W.mut = true
                    ; typ = W.Value (Ref { nullable = true; typ = Type cl_typ' })
                    }
                  ])
          })
end

module Value = struct
  let value = Type.value

  let unit = return (W.RefI31 (Const (I32 0l)))

  let val_int = Arith.to_int31

  let int_val i = Arith.of_int31 (cast I31 i)

  let check_is_not_zero i =
    let* i = i in
    match i with
    | W.LocalGet x -> (
        let* x_opt = get_i31_value x in
        match x_opt with
        | Some x' -> return (W.LocalGet x')
        | None -> return (W.UnOp (I32 Eqz, RefEq (i, W.RefI31 (Const (I32 0l))))))
    | _ -> return (W.UnOp (I32 Eqz, RefEq (i, W.RefI31 (Const (I32 0l)))))

  let check_is_int i =
    let* i = i in
    return (W.RefTest ({ nullable = false; typ = I31 }, i))

  let not i = val_int (Arith.eqz (int_val i))

  let binop op i i' = val_int (op (int_val i) (int_val i'))

  let lt = binop Arith.( < )

  let le = binop Arith.( <= )

  let eq i i' =
    let* i = i in
    let* i' = i' in
    val_int (return (W.RefEq (i, i')))

  let neq i i' =
    let* i = i in
    let* i' = i' in
    val_int (Arith.eqz (return (W.RefEq (i, i'))))

  let ult = binop Arith.(ult)

  let is_int i =
    let* i = i in
    val_int (return (W.RefTest ({ nullable = false; typ = I31 }, i)))

  let int_add = binop Arith.( + )

  let int_sub = binop Arith.( - )

  let int_mul = binop Arith.( * )

  let int_div = binop Arith.( / )

  let int_mod = binop Arith.( mod )

  let int_neg i = val_int Arith.(const 0l - int_val i)

  let int_or = binop Arith.( lor )

  let int_and = binop Arith.( land )

  let int_xor = binop Arith.( lxor )

  let int_lsl = binop Arith.( lsl )

  let int_lsr i i' = val_int Arith.((int_val i land const 0x7fffffffl) lsr int_val i')

  let int_asr = binop Arith.( asr )
end

module Memory = struct
  let wasm_cast ty e =
    let* e = e in
    match e with
    | W.LocalGet x ->
        return
          (W.RefCast
             ( { nullable = false; typ = Type ty }
             , W.LocalTee (x, W.RefCast ({ nullable = false; typ = Type ty }, e)) ))
    | _ -> return (W.RefCast ({ nullable = false; typ = Type ty }, e))

  let wasm_struct_get ty e i =
    let* e = e in
    match e with
    | W.RefCast (_, GlobalGet (V nm)) -> (
        let* init = get_global nm in
        match init with
        | Some (W.StructNew (_, l)) ->
            let e = List.nth l i in
            let* b = is_small_constant e in
            if b then return e else return (W.StructGet (None, ty, i, e))
        | _ -> return (W.StructGet (None, ty, i, e)))
    | _ -> return (W.StructGet (None, ty, i, e))

  let wasm_struct_set ty e i e' =
    let* e = e in
    let* e' = e' in
    instr (W.StructSet (ty, i, e, e'))

  let wasm_array_get ?(ty = Type.block_type) e e' =
    let* ty = ty in
    let* e = wasm_cast ty e in
    let* e' = e' in
    return (W.ArrayGet (None, ty, e, e'))

  let wasm_array_set ?(ty = Type.block_type) e e' e'' =
    let* ty = ty in
    let* e = wasm_cast ty e in
    let* e' = e' in
    let* e'' = e'' in
    instr (W.ArraySet (ty, e, e', e''))

  let box_float _ _ e =
    let* ty = Type.float_type in
    let* e = e in
    return (W.StructNew (ty, [ e ]))

  let unbox_float e =
    let* ty = Type.float_type in
    wasm_struct_get ty (wasm_cast ty e) 0

  let allocate _ _ ~tag l =
    if tag = 254
    then
      let* l =
        expression_list
          (fun v ->
            unbox_float
              (match v with
              | `Var y -> load y
              | `Expr e -> return e))
          l
      in
      let* ty = Type.float_array_type in
      return (W.ArrayNewFixed (ty, l))
    else
      let* l =
        expression_list
          (fun v ->
            match v with
            | `Var y -> load y
            | `Expr e -> return e)
          l
      in
      let* ty = Type.block_type in
      return (W.ArrayNewFixed (ty, RefI31 (Const (I32 (Int32.of_int tag))) :: l))

  let tag e = Value.int_val (wasm_array_get e (Arith.const 0l))

  let array_length e =
    let* block = Type.block_type in
    let* e = wasm_cast block e in
    Arith.(return (W.ArrayLen e) - const 1l)

  let float_array_length e =
    let* float_array = Type.float_array_type in
    let* e = wasm_cast float_array e in
    return (W.ArrayLen e)

  let gen_array_length e =
    let a = Code.Var.fresh_n "a" in
    block_expr
      { params = []; result = [ I32 ] }
      (let* () = store a e in
       let* () =
         drop
           (block_expr
              { params = []; result = [ Type.value ] }
              (let* block = Type.block_type in
               let* a = load a in
               let* e =
                 Arith.(
                   return
                     (W.ArrayLen
                        (W.Br_on_cast_fail
                           ( 0
                           , { nullable = false; typ = Eq }
                           , { nullable = false; typ = Type block }
                           , a )))
                   - const 1l)
               in
               instr (Br (1, Some e))))
       in
       let* e = float_array_length (load a) in
       instr (W.Push e))

  let array_get e e' = wasm_array_get e Arith.(Value.int_val e' + const 1l)

  let array_set e e' e'' = wasm_array_set e Arith.(Value.int_val e' + const 1l) e''

  let float_array_get e e' =
    box_float () () (wasm_array_get ~ty:Type.float_array_type e (Value.int_val e'))

  let float_array_set e e' e'' =
    wasm_array_set ~ty:Type.float_array_type e (Value.int_val e') (unbox_float e'')

  let gen_array_get e e' =
    let a = Code.Var.fresh_n "a" in
    let i = Code.Var.fresh_n "i" in
    block_expr
      { params = []; result = [ Value.value ] }
      (let* () = store a e in
       let* () = store ~typ:I32 i (Value.int_val e') in
       let* () =
         drop
           (block_expr
              { params = []; result = [ Value.value ] }
              (let* block = Type.block_type in
               let* a = load a in
               let* e =
                 wasm_array_get
                   (return
                      (W.Br_on_cast_fail
                         ( 0
                         , { nullable = false; typ = Eq }
                         , { nullable = false; typ = Type block }
                         , a )))
                   Arith.(load i + const 1l)
               in
               instr (Br (1, Some e))))
       in
       let* e =
         box_float () () (wasm_array_get ~ty:Type.float_array_type (load a) (load i))
       in
       instr (W.Push e))

  let gen_array_set e e' e'' =
    let a = Code.Var.fresh_n "a" in
    let i = Code.Var.fresh_n "i" in
    let v = Code.Var.fresh_n "v" in
    let* () = store a e in
    let* () = store ~typ:I32 i (Value.int_val e') in
    let* () = store v e'' in
    block
      { params = []; result = [] }
      (let* () =
         drop
           (block_expr
              { params = []; result = [ Value.value ] }
              (let* block = Type.block_type in
               let* a = load a in
               let* () =
                 wasm_array_set
                   (return
                      (W.Br_on_cast_fail
                         ( 0
                         , { nullable = false; typ = Eq }
                         , { nullable = false; typ = Type block }
                         , a )))
                   Arith.(load i + const 1l)
                   (load v)
               in
               instr (Br (1, None))))
       in
       wasm_array_set ~ty:Type.float_array_type (load a) (load i) (unbox_float (load v)))

  let bytes_length e =
    let* ty = Type.string_type in
    let* e = wasm_cast ty e in
    return (W.ArrayLen e)

  let bytes_get e e' =
    Value.val_int (wasm_array_get ~ty:Type.string_type e (Value.int_val e'))

  let bytes_set e e' e'' =
    wasm_array_set ~ty:Type.string_type e (Value.int_val e') (Value.int_val e'')

  let field e idx = wasm_array_get e (Arith.const (Int32.of_int (idx + 1)))

  let set_field e idx e' = wasm_array_set e (Arith.const (Int32.of_int (idx + 1))) e'

  let env_start arity =
    if arity = 0
    then 1
    else (if include_closure_arity then 1 else 0) + if arity = 1 then 1 else 2

  let load_function_pointer ~cps ~arity ?(skip_cast = false) closure =
    let arity = if cps then arity - 1 else arity in
    let* ty = Type.closure_type ~usage:`Access ~cps arity in
    let* fun_ty = Type.function_type ~cps arity in
    let casted_closure = if skip_cast then closure else wasm_cast ty closure in
    let* e = wasm_struct_get ty casted_closure (env_start arity - 1) in
    return (`Ref fun_ty, e)

  let load_real_closure ~cps ~arity closure =
    let arity = if cps then arity - 1 else arity in
    let* ty = Type.dummy_closure_type ~cps ~arity in
    let* cl_typ = Type.closure_type ~usage:`Access ~cps arity in
    let* e =
      wasm_cast cl_typ (wasm_struct_get ty (wasm_cast ty closure) (env_start arity))
    in
    return (cl_typ, e)

  let check_function_arity f ~cps ~arity if_match if_mismatch =
    let* fun_ty = Type.closure_type ~usage:`Access ~cps arity in
    let* closure = load f in
    let* () =
      drop
        (block_expr
           { params = []; result = [ Value.value ] }
           (let* e =
              if_match
                ~typ:(Some (W.Ref { nullable = false; typ = Type fun_ty }))
                (return
                   (W.Br_on_cast_fail
                      ( 0
                      , { nullable = false; typ = Eq }
                      , { nullable = false; typ = Type fun_ty }
                      , closure )))
            in
            instr (W.Return (Some e))))
    in
    if_mismatch

  let make_int32 ~kind e =
    let* custom_operations = Type.custom_operations_type in
    let* int32_ops =
      register_import
        ~name:
          (match kind with
          | `Int32 -> "int32_ops"
          | `Nativeint -> "nativeint_ops")
        (Global
           { mut = false; typ = Ref { nullable = false; typ = Type custom_operations } })
    in
    let* ty = Type.int32_type in
    let* e = e in
    return (W.StructNew (ty, [ GlobalGet (V int32_ops); e ]))

  let box_int32 _ _ e = make_int32 ~kind:`Int32 e

  let unbox_int32 e =
    let* ty = Type.int32_type in
    wasm_struct_get ty (wasm_cast ty e) 1

  let make_int64 e =
    let* custom_operations = Type.custom_operations_type in
    let* int64_ops =
      register_import
        ~name:"int64_ops"
        (Global
           { mut = false; typ = Ref { nullable = false; typ = Type custom_operations } })
    in
    let* ty = Type.int64_type in
    let* e = e in
    return (W.StructNew (ty, [ GlobalGet (V int64_ops); e ]))

  let box_int64 _ _ e = make_int64 e

  let unbox_int64 e =
    let* ty = Type.int64_type in
    wasm_struct_get ty (wasm_cast ty e) 1

  let box_nativeint _ _ e = make_int32 ~kind:`Nativeint e

  let unbox_nativeint e =
    let* ty = Type.int32_type in
    wasm_struct_get ty (wasm_cast ty e) 1
end

module Constant = struct
  let string_length_threshold = 100

  let store_in_global c =
    let name = Code.Var.fresh_n "const" in
    let* () = register_global (V name) { mut = false; typ = Type.value } c in
    return (W.GlobalGet (V name))

  let str_js_utf8 s =
    let b = Buffer.create (String.length s) in
    String.iter s ~f:(function
        | '\\' -> Buffer.add_string b "\\\\"
        | c -> Buffer.add_char b c);
    Buffer.contents b

  let str_js_byte s =
    let b = Buffer.create (String.length s) in
    String.iter s ~f:(function
        | '\\' -> Buffer.add_string b "\\\\"
        | '\128' .. '\255' as c ->
            Buffer.add_string b "\\x";
            Buffer.add_char_hex b c
        | c -> Buffer.add_char b c);
    Buffer.contents b

  let rec translate_rec c =
    match c with
    | Code.Int (Regular, i) -> return (true, W.RefI31 (Const (I32 i)))
    | Tuple (tag, a, _) ->
        let* ty = Type.block_type in
        let* l =
          Array.fold_left
            ~f:(fun prev c ->
              let* acc = prev in
              let* c = translate_rec c in
              return (c :: acc))
            ~init:(return [])
            a
        in
        let l = List.rev l in
        let l' =
          List.map ~f:(fun (const, v) -> if const then v else W.RefI31 (Const (I32 0l))) l
        in
        let c = W.ArrayNewFixed (ty, RefI31 (Const (I32 (Int32.of_int tag))) :: l') in
        if List.exists ~f:(fun (const, _) -> not const) l
        then
          let* c = store_in_global c in
          let* () =
            register_init_code
              (snd
                 (List.fold_left
                    ~f:(fun (i, before) (const, v) ->
                      ( i + 1
                      , let* () = before in
                        if const
                        then return ()
                        else
                          Memory.wasm_array_set
                            (return c)
                            (Arith.const (Int32.of_int i))
                            (return v) ))
                    ~init:(1, return ())
                    l))
          in
          return (true, c)
        else return (true, c)
    | NativeString s ->
        let s =
          match s with
          | Utf (Utf8 s) -> str_js_utf8 s
          | Byte s -> str_js_byte s
        in
        let* i = register_string s in
        let* x =
          register_import
            ~import_module:"strings"
            ~name:(string_of_int i)
            (Global { mut = false; typ = Ref { nullable = false; typ = Extern } })
        in
        let* ty = Type.js_type in
        return (true, W.StructNew (ty, [ ExternInternalize (GlobalGet (V x)) ]))
    | String s ->
        let* ty = Type.string_type in
        if String.length s > string_length_threshold
        then
          let name = Code.Var.fresh_n "string" in
          let* () = register_data_segment name ~active:false [ DataBytes s ] in
          return
            ( false
            , W.ArrayNewData
                (ty, name, Const (I32 0l), Const (I32 (Int32.of_int (String.length s))))
            )
        else
          let l =
            String.fold_right
              ~f:(fun c r -> W.Const (I32 (Int32.of_int (Char.code c))) :: r)
              s
              ~init:[]
          in
          return (true, W.ArrayNewFixed (ty, l))
    | Float f ->
        let* ty = Type.float_type in
        return (true, W.StructNew (ty, [ Const (F64 f) ]))
    | Float_array l ->
        let l = Array.to_list l in
        let* ty = Type.float_array_type in
        (*ZZZ Boxed array? *)
        return (true, W.ArrayNewFixed (ty, List.map ~f:(fun f -> W.Const (F64 f)) l))
    | Int64 i ->
        let* e = Memory.make_int64 (return (W.Const (I64 i))) in
        return (true, e)
    | Int (Int32, i) ->
        let* e = Memory.make_int32 ~kind:`Int32 (return (W.Const (I32 i))) in
        return (true, e)
    | Int (Native, i) ->
        let* e = Memory.make_int32 ~kind:`Nativeint (return (W.Const (I32 i))) in
        return (true, e)

  let translate c =
    let* const, c = translate_rec c in
    if const
    then
      let* b = is_small_constant c in
      if b then return c else store_in_global c
    else
      let name = Code.Var.fresh_n "const" in
      let* () =
        register_global
          ~constant:true
          (V name)
          { mut = true; typ = Type.value }
          (W.RefI31 (Const (I32 0l)))
      in
      let* () = register_init_code (instr (W.GlobalSet (V name, c))) in
      return (W.GlobalGet (V name))
end

module Closure = struct
  let get_free_variables ~context info =
    List.filter
      ~f:(fun x -> not (Hashtbl.mem context.constants x))
      info.Wa_closure_conversion.free_variables

  let rec is_last_fun l f =
    match l with
    | [] -> false
    | [ (g, _) ] -> Code.Var.equal f g
    | _ :: r -> is_last_fun r f

  let translate ~context ~closures ~stack_ctx:_ ~cps f =
    let info = Code.Var.Map.find f closures in
    let free_variables = get_free_variables ~context info in
    let arity = List.assoc f info.functions in
    let arity = if cps then arity - 1 else arity in
    let* curry_fun = if arity > 1 then need_curry_fun ~cps ~arity else return f in
    if List.is_empty free_variables
    then
      let* typ = Type.closure_type ~usage:`Alloc ~cps arity in
      let name = Code.Var.fresh_n "closure" in
      let* () =
        register_global
          (V name)
          { mut = false; typ = Type.value }
          (W.StructNew
             ( typ
             , if arity = 0
               then [ W.RefFunc f ]
               else
                 let code_pointers =
                   if arity = 1 then [ W.RefFunc f ] else [ RefFunc curry_fun; RefFunc f ]
                 in
                 if include_closure_arity
                 then Const (I32 (Int32.of_int arity)) :: code_pointers
                 else code_pointers ))
      in
      return (W.GlobalGet (V name))
    else
      let free_variable_count = List.length free_variables in
      match info.Wa_closure_conversion.functions with
      | [] -> assert false
      | [ _ ] ->
          let* typ = Type.env_type ~cps ~arity free_variable_count in
          let* l = expression_list load free_variables in
          return
            (W.StructNew
               ( typ
               , (if arity = 0
                  then [ W.RefFunc f ]
                  else
                    let code_pointers =
                      if arity = 1
                      then [ W.RefFunc f ]
                      else [ RefFunc curry_fun; RefFunc f ]
                    in
                    if include_closure_arity
                    then W.Const (I32 (Int32.of_int arity)) :: code_pointers
                    else code_pointers)
                 @ l ))
      | (g, _) :: _ as functions ->
          let function_count = List.length functions in
          let* env_typ = Type.rec_env_type ~function_count ~free_variable_count in
          let env =
            if Code.Var.equal f g
            then
              let env = Code.Var.fresh () in
              let* () = set_closure_env f env in
              let* l = expression_list load free_variables in
              tee
                ~typ:(W.Ref { nullable = false; typ = Type env_typ })
                env
                (return
                   (W.StructNew
                      ( env_typ
                      , List.init ~len:function_count ~f:(fun _ ->
                            W.RefI31 (W.Const (I32 0l)))
                        @ l )))
            else
              let* env = get_closure_env g in
              let* () = set_closure_env f env in
              load env
          in
          let* typ =
            Type.rec_closure_type ~cps ~arity ~function_count ~free_variable_count
          in
          let res =
            let* env = env in
            return
              (W.StructNew
                 ( typ
                 , (let code_pointers =
                      if arity = 1
                      then [ W.RefFunc f ]
                      else [ RefFunc curry_fun; RefFunc f ]
                    in
                    if include_closure_arity
                    then W.Const (I32 (Int32.of_int arity)) :: code_pointers
                    else code_pointers)
                   @ [ env ] ))
          in
          if is_last_fun functions f
          then
            seq
              (snd
                 (List.fold_left
                    ~f:(fun (i, prev) (g, _) ->
                      ( i + 1
                      , let* () = prev in
                        Memory.wasm_struct_set
                          env_typ
                          env
                          i
                          (if Code.Var.equal f g then tee f res else load g) ))
                    ~init:(0, return ())
                    functions))
              (load f)
          else res

  let bind_environment ~context ~closures ~cps f =
    if Hashtbl.mem context.constants f
    then
      (* The closures are all constants and the environment is empty. *)
      let* _ = add_var (Code.Var.fresh ()) in
      return ()
    else
      let info = Code.Var.Map.find f closures in
      let free_variables = get_free_variables ~context info in
      let free_variable_count = List.length free_variables in
      let arity = List.assoc f info.functions in
      let arity = if cps then arity - 1 else arity in
      let offset = Memory.env_start arity in
      match info.Wa_closure_conversion.functions with
      | [ _ ] ->
          let* typ = Type.env_type ~cps ~arity free_variable_count in
          let* _ = add_var f in
          let env = Code.Var.fresh_n "env" in
          let* () =
            store
              ~typ:(W.Ref { nullable = false; typ = Type typ })
              env
              Memory.(wasm_cast typ (load f))
          in
          snd
            (List.fold_left
               ~f:(fun (i, prev) x ->
                 ( i + 1
                 , let* () = prev in
                   define_var x Memory.(wasm_struct_get typ (load env) i) ))
               ~init:(offset, return ())
               free_variables)
      | functions ->
          let function_count = List.length functions in
          let* typ =
            Type.rec_closure_type ~cps ~arity ~function_count ~free_variable_count
          in
          let* _ = add_var f in
          let env = Code.Var.fresh_n "env" in
          let* env_typ = Type.rec_env_type ~function_count ~free_variable_count in
          let* () =
            store
              ~typ:(W.Ref { nullable = false; typ = Type env_typ })
              env
              Memory.(wasm_struct_get typ (wasm_cast typ (load f)) offset)
          in
          snd
            (List.fold_left
               ~f:(fun (i, prev) x ->
                 ( i + 1
                 , let* () = prev in
                   define_var x Memory.(wasm_struct_get env_typ (load env) i) ))
               ~init:(0, return ())
               (List.map ~f:fst functions @ free_variables))

  let curry_allocate ~stack_ctx:_ ~x:_ ~cps ~arity m ~f ~closure ~arg =
    let* ty = Type.curry_type ~cps arity m in
    let* cl_ty =
      if m = arity
      then Type.closure_type ~usage:`Alloc ~cps arity
      else Type.curry_type ~cps arity (m + 1)
    in
    let* closure = Memory.wasm_cast cl_ty (load closure) in
    let* arg = load arg in
    let closure_contents = [ W.RefFunc f; closure; arg ] in
    return
      (W.StructNew
         ( ty
         , if include_closure_arity
           then Const (I32 1l) :: closure_contents
           else closure_contents ))

  let curry_load ~cps ~arity m closure =
    let m = m + 1 in
    let* ty = Type.curry_type ~cps arity m in
    let* cl_ty =
      if m = arity
      then Type.closure_type ~usage:`Alloc ~cps arity
      else Type.curry_type ~cps arity (m + 1)
    in
    let cast e = if m = 2 then Memory.wasm_cast ty e else e in
    let offset = Memory.env_start 1 in
    return
      ( Memory.wasm_struct_get ty (cast (load closure)) (offset + 1)
      , Memory.wasm_struct_get ty (cast (load closure)) offset
      , Some (W.Ref { nullable = false; typ = Type cl_ty }) )

  let dummy ~cps ~arity =
    (* The runtime only handle function with arity up to 4
       (1 for CPS functions) *)
    let arity = if cps then 1 else if arity > 4 then 1 else arity in
    let* dummy_fun = need_dummy_fun ~cps ~arity in
    let* ty = Type.dummy_closure_type ~cps ~arity in
    let* curry_fun = if arity > 1 then need_curry_fun ~cps ~arity else return dummy_fun in
    let* cl_typ = Type.closure_type ~usage:`Alloc ~cps arity in
    let closure_contents =
      if arity = 1
      then [ W.RefFunc dummy_fun; RefNull (Type cl_typ) ]
      else [ RefFunc curry_fun; RefFunc dummy_fun; RefNull (Type cl_typ) ]
    in
    return
      (W.StructNew
         ( ty
         , if include_closure_arity
           then Const (I32 1l) :: closure_contents
           else closure_contents ))
end

module Stack = struct
  type stack = Code.Var.t option list

  type info = unit

  let generate_spilling_information _ ~context:_ ~closures:_ ~pc:_ ~env:_ ~params:_ = ()

  let add_spilling _ ~location:_ ~stack:_ ~live_vars:_ ~spilled_vars:_ = (), []

  type ctx = unit

  let start_function ~context:_ _ = ()

  let start_block ~context:_ _ _ = ()

  let perform_reloads _ _ = return ()

  let perform_spilling _ _ = return ()

  let kill_variables _ = ()

  let assign _ _ = return ()

  let make_info () = ()

  let adjust_stack _ ~src:_ ~dst:_ = return ()

  let stack_adjustment_needed _ ~src:_ ~dst:_ = false
end

module Math = struct
  let float_func_type n =
    { W.params = List.init ~len:n ~f:(fun _ : W.value_type -> F64); result = [ F64 ] }

  let unary name x =
    let* f = register_import ~import_module:"Math" ~name (Fun (float_func_type 1)) in
    let* x = x in
    return (W.Call (f, [ x ]))

  let cos f = unary "cos" f

  let sin f = unary "sin" f

  let tan f = unary "tan" f

  let acos f = unary "acos" f

  let asin f = unary "asin" f

  let atan f = unary "atan" f

  let cosh f = unary "cosh" f

  let sinh f = unary "sinh" f

  let tanh f = unary "tanh" f

  let acosh f = unary "acosh" f

  let asinh f = unary "asinh" f

  let atanh f = unary "atanh" f

  let cbrt f = unary "cbrt" f

  let exp f = unary "exp" f

  let expm1 f = unary "expm1" f

  let log f = unary "log" f

  let log1p f = unary "log1p" f

  let log2 f = unary "log2" f

  let log10 f = unary "log10" f

  let binary name x y =
    let* f = register_import ~import_module:"Math" ~name (Fun (float_func_type 2)) in
    let* x = x in
    let* y = y in
    return (W.Call (f, [ x; y ]))

  let atan2 f g = binary "atan2" f g

  let hypot f g = binary "hypot" f g

  let power f g = binary "pow" f g

  let fmod f g = binary "fmod" f g

  let round x =
    let* f = register_import ~name:"caml_round" (Fun (float_func_type 1)) in
    let* x = x in
    return (W.Call (f, [ x ]))

  let exp2 x = power (return (W.Const (F64 2.))) x
end

module JavaScript = struct
  let anyref = W.Ref { nullable = true; typ = Any }

  let invoke_fragment name args =
    let* f =
      register_import
        ~import_module:"fragments"
        ~name
        (Fun { params = List.map ~f:(fun _ -> anyref) args; result = [ anyref ] })
    in
    let* wrap =
      register_import ~name:"wrap" (Fun { params = [ anyref ]; result = [ Type.value ] })
    in
    let* unwrap =
      register_import
        ~name:"unwrap"
        (Fun { params = [ Type.value ]; result = [ anyref ] })
    in
    let* args =
      expression_list
        (fun e ->
          let* e = e in
          return (W.Call (unwrap, [ e ])))
        args
    in
    return (W.Call (wrap, [ Call (f, args) ]))
end

let internal_primitives = Hashtbl.create 100

let () =
  let register name f = Hashtbl.add internal_primitives name f in
  let module J = Javascript in
  register "%caml_js_opt_call" (fun transl_prim_arg l ->
      let arity = List.length l - 2 in
      let name = Printf.sprintf "call_%d" arity in
      let* () =
        register_fragment name (fun () ->
            let f = Utf8_string.of_string_exn "f" in
            let o = Utf8_string.of_string_exn "o" in
            let params =
              List.init ~len:arity ~f:(fun i ->
                  Utf8_string.of_string_exn (Printf.sprintf "x%d" i))
            in
            EArrow
              ( J.fun_
                  (List.map ~f:J.ident (f :: params))
                  [ ( Return_statement
                        (Some
                           (J.call
                              (J.dot
                                 (EVar (J.ident f))
                                 (Utf8_string.of_string_exn "call"))
                              (List.map ~f:(fun x -> J.EVar (J.ident x)) (o :: params))
                              N))
                    , N )
                  ]
                  N
              , AUnknown ))
      in
      let l = List.map ~f:transl_prim_arg l in
      JavaScript.invoke_fragment name l);
  register "%caml_js_opt_fun_call" (fun transl_prim_arg l ->
      let arity = List.length l - 1 in
      let name = Printf.sprintf "fun_call_%d" arity in
      let* () =
        register_fragment name (fun () ->
            let f = Utf8_string.of_string_exn "f" in
            let params =
              List.init ~len:arity ~f:(fun i ->
                  Utf8_string.of_string_exn (Printf.sprintf "x%d" i))
            in
            EArrow
              ( J.fun_
                  (List.map ~f:J.ident (f :: params))
                  [ ( Return_statement
                        (Some
                           (J.call
                              (EVar (J.ident f))
                              (List.map ~f:(fun x -> J.EVar (J.ident x)) params)
                              N))
                    , N )
                  ]
                  N
              , AUnknown ))
      in
      let l = List.map ~f:transl_prim_arg l in
      JavaScript.invoke_fragment name l);
  register "%caml_js_opt_meth_call" (fun transl_prim_arg l ->
      match l with
      | o :: Code.Pc (NativeString (Utf meth)) :: args ->
          let arity = List.length args in
          let name =
            let (Utf8 name) = meth in
            Printf.sprintf "meth_call_%d_%s" arity name
          in
          let* () =
            register_fragment name (fun () ->
                let o = Utf8_string.of_string_exn "o" in
                let params =
                  List.init ~len:arity ~f:(fun i ->
                      Utf8_string.of_string_exn (Printf.sprintf "x%d" i))
                in
                EArrow
                  ( J.fun_
                      (List.map ~f:J.ident (o :: params))
                      [ ( Return_statement
                            (Some
                               (J.call
                                  (J.dot (EVar (J.ident o)) meth)
                                  (List.map ~f:(fun x -> J.EVar (J.ident x)) params)
                                  N))
                        , N )
                      ]
                      N
                  , AUnknown ))
          in
          let o = transl_prim_arg o in
          let args = List.map ~f:transl_prim_arg args in
          JavaScript.invoke_fragment name (o :: args)
      | _ -> assert false);
  register "%caml_js_opt_new" (fun transl_prim_arg l ->
      let arity = List.length l - 1 in
      let name = Printf.sprintf "new_%d" arity in
      let* () =
        register_fragment name (fun () ->
            let c = Utf8_string.of_string_exn "c" in
            let params =
              List.init ~len:arity ~f:(fun i ->
                  Utf8_string.of_string_exn (Printf.sprintf "x%d" i))
            in
            EArrow
              ( J.fun_
                  (List.map ~f:J.ident (c :: params))
                  [ ( Return_statement
                        (Some
                           (ENew
                              ( EVar (J.ident c)
                              , Some
                                  (List.map ~f:(fun x -> J.Arg (EVar (J.ident x))) params)
                              )))
                    , N )
                  ]
                  N
              , AUnknown ))
      in
      let l = List.map ~f:transl_prim_arg l in
      JavaScript.invoke_fragment name l);
  register "caml_js_get" (fun transl_prim_arg l ->
      match l with
      | [ x; Code.Pc (NativeString (Utf prop)) ] when J.is_ident' prop ->
          let name =
            let (Utf8 name) = prop in
            Printf.sprintf "get_%s" name
          in
          let* () =
            register_fragment name (fun () ->
                let o = Utf8_string.of_string_exn "o" in
                EArrow
                  ( J.fun_
                      [ J.ident o ]
                      [ Return_statement (Some (J.dot (EVar (J.ident o)) prop)), N ]
                      N
                  , AUnknown ))
          in
          JavaScript.invoke_fragment name [ transl_prim_arg x ]
      | [ _; _ ] ->
          let* f = register_import ~name:"caml_js_get" (Fun (Type.func_type 1)) in
          let l = List.map ~f:transl_prim_arg l in
          let* l = expression_list (fun e -> e) l in
          return (W.Call (f, l))
      | _ -> assert false);
  register "caml_js_set" (fun transl_prim_arg l ->
      match l with
      | [ x; Code.Pc (NativeString (Utf prop)); y ] when J.is_ident' prop ->
          let name =
            let (Utf8 name) = prop in
            Printf.sprintf "set_%s" name
          in
          let* () =
            register_fragment name (fun () ->
                let o = Utf8_string.of_string_exn "o" in
                let v = Utf8_string.of_string_exn "v" in
                EArrow
                  ( J.fun_
                      [ J.ident o; J.ident v ]
                      [ ( Return_statement
                            (Some
                               (J.EBin
                                  (J.Eq, J.dot (EVar (J.ident o)) prop, EVar (J.ident v))))
                        , N )
                      ]
                      N
                  , AUnknown ))
          in
          let l = List.map ~f:transl_prim_arg [ x; y ] in
          JavaScript.invoke_fragment name l
      | [ _; _; _ ] ->
          let* f = register_import ~name:"caml_js_set" (Fun (Type.func_type 2)) in
          let l = List.map ~f:transl_prim_arg l in
          let* l = expression_list (fun e -> e) l in
          return (W.Call (f, l))
      | _ -> assert false);
  let counter = ref (-1) in
  register "%caml_js_opt_object" (fun transl_prim_arg l ->
      let rec split kl vl l =
        match l with
        | [] -> List.rev kl, List.rev vl
        | Code.Pc (NativeString (Utf k)) :: v :: r -> split (k :: kl) (v :: vl) r
        | _ -> assert false
      in
      let kl, vl = split [] [] l in
      let name =
        incr counter;
        Printf.sprintf "obj_%d" !counter
      in
      let* () =
        register_fragment name (fun () ->
            let arity = List.length kl in
            let params =
              List.init ~len:arity ~f:(fun i ->
                  Utf8_string.of_string_exn (Printf.sprintf "x%d" i))
            in
            EArrow
              ( J.fun_
                  (List.map ~f:J.ident params)
                  [ ( Return_statement
                        (Some
                           (EObj
                              (List.map2
                                 ~f:(fun k x ->
                                   J.Property
                                     ( (if J.is_ident' k then J.PNI k else J.PNS k)
                                     , EVar (J.ident x) ))
                                 kl
                                 params)))
                    , N )
                  ]
                  N
              , AUnknown ))
      in
      let l = List.map ~f:transl_prim_arg vl in
      JavaScript.invoke_fragment name l)

let externref = W.Ref { nullable = true; typ = Extern }

let handle_exceptions ~result_typ ~fall_through ~context body x exn_handler =
  let* js_tag = register_import ~name:"javascript_exception" (Tag externref) in
  let* ocaml_tag = register_import ~name:"ocaml_exception" (Tag Value.value) in
  let* f =
    register_import
      ~name:"caml_wrap_exception"
      (Fun { params = [ externref ]; result = [ Value.value ] })
  in
  block
    { params = []; result = result_typ }
    (let* () =
       try_
         { params = []; result = [] }
         (body ~result_typ:[] ~fall_through:(`Block (-1)) ~context:(`Skip :: context))
         [ ocaml_tag, store ~always:true x (return (W.Pop Value.value))
         ; ( js_tag
           , let exn = Code.Var.fresh () in
             let* () = store ~always:true ~typ:externref exn (return (W.Pop externref)) in
             let* exn = load exn in
             store ~always:true x (return (W.Call (f, [ exn ]))) )
         ]
     in
     exn_handler ~result_typ ~fall_through ~context)

let post_process_function_body = Wa_initialize_locals.f

let entry_point ~context ~toplevel_fun =
  let code =
    let* f =
      register_import
        ~name:
          (if Config.Flag.effects ()
           then "caml_cps_initialize_effects"
           else "caml_initialize_effects")
        (Fun { W.params = [ W.Ref { nullable = true; typ = Extern } ]; result = [] })
    in
    let suspender = Code.Var.fresh () in
    let* _ = add_var suspender in
    let* s = load suspender in
    let* () = instr (W.CallInstr (f, [ s ])) in
    let* () = init_code context in
    let* main =
      register_import
        ~name:"caml_main"
        (Fun { params = [ W.Ref { nullable = false; typ = Func } ]; result = [] })
    in
    instr (W.CallInstr (main, [ RefFunc toplevel_fun ]))
  in
  { W.params = [ W.Ref { nullable = true; typ = Extern } ]; result = [] }, code
