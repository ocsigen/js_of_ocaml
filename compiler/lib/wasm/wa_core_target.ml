open! Stdlib
module W = Wa_ast
open Wa_code_generation

type expression = Wa_ast.expression Wa_code_generation.t

module Memory = struct
  let mem_load ?(offset = 0) e =
    assert (offset >= 0);
    let* e = e in
    match e with
    | W.ConstSym (V x, offset') ->
        let rec get_data offset l =
          match l with
          | [] -> assert false
          | W.DataI32 i :: _ when offset = 0 -> W.Const (I32 i)
          | W.DataSym (sym, ofs) :: _ when offset = 0 -> W.ConstSym (sym, ofs)
          | (W.DataI32 _ | DataSym _) :: r -> get_data (offset - 4) r
          | (DataI8 _ | DataBytes _ | DataSpace _ | DataI64 _) :: _ -> assert false
        in
        let* _, l = get_data_segment x in
        let data = get_data (offset + offset') l in
        return data
    | _ -> return (W.Load (I32 (Int32.of_int offset), e))

  let mem_init ?(offset = 0) e e' =
    assert (offset >= 0);
    let* e = e in
    let* e' = e' in
    instr (Store (I32 (Int32.of_int offset), e, e'))

  let mem_store ?(offset = 0) e e' =
    assert (offset >= 0);
    let* e = Arith.(e + const (Int32.of_int offset)) in
    let* e' = e' in
    instr (CallInstr (S "caml_modify", [ e; e' ]))

  (*ZZZ
    p = young_ptr - size;
    if (p < young_limit) {caml_call_gc(); p = young_ptr - size}
    ...
    return p + 4
  *)
  let header ?(const = false) ~tag ~len () =
    Int32.(add (shift_left (of_int len) 10) (of_int (tag + if const then 3 * 256 else 0)))

  let allocate ~tag l =
    let len = List.length l in
    let p = Code.Var.fresh_n "p" in
    let size = (len + 1) * 4 in
    seq
      (let* v =
         tee p Arith.(return (W.GlobalGet (S "young_ptr")) - const (Int32.of_int size))
       in
       let* () = instr (W.GlobalSet (S "young_ptr", v)) in
       let* () = mem_init (load p) (Arith.const (header ~tag ~len ())) in
       snd
         (List.fold_right
            ~init:(len, return ())
            ~f:(fun v (i, cont) ->
              ( i - 1
              , let* () =
                  mem_init
                    ~offset:(4 * i)
                    (load p)
                    (match v with
                    | `Var y -> load y
                    | `Expr e -> return e)
                in
                cont ))
            l))
      Arith.(load p + const 4l)
  (*ZZZ Float array?*)

  let tag e = Arith.(mem_load (e - const 4l) land const 0xffl)

  (*
  let length e = Arith.(mem_load (e - const 4l) lsr const 10l)
*)
  let block_length e = Arith.((mem_load (e - const 4l) lsr const 9l) lor const 1l)

  let array_get e e' = mem_load Arith.(e + ((e' - const 1l) lsl const 1l))

  let array_set e e' e'' = mem_store Arith.(e + ((e' - const 1l) lsl const 1l)) e''

  let bytes_get e e' =
    let* addr = Arith.(e + e' - const 1l) in
    return (W.Load8 (U, I32 (Int32.of_int 0), addr))

  let bytes_set e e' e'' =
    let* addr = Arith.(e + e' - const 1l) in
    let* e'' = e'' in
    instr (W.Store8 (U, I32 (Int32.of_int 0), addr, e''))

  let field e idx = mem_load ~offset:(4 * idx) e

  let set_field e idx e' = mem_store ~offset:(4 * idx) e e'

  let load_function_pointer ~arity closure = field closure (if arity = 1 then 0 else 2)
end

module Value = struct
  let value : W.value_type = I32

  let unit = Arith.const 1l

  let val_int i = Arith.((i lsl const 1l) + const 1l)

  let int_val i = Arith.(i asr const 1l)

  let check_is_not_zero i = Arith.(i <> const 1l)

  let check_is_int i = Arith.(i land const 1l)

  let not b = Arith.(const 4l - b)

  let lt i i' = val_int Arith.(i < i')

  let le i i' = val_int Arith.(i <= i')

  let eq i i' = val_int Arith.(i = i')

  let neq i i' = val_int Arith.(i <> i')

  let ult i i' = val_int Arith.(ult i i')

  let is_int i = val_int Arith.(i land const 1l)

  let int_add i i' = Arith.(i + i' - const 1l)

  let int_sub i i' = Arith.(i - i' + const 1l)

  let int_mul i i' = val_int Arith.(int_val i * int_val i')

  let int_neg i = Arith.(const 2l - i)

  let int_or i i' = Arith.(i lor i')

  let int_and i i' = Arith.(i land i')

  let int_xor i i' = Arith.(i lxor i' lor const 1l)

  let int_lsl i i' = Arith.(((i - const 1l) lsl int_val i') + const 1l)

  let int_lsr i i' = Arith.((i lsr int_val i') lor const 1l)

  let int_asr i i' = Arith.((i asr int_val i') lor const 1l)
end

module Constant = struct
  let rec translate_rec context c =
    match c with
    | Code.Int i -> W.DataI32 Int32.(add (add i i) 1l)
    | Tuple (tag, a, _) ->
        let h = Memory.header ~const:true ~tag ~len:(Array.length a) () in
        let name = Code.Var.fresh_n "block" in
        let block =
          W.DataI32 h :: List.map ~f:(fun c -> translate_rec context c) (Array.to_list a)
        in
        context.data_segments <- Code.Var.Map.add name (true, block) context.data_segments;
        W.DataSym (V name, 4)
    | NativeString (Byte s | Utf (Utf8 s)) | String s ->
        let l = String.length s in
        let len = (l + 4) / 4 in
        let h = Memory.header ~const:true ~tag:Obj.string_tag ~len () in
        let name = Code.Var.fresh_n "str" in
        let extra = (4 * len) - l - 1 in
        let string =
          W.DataI32 h
          :: DataBytes s
          :: (if extra = 0 then [ DataI8 0 ] else [ DataSpace extra; DataI8 extra ])
        in
        context.data_segments <-
          Code.Var.Map.add name (true, string) context.data_segments;
        W.DataSym (V name, 4)
    | Float f ->
        let h = Memory.header ~const:true ~tag:Obj.double_tag ~len:2 () in
        let name = Code.Var.fresh_n "float" in
        let block = [ W.DataI32 h; DataI64 (Int64.bits_of_float f) ] in
        context.data_segments <- Code.Var.Map.add name (true, block) context.data_segments;
        W.DataSym (V name, 4)
    | Float_array l ->
        (*ZZZ Boxed array? *)
        let l = Array.to_list l in
        let h =
          Memory.header ~const:true ~tag:Obj.double_array_tag ~len:(List.length l) ()
        in
        let name = Code.Var.fresh_n "float_array" in
        let block =
          W.DataI32 h :: List.map ~f:(fun f -> translate_rec context (Float f)) l
        in
        context.data_segments <- Code.Var.Map.add name (true, block) context.data_segments;
        W.DataSym (V name, 4)
    | Int64 i ->
        let h = Memory.header ~const:true ~tag:Obj.custom_tag ~len:3 () in
        let name = Code.Var.fresh_n "int64" in
        let block = [ W.DataI32 h; DataSym (S "caml_int64_ops", 0); DataI64 i ] in
        context.data_segments <- Code.Var.Map.add name (true, block) context.data_segments;
        W.DataSym (V name, 4)

  let translate c =
    let* context = get_context in
    return
      (match translate_rec context c with
      | W.DataSym (V name, offset) -> W.ConstSym (V name, offset)
      | W.DataI32 i -> W.Const (I32 i)
      | _ -> assert false)
end

module Closure = struct
  let get_free_variables ~context info =
    List.filter
      ~f:(fun x -> not (Hashtbl.mem context.constants x))
      info.Wa_closure_conversion.free_variables

  let closure_stats =
    let s = ref 0 in
    let n = ref 0 in
    fun context info ->
      let free_variables = get_free_variables ~context info in
      if false && not (List.is_empty free_variables)
      then
        (incr n;
         s := !s + List.length free_variables;
         Format.eprintf
           "OOO %d %f %s@."
           (List.length free_variables)
           (float !s /. float !n))
          (Code.Var.to_string (fst (List.hd info.functions)))

  let closure_env_start info =
    List.fold_left
      ~f:(fun i (_, arity) -> i + if arity > 1 then 4 else 3)
      ~init:(-1)
      info.Wa_closure_conversion.functions

  let function_offset_in_closure info f =
    let rec index i l =
      match l with
      | [] -> assert false
      | (g, arity) :: r ->
          if Code.Var.equal f g then i else index (i + if arity > 1 then 4 else 3) r
    in
    index 0 info.Wa_closure_conversion.functions

  let closure_info ~arity ~sz =
    W.Const (I32 Int32.(add (shift_left (of_int arity) 24) (of_int ((sz lsl 1) + 1))))

  let translate ~context ~closures x =
    let info = Code.Var.Map.find x closures in
    let f, _ = List.hd info.Wa_closure_conversion.functions in
    if Code.Var.equal x f
    then (
      let start_env = closure_env_start info in
      let* _, start =
        List.fold_left
          ~f:(fun accu (f, arity) ->
            let* i, start = accu in
            let* curry_fun = return f in
            let start =
              if i = 0
              then start
              else W.Const (I32 (Memory.header ~tag:Obj.infix_tag ~len:i ())) :: start
            in
            let clos_info = closure_info ~arity ~sz:(start_env - i) in
            let start = clos_info :: W.ConstSym (V curry_fun, 0) :: start in
            return
              (if arity > 1 then i + 4, W.ConstSym (V f, 0) :: start else i + 3, start))
          ~init:(return (0, []))
          info.functions
      in
      closure_stats context info;
      let free_variables = get_free_variables ~context info in
      if List.is_empty free_variables
      then
        let l =
          List.rev_map
            ~f:(fun e ->
              match e with
              | W.Const (I32 i) -> W.DataI32 i
              | ConstSym (sym, offset) -> DataSym (sym, offset)
              | _ -> assert false)
            start
        in
        let h = Memory.header ~const:true ~tag:Obj.closure_tag ~len:(List.length l) () in
        let name = Code.Var.fresh_n "closure" in
        let* () = register_data_segment name ~active:true (W.DataI32 h :: l) in
        return (W.ConstSym (V name, 4))
      else
        Memory.allocate
          ~tag:Obj.closure_tag
          (List.rev_map ~f:(fun e -> `Expr e) start
          @ List.map ~f:(fun x -> `Var x) free_variables))
    else
      let offset = Int32.of_int (4 * function_offset_in_closure info x) in
      Arith.(load f + const offset)

  let bind_environment ~context ~closures f =
    if Hashtbl.mem context.constants f
    then
      (* The closures are all constants and the environment is empty. *)
      let* _ = add_var (Code.Var.fresh ()) in
      return ()
    else
      let info = Code.Var.Map.find f closures in
      let funct_index = function_offset_in_closure info f in
      let* _ = add_var f in
      let* () =
        snd
          (List.fold_left
             ~f:(fun (i, prev) (x, arity) ->
               ( (i + if arity > 1 then 4 else 3)
               , let* () = prev in
                 if i = 0
                 then return ()
                 else
                   define_var
                     x
                     (let offset = 4 * i in
                      Arith.(load f + const (Int32.of_int offset))) ))
             ~init:(-funct_index, return ())
             info.functions)
      in
      let start_env = closure_env_start info in
      let offset = start_env - funct_index in
      let free_variables = get_free_variables ~context info in
      snd
        (List.fold_left
           ~f:(fun (i, prev) x ->
             ( i + 1
             , let* () = prev in
               define_var
                 x
                 (let* f = load f in
                  return (W.Load (I32 (Int32.of_int (4 * i)), f))) ))
           ~init:(offset, return ())
           free_variables)
end

let entry_point ~register_primitive =
  let declare_global name =
    register_global name { mut = true; typ = I32 } (Const (I32 0l))
  in
  let* () = declare_global "young_ptr" in
  let* () = declare_global "young_limit" in
  register_primitive "caml_modify" { W.params = [ I32; I32 ]; result = [] };
  register_primitive "__wasm_call_ctors" { W.params = []; result = [] };
  let* () = instr (W.CallInstr (S "__wasm_call_ctors", [])) in
  let* sz = Arith.const 3l in
  let* high = Arith.((return (W.MemoryGrow (0, sz)) + const 3l) lsl const 16l) in
  let* () = instr (W.GlobalSet (S "young_ptr", high)) in
  let low = W.ConstSym (S "__heap_base", 0) in
  instr (W.GlobalSet (S "young_limit", low))
