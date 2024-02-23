open Stdlib

type ctx =
  { mutable initialized : IntSet.t
  ; uninitialized : IntSet.t ref
  }

let mark_initialized ctx i = ctx.initialized <- IntSet.add i ctx.initialized

let fork_context { initialized; uninitialized } = { initialized; uninitialized }

let check_initialized ctx i =
  if not (IntSet.mem i ctx.initialized)
  then ctx.uninitialized := IntSet.add i !(ctx.uninitialized)

let rec scan_expression ctx e =
  match e with
  | Wa_ast.Const _ | ConstSym _ | GlobalGet _ | Pop _ | RefFunc _ | RefNull _ -> ()
  | UnOp (_, e')
  | I32WrapI64 e'
  | I64ExtendI32 (_, e')
  | F32DemoteF64 e'
  | F64PromoteF32 e'
  | Load (_, e')
  | Load8 (_, _, e')
  | MemoryGrow (_, e')
  | RefI31 e'
  | I31Get (_, e')
  | ArrayLen e'
  | StructGet (_, _, _, e')
  | RefCast (_, e')
  | RefTest (_, e')
  | Br_on_cast (_, _, _, e')
  | Br_on_cast_fail (_, _, _, e')
  | ExternInternalize e'
  | ExternExternalize e' -> scan_expression ctx e'
  | BinOp (_, e', e'')
  | ArrayNew (_, e', e'')
  | ArrayNewData (_, _, e', e'')
  | ArrayGet (_, _, e', e'')
  | RefEq (e', e'') ->
      scan_expression ctx e';
      scan_expression ctx e''
  | LocalGet i -> check_initialized ctx i
  | LocalTee (i, e') ->
      scan_expression ctx e';
      mark_initialized ctx i
  | Call_indirect (_, e', l) | Call_ref (_, e', l) ->
      scan_expressions ctx l;
      scan_expression ctx e'
  | Call (_, l) | ArrayNewFixed (_, l) | StructNew (_, l) -> scan_expressions ctx l
  | BlockExpr (_, l) -> scan_instructions ctx l
  | Seq (l, e') -> scan_instructions ctx (l @ [ Push e' ])

and scan_expressions ctx l = List.iter ~f:(fun e -> scan_expression ctx e) l

and scan_instruction ctx i =
  match i with
  | Wa_ast.Drop e
  | GlobalSet (_, e)
  | Br (_, Some e)
  | Br_if (_, e)
  | Br_table (e, _, _)
  | Throw (_, e)
  | Return (Some e)
  | Push e -> scan_expression ctx e
  | Store (_, e, e') | Store8 (_, e, e') | StructSet (_, _, e, e') ->
      scan_expression ctx e;
      scan_expression ctx e'
  | LocalSet (i, e) ->
      scan_expression ctx e;
      mark_initialized ctx i
  | Loop (_, l) | Block (_, l) -> scan_instructions ctx l
  | If (_, e, l, l') ->
      scan_expression ctx e;
      scan_instructions ctx l;
      scan_instructions ctx l'
  | Try (_, body, catches, catch_all) ->
      scan_instructions ctx body;
      List.iter ~f:(fun (_, l) -> scan_instructions ctx l) catches;
      Option.iter ~f:(fun l -> scan_instructions ctx l) catch_all
  | CallInstr (_, l) | Return_call (_, l) -> scan_expressions ctx l
  | Br (_, None) | Return None | Rethrow _ | Nop -> ()
  | ArraySet (_, e, e', e'') ->
      scan_expression ctx e;
      scan_expression ctx e';
      scan_expression ctx e''
  | Return_call_indirect (_, e', l) | Return_call_ref (_, e', l) ->
      scan_expressions ctx l;
      scan_expression ctx e'

and scan_instructions ctx l =
  let ctx = fork_context ctx in
  List.iter ~f:(fun i -> scan_instruction ctx i) l

let f ~param_count ~locals instrs =
  let ctx = { initialized = IntSet.empty; uninitialized = ref IntSet.empty } in
  for i = 0 to param_count - 1 do
    mark_initialized ctx i
  done;
  List.iteri
    ~f:(fun i typ ->
      match (typ : Wa_ast.value_type) with
      | I32 | I64 | F32 | F64 | Ref { nullable = true; _ } ->
          mark_initialized ctx (i + param_count)
      | Ref { nullable = false; _ } -> ())
    locals;
  scan_instructions ctx instrs;
  List.map
    ~f:(fun i -> Wa_ast.LocalSet (i, RefI31 (Const (I32 0l))))
    (IntSet.elements !(ctx.uninitialized))
  @ instrs
