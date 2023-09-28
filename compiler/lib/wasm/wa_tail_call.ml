open! Stdlib

let rec instruction ~tail i =
  match i with
  | Wa_ast.Loop (ty, l) -> Wa_ast.Loop (ty, instructions ~tail l)
  | Block (ty, l) -> Block (ty, instructions ~tail l)
  | If (ty, e, l1, l2) -> If (ty, e, instructions ~tail l1, instructions ~tail l2)
  | Try (ty, l, catches, catch_all) ->
      Try
        ( ty
        , l
        , List.map ~f:(fun (tag, l) -> tag, instructions ~tail l) catches
        , Option.map ~f:(fun l -> instructions ~tail l) catch_all )
  | Return (Some (Call (symb, l))) -> Return_call (symb, l)
  | Return (Some (Call_indirect (ty, e, l))) -> Return_call_indirect (ty, e, l)
  | Return (Some (Call_ref (ty, e, l))) -> Return_call_ref (ty, e, l)
  | Push (Call (symb, l)) when tail -> Return_call (symb, l)
  | Push (Call_indirect (ty, e, l)) when tail -> Return_call_indirect (ty, e, l)
  | Push (Call_ref (ty, e, l)) when tail -> Return_call_ref (ty, e, l)
  | Push (Call_ref _) -> i
  | Drop (BlockExpr (typ, l)) -> Drop (BlockExpr (typ, instructions ~tail:false l))
  | Drop _
  | Store _
  | Store8 _
  | LocalSet _
  | GlobalSet _
  | Br_table _
  | Br _
  | Br_if _
  | Return _
  | Throw _
  | Rethrow _
  | CallInstr _
  | Nop
  | Push _
  | ArraySet _
  | StructSet _
  | Return_call_indirect _
  | Return_call _
  | Return_call_ref _ -> i

and instructions ~tail l =
  match l with
  | [] -> []
  | [ i ] -> [ instruction ~tail i ]
  | [ LocalSet (x, Call (symb, l)); Return (Some (LocalGet y)) ] when x = y ->
      [ Return_call (symb, l) ]
  | [ LocalSet (x, Call_indirect (ty, e, l)); Return (Some (LocalGet y)) ] when x = y ->
      [ Return_call_indirect (ty, e, l) ]
  | [ LocalSet (x, Call_ref (ty, e, l)); Return (Some (LocalGet y)) ] when x = y ->
      [ Return_call_ref (ty, e, l) ]
  | [ LocalSet (x, Call (symb, l)); Push (LocalGet y) ] when tail && x = y ->
      [ Return_call (symb, l) ]
  | [ LocalSet (x, Call_indirect (ty, e, l)); Push (LocalGet y) ] when tail && x = y ->
      [ Return_call_indirect (ty, e, l) ]
  | [ LocalSet (x, Call_ref (ty, e, l)); Push (LocalGet y) ] when tail && x = y ->
      [ Return_call_ref (ty, e, l) ]
  | i :: Nop :: rem -> instructions ~tail (i :: rem)
  | i :: i' :: Nop :: rem -> instructions ~tail (i :: i' :: rem)
  | i :: rem -> instruction ~tail:false i :: instructions ~tail rem

let f l = instructions ~tail:true l
