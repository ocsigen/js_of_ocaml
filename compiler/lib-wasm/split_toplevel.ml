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

(*

Split the toplevel function into smaller functions.

The toplevel function of a large program can be huge, which makes
optimizing compilers (Binaryen, TurboFan, ...) very slow, or even
makes them fail. Long-lived variables have already been moved to
globals (see [Globalize]), so there are many points in the function
where no local variable is live. The function can be cut at these
points: a run of consecutive instructions delimited by two such
points, and which does not depend on its context (no branch to an
enclosing label, no value left on the stack, ...), is outlined into
a separate function and replaced by a call to that function. This is
performed at every nesting level, innermost first.

Liveness is computed by a backward traversal of the structured
code, which also performs the outlining: once a sequence of
instructions has been traversed, we know at which points between its
instructions no local is live, and the sequence is split. Inner
sequences are traversed first, so outlining is performed innermost
first. We never cut inside a loop.

A branch escaping the outlined code (typically to the exception
handler wrapping the whole toplevel, or to a merge node) is
supported when it carries no value: the outlined function then
returns an integer code indicating how it exited, and the call site
performs the corresponding branch. The locals live at the branch
target must then hold the same values in both functions, which is
only guaranteed when they have not been written before the branch
(see [instruction]). A [return] is handled the same way (the
toplevel function always returns unit). Exceptions propagate through
the call to the outlined function; this is fine since an exception
handler can only read locals defined before the corresponding [try]
block, and such locals are live at the start of the outlined code
if they are read by the handler.
*)

open! Stdlib
module W = Wasm_ast

let debug = Debug.find "split"

(****)

(* Number of writes to each local. A local may be read on some path
   before being written; it then holds its default value, which it
   would also hold in an outlined function. So we do not consider
   such a local live before its first (textual) write. The backward
   traversal counts the writes it has seen: a local is written before
   the current point if some of its writes have not been seen yet. *)

let add_write counts x =
  Code.Var.Hashtbl.replace
    counts
    x
    (1 + Option.value (Code.Var.Hashtbl.find_opt counts x) ~default:0)

let rec count_expression counts e =
  match e with
  | W.Const _ | GlobalGet _ | Pop _ | RefFunc _ | RefNull _ | LocalGet _ -> ()
  | LocalTee (x, e') ->
      add_write counts x;
      count_expression counts e'
  | UnOp (_, e')
  | I32WrapI64 e'
  | I64ExtendI32 (_, e')
  | F32DemoteF64 e'
  | F64PromoteF32 e'
  | RefI31 e'
  | I31Get (_, e')
  | ArrayLen e'
  | StructGet (_, _, _, e')
  | RefCast (_, e')
  | RefTest (_, e')
  | Br_on_cast (_, _, _, e')
  | Br_on_cast_fail (_, _, _, e')
  | Br_on_null (_, e')
  | ExternConvertAny e'
  | AnyConvertExtern e' -> count_expression counts e'
  | BinOp (_, e', e'')
  | ArrayNew (_, e', e'')
  | ArrayNewData (_, _, e', e'')
  | ArrayGet (_, _, e', e'')
  | RefEq (e', e'') ->
      count_expression counts e';
      count_expression counts e''
  | Call (_, l) | ArrayNewFixed (_, l) | StructNew (_, l) -> count_expressions counts l
  | Call_ref (_, e', l) ->
      count_expression counts e';
      count_expressions counts l
  | BlockExpr (_, l) | Try (_, l, _) -> count_instructions counts l
  | Seq (l, e') ->
      count_instructions counts l;
      count_expression counts e'
  | IfExpr (_, e1, e2, e3) ->
      count_expression counts e1;
      count_expression counts e2;
      count_expression counts e3

and count_expressions counts l = List.iter ~f:(fun e -> count_expression counts e) l

and count_instruction counts i =
  match i with
  | W.Drop e
  | GlobalSet (_, e)
  | Br (_, Some e)
  | Br_if (_, e)
  | Br_table (e, _, _)
  | Throw (_, e)
  | Return (Some e)
  | Push e -> count_expression counts e
  | LocalSet (x, e) ->
      add_write counts x;
      count_expression counts e
  | Loop (_, l) | Block (_, l) -> count_instructions counts l
  | If (_, e, l1, l2) ->
      count_expression counts e;
      count_instructions counts l1;
      count_instructions counts l2
  | CallInstr (_, l) | Return_call (_, l) -> count_expressions counts l
  | Return_call_ref (_, e', l) ->
      count_expression counts e';
      count_expressions counts l
  | ArraySet (_, e1, e2, e3) ->
      count_expression counts e1;
      count_expression counts e2;
      count_expression counts e3
  | StructSet (_, _, e1, e2) ->
      count_expression counts e1;
      count_expression counts e2
  | Br (_, None) | Return None | Rethrow _ | Nop | Unreachable | Event _ -> ()

and count_instructions counts l = List.iter ~f:(fun i -> count_instruction counts i) l

(****)

(* How a piece of code may exit, besides falling through: by
   branching to an enclosing label, given by its depth relative to
   the code, or by returning from the toplevel function. *)
type exit =
  | Br_exit of int
  | Return_exit

let exit_equal e e' =
  match e, e' with
  | Br_exit i, Br_exit j -> i = j
  | Return_exit, Return_exit -> true
  | Br_exit _, Return_exit | Return_exit, Br_exit _ -> false

(* The value returned by the toplevel function *)
let unit_value = W.RefI31 (Const (I32 0l))

let add_exit e l = if List.exists ~f:(fun e' -> exit_equal e e') l then l else e :: l

let union_exits l l' = List.fold_left ~f:(fun l e -> add_exit e l) ~init:l l'

let add_label lbl l = if List.mem lbl ~eq:Int.equal l then l else lbl :: l

(* A function outlined from the toplevel function. Its body is
   mutable so that the location in effect at the start of the
   outlined code can be added once known. *)
type outlined =
  { fname : Code.Var.t
  ; result : W.value_type list
  ; flocals : (Code.Var.t * W.value_type) list
  ; mutable fbody : W.instruction list
  }

(* Locations are stateful: an [Event] applies to the code until the
   next one. So an outlined function must start with the location in
   effect at the start of the outlined code, if it does not start
   with an [Event] itself. *)
let set_start_location loc f =
  match loc, f.fbody with
  | None, _ | _, W.Event _ :: _ -> ()
  | Some loc, _ -> f.fbody <- W.Event loc :: f.fbody

(* Information about a piece of code:
   - its size;
   - the ways it may exit that can be emulated by an outlined
     function returning a code;
   - the enclosing labels (relative depth) targeted by branches
     that cannot be emulated (branches carrying a value, multi-way
     branches, exception catches);
   - whether it may exit in some other way we do not support:
     return of a value other than unit, tail calls, [rethrow], and
     [pop] (which reads an exception value from the stack);
   - the location of its last [Event];
   - the functions outlined from it which are not preceded by an
     [Event] in the code: they must start with the location in
     effect at the start of the code, which is not known yet. *)
type info =
  { weight : int
  ; exits : exit list
  ; blocked : int list
  ; unsupported : bool
  ; last_event : Parse_info.t option
  ; pending : outlined list
  }

let empty =
  { weight = 0
  ; exits = []
  ; blocked = []
  ; unsupported = false
  ; last_event = None
  ; pending = []
  }

let leaf = { empty with weight = 1 }

let unsupported = { leaf with unsupported = true }

(* Information about code [i] followed by code [i']. *)
let merge i i' =
  { weight = i.weight + i'.weight
  ; exits = union_exits i.exits i'.exits
  ; blocked = List.fold_left ~f:(fun l lbl -> add_label lbl l) ~init:i.blocked i'.blocked
  ; unsupported = i.unsupported || i'.unsupported
  ; last_event =
      (match i'.last_event with
      | Some _ -> i'.last_event
      | None -> i.last_event)
  ; pending =
      (match i.last_event with
      | None -> i.pending @ i'.pending
      | Some _ ->
          List.iter ~f:(fun f -> set_start_location i.last_event f) i'.pending;
          i.pending)
  }

let node i = { i with weight = i.weight + 1 }

(* Crossing a labeled construct: label 0 refers to the construct
   itself, so does not escape. *)
let shift i =
  { i with
    exits =
      List.filter_map
        ~f:(fun e ->
          match e with
          | Br_exit 0 -> None
          | Br_exit d -> Some (Br_exit (d - 1))
          | Return_exit -> Some e)
        i.exits
  ; blocked = List.filter_map ~f:(fun d -> if d = 0 then None else Some (d - 1)) i.blocked
  }

(* The instruction leaves nothing on the stack. *)
let is_stack_neutral i =
  match i with
  | W.Push _ -> false
  | Loop (ty, _) | Block (ty, _) | If (ty, _, _, _) ->
      List.is_empty ty.params && List.is_empty ty.result
  | CallInstr _ ->
      (* The functions called this way return nothing, except
         [caml_wrap_exception], whose result is the value of the
         enclosing block. It is only called in the body of this block,
         with an argument containing a [try] whose catch targets this
         block. Hence it is never outlined. *)
      true
  | Drop _
  | LocalSet _
  | GlobalSet _
  | Br_table _
  | Br _
  | Br_if _
  | Return _
  | Nop
  | Throw _
  | Rethrow _
  | ArraySet _
  | StructSet _
  | Return_call _
  | Return_call_ref _
  | Unreachable
  | Event _ -> true

(****)

(* Extraction of a run of instructions: collect the locals it uses
   and replace the escaping branches by the return of a code. [depth]
   is the number of labels between the outlined code and the current
   point. *)

type collector =
  { locals : unit Code.Var.Hashtbl.t
  ; exit_codes : exit list option
        (* When set, escaping branches are rewritten; code n
           corresponds to element n - 1 *)
  }

let exit_code c e =
  let rec find l i =
    match l with
    | [] -> assert false
    | e' :: rem -> if exit_equal e e' then i else find rem (i + 1)
  in
  match c.exit_codes with
  | Some codes -> W.Const (I32 (Int32.of_int (find codes 1)))
  | None -> assert false

let escapes c depth i = Option.is_some c.exit_codes && i >= depth

let rec collect_expression c depth e =
  match e with
  | W.Const _ | GlobalGet _ | Pop _ | RefFunc _ | RefNull _ -> e
  | LocalGet x ->
      Code.Var.Hashtbl.replace c.locals x ();
      e
  | LocalTee (x, e') ->
      Code.Var.Hashtbl.replace c.locals x ();
      LocalTee (x, collect_expression c depth e')
  | UnOp (op, e') -> UnOp (op, collect_expression c depth e')
  | I32WrapI64 e' -> I32WrapI64 (collect_expression c depth e')
  | I64ExtendI32 (s, e') -> I64ExtendI32 (s, collect_expression c depth e')
  | F32DemoteF64 e' -> F32DemoteF64 (collect_expression c depth e')
  | F64PromoteF32 e' -> F64PromoteF32 (collect_expression c depth e')
  | RefI31 e' -> RefI31 (collect_expression c depth e')
  | I31Get (s, e') -> I31Get (s, collect_expression c depth e')
  | ArrayLen e' -> ArrayLen (collect_expression c depth e')
  | StructGet (s, ty, i, e') -> StructGet (s, ty, i, collect_expression c depth e')
  | RefCast (ty, e') -> RefCast (ty, collect_expression c depth e')
  | RefTest (ty, e') -> RefTest (ty, collect_expression c depth e')
  | Br_on_cast (i, ty, ty', e') ->
      assert (not (escapes c depth i));
      Br_on_cast (i, ty, ty', collect_expression c depth e')
  | Br_on_cast_fail (i, ty, ty', e') ->
      assert (not (escapes c depth i));
      Br_on_cast_fail (i, ty, ty', collect_expression c depth e')
  | Br_on_null (i, e') ->
      assert (not (escapes c depth i));
      Br_on_null (i, collect_expression c depth e')
  | ExternConvertAny e' -> ExternConvertAny (collect_expression c depth e')
  | AnyConvertExtern e' -> AnyConvertExtern (collect_expression c depth e')
  | BinOp (op, e', e'') ->
      let e' = collect_expression c depth e' in
      BinOp (op, e', collect_expression c depth e'')
  | ArrayNew (ty, e', e'') ->
      let e' = collect_expression c depth e' in
      ArrayNew (ty, e', collect_expression c depth e'')
  | ArrayNewData (ty, i, e', e'') ->
      let e' = collect_expression c depth e' in
      ArrayNewData (ty, i, e', collect_expression c depth e'')
  | ArrayGet (s, ty, e', e'') ->
      let e' = collect_expression c depth e' in
      ArrayGet (s, ty, e', collect_expression c depth e'')
  | RefEq (e', e'') ->
      let e' = collect_expression c depth e' in
      RefEq (e', collect_expression c depth e'')
  | Call (f, l) -> Call (f, collect_expressions c depth l)
  | ArrayNewFixed (ty, l) -> ArrayNewFixed (ty, collect_expressions c depth l)
  | StructNew (ty, l) -> StructNew (ty, collect_expressions c depth l)
  | Call_ref (ty, e', l) ->
      let l = collect_expressions c depth l in
      Call_ref (ty, collect_expression c depth e', l)
  | BlockExpr (ty, l) -> BlockExpr (ty, collect_instructions c (depth + 1) l)
  | Try (ty, l, catches) ->
      List.iter ~f:(fun (_, i, _) -> assert (not (escapes c depth i))) catches;
      Try (ty, collect_instructions c (depth + 1) l, catches)
  | Seq (l, e') ->
      let l = collect_instructions c depth l in
      Seq (l, collect_expression c depth e')
  | IfExpr (ty, e1, e2, e3) ->
      let e1 = collect_expression c depth e1 in
      let e2 = collect_expression c (depth + 1) e2 in
      IfExpr (ty, e1, e2, collect_expression c (depth + 1) e3)

and collect_expressions c depth l = List.map ~f:(fun e -> collect_expression c depth e) l

and collect_instruction c depth i =
  match i with
  | W.Drop e -> W.Drop (collect_expression c depth e)
  | LocalSet (x, e) ->
      Code.Var.Hashtbl.replace c.locals x ();
      LocalSet (x, collect_expression c depth e)
  | GlobalSet (x, e) -> GlobalSet (x, collect_expression c depth e)
  | Br (i, None) when escapes c depth i ->
      Return (Some (exit_code c (Br_exit (i - depth))))
  | Br (i, e) ->
      assert (not (escapes c depth i));
      Br (i, Option.map ~f:(fun e -> collect_expression c depth e) e)
  | Br_if (i, e) ->
      let e = collect_expression c depth e in
      if escapes c depth i
      then
        If
          ( { params = []; result = [] }
          , e
          , [ Return (Some (exit_code c (Br_exit (i - depth)))) ]
          , [] )
      else Br_if (i, e)
  | Br_table (e, l, i) ->
      List.iter ~f:(fun i -> assert (not (escapes c depth i))) (i :: l);
      Br_table (collect_expression c depth e, l, i)
  | Throw (t, e) -> Throw (t, collect_expression c depth e)
  | Return (Some e) when Option.is_some c.exit_codes ->
      assert (Poly.equal e unit_value);
      Return (Some (exit_code c Return_exit))
  | Return (Some e) -> Return (Some (collect_expression c depth e))
  | Push e -> Push (collect_expression c depth e)
  | Loop (ty, l) -> Loop (ty, collect_instructions c (depth + 1) l)
  | Block (ty, l) -> Block (ty, collect_instructions c (depth + 1) l)
  | If (ty, e, l1, l2) ->
      let e = collect_expression c depth e in
      let l1 = collect_instructions c (depth + 1) l1 in
      If (ty, e, l1, collect_instructions c (depth + 1) l2)
  | CallInstr (f, l) -> CallInstr (f, collect_expressions c depth l)
  | ArraySet (ty, e1, e2, e3) ->
      let e1 = collect_expression c depth e1 in
      let e2 = collect_expression c depth e2 in
      ArraySet (ty, e1, e2, collect_expression c depth e3)
  | StructSet (ty, i, e1, e2) ->
      let e1 = collect_expression c depth e1 in
      StructSet (ty, i, e1, collect_expression c depth e2)
  | Return_call (f, l) ->
      assert (Option.is_none c.exit_codes);
      Return_call (f, collect_expressions c depth l)
  | Return_call_ref (ty, e', l) ->
      assert (Option.is_none c.exit_codes);
      let l = collect_expressions c depth l in
      Return_call_ref (ty, collect_expression c depth e', l)
  | Return None | Rethrow _ ->
      assert (Option.is_none c.exit_codes);
      i
  | Nop | Unreachable | Event _ -> i

and collect_instructions c depth l =
  List.map ~f:(fun i -> collect_instruction c depth i) l

(****)

(* The traversal. Each function takes the locals live after the code
   and returns the locals live before it, the rewritten code, and
   information about it. Subexpressions are traversed in reverse
   evaluation order. *)

type state =
  { total_writes : int Code.Var.Hashtbl.t
  ; mutable seen_writes : int Code.Var.Map.t
        (* Number of writes to each local after the current point *)
  ; mutable loop_live : (W.instruction list * Code.Var.Set.t) list
        (* Locals live at the start of the loops nested in the current
           outermost loop, indexed by loop body *)
  ; local_types : W.value_type Code.Var.Hashtbl.t
  ; name : Code.Var.t
  ; mutable functions : outlined list
  }

type env =
  { labels : Code.Var.Set.t list (* Locals live when branching to the enclosing labels *)
  ; exn_live : Code.Var.Set.t
        (* Locals live when an exception is raised: they are live when
           reaching the enclosing exception handlers *)
  ; loop_seen : int Code.Var.Map.t option
        (* Writes seen at the end of the outermost enclosing loop *)
  }

let label env i = List.nth env.labels i

let push_label env live = { env with labels = live :: env.labels }

let seen_count seen x = Option.value (Code.Var.Map.find_opt x seen) ~default:0

let write st x =
  st.seen_writes <- Code.Var.Map.add x (seen_count st.seen_writes x + 1) st.seen_writes

(* Whether [x] is written before the point where the writes [seen]
   have been seen. *)
let written_before st seen x =
  seen_count seen x
  < Option.value (Code.Var.Hashtbl.find_opt st.total_writes x) ~default:0

let local_declarations st locals =
  Code.Var.Hashtbl.fold
    (fun x () acc -> (x, Code.Var.Hashtbl.find st.local_types x) :: acc)
    locals
    []
  |> List.sort ~cmp:(fun (x, _) (y, _) -> Code.Var.compare x y)

(* The instruction never falls through, so the code following it is
   unreachable and does not have to be well-typed. *)
let terminates i =
  match i with
  | W.Br (_, None) | Return _ | Throw _ | Unreachable -> true
  | _ -> false

(* Outline a run of instructions [body] with information [info].
   Returns the instructions replacing it and their information.
   [start] is the location in effect at the start of the run, when
   known ([None] if it is inherited from the enclosing code). *)
let outline st ~start body info =
  let exits = info.exits in
  let c = { locals = Code.Var.Hashtbl.create 16; exit_codes = Some exits } in
  (* Attribute the call site to the location at the start of the
     run *)
  let leading_event =
    match body with
    | W.Event loc :: _ -> Some loc
    | _ -> None
  in
  (* If the run always exits before its end, the call site must not
     fall through either (the stack may not be well-typed there). *)
  let epilogue =
    match List.rev body with
    | i :: _ when terminates i -> [ W.Unreachable ]
    | _ -> []
  in
  let body = collect_instructions c 0 body in
  let f = Code.Var.fork st.name in
  let result, body =
    match exits with
    | [] -> [], body
    | _ -> [ (W.I32 : W.value_type) ], body @ [ W.Push (Const (I32 0l)) ]
  in
  let fn =
    { fname = f; result; flocals = local_declarations st c.locals; fbody = body }
  in
  st.functions <- fn :: st.functions;
  let exit_instr e code =
    match e with
    | Br_exit d -> W.Br_if (d, code)
    | Return_exit ->
        If ({ params = []; result = [] }, code, [ Return (Some unit_value) ], [])
  in
  let call_site =
    match exits with
    | [] -> [ W.CallInstr (f, []) ]
    | [ e ] -> [ exit_instr e (Call (f, [])) ]
    | _ ->
        let r = Code.Var.fresh_n "exit" in
        Code.Var.Hashtbl.add st.local_types r (W.I32 : W.value_type);
        W.LocalSet (r, Call (f, []))
        :: List.mapi
             ~f:(fun i e ->
               exit_instr
                 e
                 (BinOp (I32 Eq, LocalGet r, Const (I32 (Int32.of_int (i + 1))))))
             exits
  in
  (* Restore the location in effect at the end of the run *)
  let restore =
    let call_location =
      match leading_event, start with
      | Some _, _ -> Some leading_event
      | None, start -> start
    in
    match info.last_event with
    | Some loc
      when List.is_empty epilogue && not (Poly.equal (Some info.last_event) call_location)
      -> [ W.Event loc ]
    | _ -> []
  in
  let l =
    (match leading_event with
      | Some loc -> [ W.Event loc ]
      | None -> [])
    @ call_site
    @ restore
    @ epilogue
  in
  ( l
  , { empty with
      weight = List.length l + 1
    ; exits
    ; last_event = info.last_event
    ; pending = fn :: info.pending
    } )

let rec expression st env e live =
  let unary f e' =
    let live, e', i = expression st env e' live in
    live, f e', node i
  in
  let binary f e1 e2 =
    let live, e2, i2 = expression st env e2 live in
    let live, e1, i1 = expression st env e1 live in
    live, f e1 e2, node (merge i1 i2)
  in
  (* A branch carrying a value *)
  let branch lbl f e' =
    let live, e', i = expression st env e' (Code.Var.Set.union live (label env lbl)) in
    live, f e', node { i with blocked = add_label lbl i.blocked }
  in
  match e with
  | W.Const _ | GlobalGet _ | RefFunc _ | RefNull _ -> live, e, leaf
  | Pop _ -> live, e, unsupported
  | LocalGet x -> Code.Var.Set.add x live, e, leaf
  | LocalTee (x, e') ->
      write st x;
      let live, e', i = expression st env e' (Code.Var.Set.remove x live) in
      live, LocalTee (x, e'), node i
  | UnOp (op, e') -> unary (fun e' -> W.UnOp (op, e')) e'
  | I32WrapI64 e' -> unary (fun e' -> W.I32WrapI64 e') e'
  | I64ExtendI32 (s, e') -> unary (fun e' -> W.I64ExtendI32 (s, e')) e'
  | F32DemoteF64 e' -> unary (fun e' -> W.F32DemoteF64 e') e'
  | F64PromoteF32 e' -> unary (fun e' -> W.F64PromoteF32 e') e'
  | RefI31 e' -> unary (fun e' -> W.RefI31 e') e'
  | I31Get (s, e') -> unary (fun e' -> W.I31Get (s, e')) e'
  | ArrayLen e' -> unary (fun e' -> W.ArrayLen e') e'
  | StructGet (s, ty, idx, e') -> unary (fun e' -> W.StructGet (s, ty, idx, e')) e'
  | RefCast (ty, e') -> unary (fun e' -> W.RefCast (ty, e')) e'
  | RefTest (ty, e') -> unary (fun e' -> W.RefTest (ty, e')) e'
  | ExternConvertAny e' -> unary (fun e' -> W.ExternConvertAny e') e'
  | AnyConvertExtern e' -> unary (fun e' -> W.AnyConvertExtern e') e'
  | Br_on_cast (lbl, ty, ty', e') ->
      branch lbl (fun e' -> W.Br_on_cast (lbl, ty, ty', e')) e'
  | Br_on_cast_fail (lbl, ty, ty', e') ->
      branch lbl (fun e' -> W.Br_on_cast_fail (lbl, ty, ty', e')) e'
  | Br_on_null (lbl, e') -> branch lbl (fun e' -> W.Br_on_null (lbl, e')) e'
  | BinOp (op, e1, e2) -> binary (fun e1 e2 -> W.BinOp (op, e1, e2)) e1 e2
  | ArrayNew (ty, e1, e2) -> binary (fun e1 e2 -> W.ArrayNew (ty, e1, e2)) e1 e2
  | ArrayNewData (ty, d, e1, e2) ->
      binary (fun e1 e2 -> W.ArrayNewData (ty, d, e1, e2)) e1 e2
  | ArrayGet (s, ty, e1, e2) -> binary (fun e1 e2 -> W.ArrayGet (s, ty, e1, e2)) e1 e2
  | RefEq (e1, e2) -> binary (fun e1 e2 -> W.RefEq (e1, e2)) e1 e2
  | Call (f, l) ->
      let live, l, i = expressions st env l live in
      live, Call (f, l), node i
  | ArrayNewFixed (ty, l) ->
      let live, l, i = expressions st env l live in
      live, ArrayNewFixed (ty, l), node i
  | StructNew (ty, l) ->
      let live, l, i = expressions st env l live in
      live, StructNew (ty, l), node i
  | Call_ref (ty, e', l) ->
      let live, e', i' = expression st env e' live in
      let live, l, i = expressions st env l live in
      live, Call_ref (ty, e', l), node (merge i i')
  | BlockExpr (ty, l) ->
      let live, l, i = instructions st (push_label env live) l live in
      live, BlockExpr (ty, l), node (shift i)
  | Try (ty, l, catches) ->
      let exn_live =
        List.fold_left
          ~f:(fun s (_, i, _) -> Code.Var.Set.union s (label env i))
          ~init:env.exn_live
          catches
      in
      let live, l, i =
        instructions st { env with labels = live :: env.labels; exn_live } l live
      in
      (* The catch labels are relative to the outside of the [try]
         block, and carry a value. *)
      let i = shift i in
      let i =
        { i with
          blocked =
            List.fold_left
              ~f:(fun blocked (_, lbl, _) -> add_label lbl blocked)
              ~init:i.blocked
              catches
        }
      in
      live, Try (ty, l, catches), node i
  | Seq (l, e') ->
      let live, e', i' = expression st env e' live in
      let live, l, i = instructions st env l live in
      live, Seq (l, e'), node (merge i i')
  | IfExpr (ty, e1, e2, e3) ->
      let env' = push_label env live in
      let live3, e3, i3 = expression st env' e3 live in
      let live2, e2, i2 = expression st env' e2 live in
      let live, e1, i1 = expression st env e1 (Code.Var.Set.union live2 live3) in
      live, IfExpr (ty, e1, e2, e3), node (merge i1 (shift (merge i2 i3)))

and expressions st env l live =
  List.fold_right
    ~f:(fun e (live, l, i) ->
      let live, e, i' = expression st env e live in
      live, e :: l, merge i' i)
    l
    ~init:(live, [], empty)

and instruction st env i live =
  (* When a branch escapes an outlined function, the function returns
     an exit code and the caller performs the branch. The locals live
     at the branch target are then those of the caller, which are not
     updated by the outlined function. So the branch can only be
     emulated this way if these locals have not been written before
     the branch (they then hold their default value in both
     functions). Inside a loop, a write anywhere in the loop may be
     executed before the branch. Otherwise, the branch blocks
     outlining. *)
  let seen = Option.value env.loop_seen ~default:st.seen_writes in
  let branch lbl i =
    if Code.Var.Set.exists (fun x -> written_before st seen x) (label env lbl)
    then { i with blocked = add_label lbl i.blocked }
    else { i with exits = add_exit (Br_exit lbl) i.exits }
  in
  let unary f e =
    let live, e, i = expression st env e live in
    live, f e, node i
  in
  let live, i, info =
    match i with
    | W.Drop e -> unary (fun e -> W.Drop e) e
    | GlobalSet (x, e) -> unary (fun e -> W.GlobalSet (x, e)) e
    | Push e -> unary (fun e -> W.Push e) e
    | Throw (t, e) ->
        let live, e, i = expression st env e Code.Var.Set.empty in
        live, Throw (t, e), node i
    | LocalSet (x, e) ->
        write st x;
        let live, e, i = expression st env e (Code.Var.Set.remove x live) in
        live, LocalSet (x, e), node i
    | Br (lbl, None) -> label env lbl, i, branch lbl leaf
    | Br (lbl, Some e) ->
        let live, e, i = expression st env e (label env lbl) in
        live, Br (lbl, Some e), node { i with blocked = add_label lbl i.blocked }
    | Br_if (lbl, e) ->
        let live, e, i = expression st env e (Code.Var.Set.union live (label env lbl)) in
        live, Br_if (lbl, e), node (branch lbl i)
    | Br_table (e, l, lbl) ->
        let live, e, i =
          expression
            st
            env
            e
            (List.fold_left
               ~f:(fun s lbl -> Code.Var.Set.union s (label env lbl))
               ~init:Code.Var.Set.empty
               (lbl :: l))
        in
        ( live
        , Br_table (e, l, lbl)
        , node
            { i with
              blocked =
                List.fold_left ~f:(fun l lbl -> add_label lbl l) ~init:i.blocked (lbl :: l)
            } )
    | Return (Some e) when Poly.equal e unit_value ->
        Code.Var.Set.empty, i, { leaf with weight = 2; exits = [ Return_exit ] }
    | Return (Some e) ->
        let live, e, i = expression st env e Code.Var.Set.empty in
        live, Return (Some e), node { i with unsupported = true }
    | Return_call (f, l) ->
        let live, l, i = expressions st env l Code.Var.Set.empty in
        live, Return_call (f, l), node { i with unsupported = true }
    | Return_call_ref (ty, e', l) ->
        let live, e', i' = expression st env e' Code.Var.Set.empty in
        let live, l, i = expressions st env l live in
        live, Return_call_ref (ty, e', l), node { (merge i i') with unsupported = true }
    | Return None | Rethrow _ -> Code.Var.Set.empty, i, unsupported
    | Unreachable -> Code.Var.Set.empty, i, leaf
    | Nop -> live, i, leaf
    | Event loc -> live, i, { leaf with last_event = Some loc }
    | Block (ty, l) ->
        let live, l, i = instructions st (push_label env live) l live in
        live, Block (ty, l), node (shift i)
    | Loop (ty, body) ->
        (* Iterate to a fixpoint. When the loop is nested in another
           loop, its context (the locals live after it and at the
           enclosing labels) only grows from one iteration of the
           enclosing loop to the next, so we can start from the
           previous result rather than from the empty set. Otherwise,
           the cost would be exponential in the nesting depth. *)
        let outermost = Option.is_none env.loop_seen in
        let seen = st.seen_writes in
        let env = if outermost then { env with loop_seen = Some seen } else env in
        let rec fixpoint live_in =
          st.seen_writes <- seen;
          let ((live_in', _, _) as res) =
            instructions st (push_label env live_in) body live
          in
          if Code.Var.Set.subset live_in' live_in
          then res
          else fixpoint (Code.Var.Set.union live_in live_in')
        in
        let live_in, l, i =
          fixpoint
            (match
               List.find_opt ~f:(fun (body', _) -> phys_equal body body') st.loop_live
             with
            | Some (_, live_in) -> live_in
            | None -> Code.Var.Set.empty)
        in
        st.loop_live <-
          (if outermost
           then []
           else
             (body, live_in)
             :: List.filter
                  ~f:(fun (body', _) -> not (phys_equal body body'))
                  st.loop_live);
        live_in, Loop (ty, l), node (shift i)
    | If (ty, e, l1, l2) ->
        let env' = push_label env live in
        let live2, l2, i2 = instructions st env' l2 live in
        let live1, l1, i1 = instructions st env' l1 live in
        let live, e, i = expression st env e (Code.Var.Set.union live1 live2) in
        live, If (ty, e, l1, l2), node (merge i (shift (merge i1 i2)))
    | CallInstr (f, l) ->
        let live, l, i = expressions st env l live in
        live, CallInstr (f, l), node i
    | ArraySet (ty, e1, e2, e3) ->
        let live, e3, i3 = expression st env e3 live in
        let live, e2, i2 = expression st env e2 live in
        let live, e1, i1 = expression st env e1 live in
        live, ArraySet (ty, e1, e2, e3), node (merge i1 (merge i2 i3))
    | StructSet (ty, idx, e1, e2) ->
        let live, e2, i2 = expression st env e2 live in
        let live, e1, i1 = expression st env e1 live in
        live, StructSet (ty, idx, e1, e2), node (merge i1 i2)
  in
  Code.Var.Set.union live env.exn_live, i, info

and instructions st env l live =
  (* Traverse the instructions, remembering whether no local is live
     before each of them *)
  let live_end = live in
  let live, items =
    List.fold_right
      ~f:(fun i (live, items) ->
        let live, i, info = instruction st env i live in
        (* Discard the locals which are not written before this
           point *)
        let live =
          Code.Var.Set.filter (fun x -> written_before st st.seen_writes x) live
        in
        live, (Code.Var.Set.is_empty live, i, info) :: items)
      l
      ~init:(live, [])
  in
  let l, info =
    if Option.is_some env.loop_seen
    then
      (* We never cut inside a loop *)
      let l, i =
        List.fold_left
          ~f:(fun (l, i) (_, instr, info) -> instr :: l, merge i info)
          ~init:([], empty)
          items
      in
      List.rev l, i
    else split st items (Code.Var.Set.is_empty live_end)
  in
  live, l, info

(* Group the instructions of a sequence into runs delimited by points
   where no local is live, and outline the runs which are large enough
   and only contain instructions that can be outlined. [items] gives,
   for each instruction, whether it is preceded by such a point;
   [cut_end] tells whether the end of the sequence is such a
   point. *)
and split st items cut_end =
  (* Size (in AST nodes) above which a run of instructions is
     outlined. *)
  let threshold = Config.Param.toplevel_split_size () in
  let outlinable i info =
    is_stack_neutral i && List.is_empty info.blocked && not info.unsupported
  in
  (* [acc] contains the output instructions, reversed; [pending] the
     current run, reversed, together with its accumulated info;
     [pending_ok] indicates whether the run started at a cut point
     and only contains outlinable instructions, in which case it is a
     candidate for outlining. *)
  let flush acc pending = List.rev_append (List.rev pending) acc in
  let outline_pending acc pending pending_info start =
    let l, info = outline st ~start (List.rev pending) pending_info in
    List.rev_append l acc, info
  in
  (* [start] is the location in effect at the start of the current
     run and [loc] the one in effect before the next instruction, when
     known (see [outline]). *)
  let rec loop items acc acc_info pending pending_info pending_ok start loc =
    match items with
    | [] ->
        if pending_ok && cut_end && pending_info.weight >= threshold
        then
          let acc, info = outline_pending acc pending pending_info start in
          List.rev acc, merge acc_info info
        else List.rev (flush acc pending), merge acc_info pending_info
    | (cut, i, info) :: rem ->
        let acc, acc_info, pending, pending_info, pending_ok, start =
          if cut
          then
            if pending_ok && pending_info.weight >= threshold
            then
              let acc, info = outline_pending acc pending pending_info start in
              acc, merge acc_info info, [], empty, true, loc
            else if not pending_ok
            then flush acc pending, merge acc_info pending_info, [], empty, true, loc
            else acc, acc_info, pending, pending_info, true, start
          else acc, acc_info, pending, pending_info, pending_ok, start
        in
        let loc =
          match info.last_event with
          | Some _ -> Some info.last_event
          | None -> loc
        in
        if outlinable i info
        then
          loop
            rem
            acc
            acc_info
            (i :: pending)
            (merge pending_info info)
            pending_ok
            start
            loc
        else
          (* This instruction cannot be outlined: emit the current
             run and the instruction as they are. *)
          loop
            rem
            (i :: flush acc pending)
            (merge acc_info (merge pending_info info))
            []
            empty
            false
            loc
            loc
  in
  (* The run can only start at a cut point; the first instruction is
     handled like any other one. *)
  loop items [] empty [] empty false None None

(****)

let f ~name ~locals body =
  let total_writes = Code.Var.Hashtbl.create 256 in
  count_instructions total_writes body;
  let local_types = Code.Var.Hashtbl.create 256 in
  List.iter ~f:(fun (x, ty) -> Code.Var.Hashtbl.add local_types x ty) locals;
  let st =
    { total_writes
    ; seen_writes = Code.Var.Map.empty
    ; loop_live = []
    ; local_types
    ; name
    ; functions = []
    }
  in
  let env = { labels = []; exn_live = Code.Var.Set.empty; loop_seen = None } in
  let _, body, info = instructions st env body Code.Var.Set.empty in
  (* All the writes have been seen by the traversal *)
  assert (
    Code.Var.Hashtbl.fold
      (fun x n ok -> ok && seen_count st.seen_writes x = n)
      total_writes
      true);
  if debug ()
  then
    Format.eprintf
      "Toplevel function split into %d pieces (residual size: %d)@."
      (List.length st.functions)
      info.weight;
  (* Only keep the locals still used in the residual function. *)
  let c = { locals = Code.Var.Hashtbl.create 256; exit_codes = None } in
  let body = collect_instructions c 0 body in
  ( local_declarations st c.locals
  , body
  , List.rev_map
      ~f:(fun { fname; result; flocals; fbody } ->
        W.Function
          { name = fname
          ; exported_name = None
          ; typ = None
          ; signature = { params = []; result }
          ; param_names = []
          ; locals = flocals
          ; body = fbody
          })
      st.functions )
