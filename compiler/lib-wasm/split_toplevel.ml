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

Liveness is computed in a very cheap but conservative way: a local is
live at a program point if it is written somewhere textually before
this point, and read somewhere textually after it. This is sound
outside of loops, since the only backward jumps in Wasm code are
branches to a [loop] label. So we never cut inside a loop.

A branch escaping the outlined code (typically to the exception
handler wrapping the whole toplevel, or to a merge node) is
supported when it carries no value: the outlined function then
returns an integer code indicating how it exited, and the call site
performs the corresponding branch. A [return] is handled the same
way (the toplevel function always returns unit).
*)

open! Stdlib
module W = Wasm_ast

let debug = Debug.find "split"

(****)

(* Pass 1: liveness analysis. The instructions are numbered in a
   pre-order traversal; the counter is incremented when entering and
   when leaving an instruction, and between the condition and the
   branches of an [if] and between its branches, so that the
   boundaries between two consecutive instructions, and the start and
   end of each sequence of instructions, get their own positions. We
   record at which positions no local is live.

   A local may be read on some path before being written; it then
   holds its default value, which it would also hold in an outlined
   function. So we do not consider such a local live before its first
   (textual) write. The positions of the first writes are computed by
   a forward traversal. Then the liveness analysis is performed
   backwards, so the positions are negative and are shifted once the
   total number of positions is known. *)

type writes =
  { mutable wpos : int
  ; first_write : int Code.Var.Hashtbl.t
  }

let wtick st = st.wpos <- st.wpos + 1

let write st x =
  if not (Code.Var.Hashtbl.mem st.first_write x)
  then Code.Var.Hashtbl.add st.first_write x st.wpos

let rec write_expression st e =
  match e with
  | W.Const _ | GlobalGet _ | Pop _ | RefFunc _ | RefNull _ | LocalGet _ -> ()
  | LocalTee (x, e') ->
      write_expression st e';
      write st x
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
  | AnyConvertExtern e' -> write_expression st e'
  | BinOp (_, e', e'')
  | ArrayNew (_, e', e'')
  | ArrayNewData (_, _, e', e'')
  | ArrayGet (_, _, e', e'')
  | RefEq (e', e'') ->
      write_expression st e';
      write_expression st e''
  | Call (_, l) | ArrayNewFixed (_, l) | StructNew (_, l) -> write_expressions st l
  | Call_ref (_, e', l) ->
      write_expressions st l;
      write_expression st e'
  | BlockExpr (_, l) | Try (_, l, _) -> write_instructions st l
  | Seq (l, e') ->
      write_instructions st l;
      write_expression st e'
  | IfExpr (_, e1, e2, e3) ->
      write_expression st e1;
      wtick st;
      write_expression st e2;
      wtick st;
      write_expression st e3

and write_expressions st l = List.iter ~f:(fun e -> write_expression st e) l

and write_instruction st i =
  wtick st;
  (match i with
  | W.Drop e
  | GlobalSet (_, e)
  | Br (_, Some e)
  | Br_if (_, e)
  | Br_table (e, _, _)
  | Throw (_, e)
  | Return (Some e)
  | Push e -> write_expression st e
  | LocalSet (x, e) ->
      write_expression st e;
      write st x
  | Loop (_, l) | Block (_, l) -> write_instructions st l
  | If (_, e, l1, l2) ->
      write_expression st e;
      wtick st;
      write_instructions st l1;
      wtick st;
      write_instructions st l2
  | CallInstr (_, l) | Return_call (_, l) -> write_expressions st l
  | Return_call_ref (_, e', l) ->
      write_expressions st l;
      write_expression st e'
  | ArraySet (_, e1, e2, e3) ->
      write_expression st e1;
      write_expression st e2;
      write_expression st e3
  | StructSet (_, _, e1, e2) ->
      write_expression st e1;
      write_expression st e2
  | Br (_, None) | Return None | Rethrow _ | Nop | Unreachable | Event _ -> ());
  wtick st

and write_instructions st l = List.iter ~f:(fun i -> write_instruction st i) l

type liveness =
  { mutable pos : int
  ; cuts : unit Int.Hashtbl.t
  ; first_write : int Code.Var.Hashtbl.t
  ; count : int (* total number of positions *)
  }

type env =
  { labels : Code.Var.Set.t list (* Locals live when branching to the enclosing labels *)
  ; exn_live : Code.Var.Set.t
        (* Locals live when an exception is raised: they are live when
           reaching the enclosing exception handlers *)
  }

let untick st = st.pos <- st.pos - 1

let label env i = List.nth env.labels i

let push_label env live = { env with labels = live :: env.labels }

(* [live_expression env e live] returns the locals live before
   evaluating [e], given the locals [live] live after. Subexpressions
   are traversed in reverse evaluation order. *)
let rec live_expression st env e live =
  match e with
  | W.Const _ | GlobalGet _ | Pop _ | RefFunc _ | RefNull _ -> live
  | LocalGet x -> Code.Var.Set.add x live
  | LocalTee (x, e') -> live_expression st env e' (Code.Var.Set.remove x live)
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
  | ExternConvertAny e'
  | AnyConvertExtern e' -> live_expression st env e' live
  | Br_on_cast (i, _, _, e') | Br_on_cast_fail (i, _, _, e') | Br_on_null (i, e') ->
      live_expression st env e' (Code.Var.Set.union live (label env i))
  | BinOp (_, e', e'')
  | ArrayNew (_, e', e'')
  | ArrayNewData (_, _, e', e'')
  | ArrayGet (_, _, e', e'')
  | RefEq (e', e'') -> live_expression st env e' (live_expression st env e'' live)
  | Call (_, l) | ArrayNewFixed (_, l) | StructNew (_, l) ->
      live_expressions st env l live
  | Call_ref (_, e', l) -> live_expressions st env l (live_expression st env e' live)
  | BlockExpr (_, l) -> live_instructions st (push_label env live) l live
  | Try (_, l, catches) ->
      let exn_live =
        List.fold_left
          ~f:(fun s (_, i, _) -> Code.Var.Set.union s (label env i))
          ~init:env.exn_live
          catches
      in
      live_instructions st { labels = live :: env.labels; exn_live } l live
  | Seq (l, e') -> live_instructions st env l (live_expression st env e' live)
  | IfExpr (_, e1, e2, e3) ->
      let env' = push_label env live in
      let live3 = live_expression st env' e3 live in
      untick st;
      let live2 = live_expression st env' e2 live in
      untick st;
      live_expression st env e1 (Code.Var.Set.union live2 live3)

and live_expressions st env l live =
  List.fold_right ~f:(fun e live -> live_expression st env e live) l ~init:live

and live_instruction st env i live =
  untick st;
  let live =
    match i with
    | W.Drop e | GlobalSet (_, e) | Push e -> live_expression st env e live
    | LocalSet (x, e) -> live_expression st env e (Code.Var.Set.remove x live)
    | Br (i, None) -> label env i
    | Br (i, Some e) -> live_expression st env e (label env i)
    | Br_if (i, e) -> live_expression st env e (Code.Var.Set.union live (label env i))
    | Br_table (e, l, i) ->
        live_expression
          st
          env
          e
          (List.fold_left
             ~f:(fun s i -> Code.Var.Set.union s (label env i))
             ~init:Code.Var.Set.empty
             (i :: l))
    | Throw (_, e) -> live_expression st env e Code.Var.Set.empty
    | Return (Some e) -> live_expression st env e Code.Var.Set.empty
    | Return_call (_, l) -> live_expressions st env l Code.Var.Set.empty
    | Return_call_ref (_, e', l) ->
        live_expressions st env l (live_expression st env e' Code.Var.Set.empty)
    | Return None | Rethrow _ | Unreachable -> Code.Var.Set.empty
    | Nop | Event _ -> live
    | Block (_, l) -> live_instructions st (push_label env live) l live
    | Loop (_, l) ->
        (* Iterate to a fixpoint. Positions inside the loop do not
           matter (we never cut inside a loop) but the counter must
           end up at the right place. *)
        let pos = st.pos in
        let rec fixpoint live_in =
          st.pos <- pos;
          let live_in' = live_instructions st (push_label env live_in) l live in
          if Code.Var.Set.subset live_in' live_in
          then live_in'
          else fixpoint (Code.Var.Set.union live_in live_in')
        in
        fixpoint live
    | If (_, e, l1, l2) ->
        let env' = push_label env live in
        let live2 = live_instructions st env' l2 live in
        untick st;
        let live1 = live_instructions st env' l1 live in
        untick st;
        live_expression st env e (Code.Var.Set.union live1 live2)
    | CallInstr (_, l) -> live_expressions st env l live
    | ArraySet (_, e1, e2, e3) ->
        live_expression
          st
          env
          e1
          (live_expression st env e2 (live_expression st env e3 live))
    | StructSet (_, _, e1, e2) ->
        live_expression st env e1 (live_expression st env e2 live)
  in
  untick st;
  Code.Var.Set.union live env.exn_live

and live_instructions st env l live =
  let record live =
    if Code.Var.Set.is_empty live then Int.Hashtbl.replace st.cuts st.pos ()
  in
  record live;
  List.fold_right
    ~f:(fun i live ->
      let live = live_instruction st env i live in
      (* Discard the locals which are not written before this
         position *)
      let live =
        Code.Var.Set.filter
          (fun x ->
            match Code.Var.Hashtbl.find_opt st.first_write x with
            | Some w -> w < st.pos + st.count
            | None -> false)
          live
      in
      record live;
      live)
    l
    ~init:live

(* Returns the number of positions, and an array indicating at which
   positions no local is live. *)
let compute_liveness body =
  let wst = { wpos = 0; first_write = Code.Var.Hashtbl.create 256 } in
  write_instructions wst body;
  let count = wst.wpos in
  let st =
    { pos = 0; cuts = Int.Hashtbl.create 1024; first_write = wst.first_write; count }
  in
  let env = { labels = []; exn_live = Code.Var.Set.empty } in
  ignore (live_instructions st env body Code.Var.Set.empty);
  assert (st.pos = -count);
  let cuts = Array.make (count + 1) false in
  Int.Hashtbl.iter (fun p () -> cuts.(p + count) <- true) st.cuts;
  count, cuts

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

(* Information about a piece of code:
   - its size;
   - the ways it may exit that can be emulated by an outlined
     function returning a code;
   - the enclosing labels (relative depth) targeted by branches
     that cannot be emulated (branches carrying a value, multi-way
     branches, exception catches);
   - whether it may exit in some other way we do not support:
     return of a value other than unit, tail calls, [rethrow], and
     [pop] (which reads an exception value from the stack). *)
type info =
  { weight : int
  ; exits : exit list
  ; blocked : int list
  ; unsupported : bool
  }

let empty = { weight = 0; exits = []; blocked = []; unsupported = false }

let leaf = { empty with weight = 1 }

let unsupported = { leaf with unsupported = true }

let merge i i' =
  { weight = i.weight + i'.weight
  ; exits = union_exits i.exits i'.exits
  ; blocked = List.fold_left ~f:(fun l lbl -> add_label lbl l) ~init:i.blocked i'.blocked
  ; unsupported = i.unsupported || i'.unsupported
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
  | Drop _
  | LocalSet _
  | GlobalSet _
  | Br_table _
  | Br _
  | Br_if _
  | Return _
  | CallInstr _
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

(* Pass 2: rewrite the function body, outlining large runs of
   instructions. The instructions are numbered again, in the same
   way as in pass 1, so that positions match. *)

type state =
  { mutable position : int
  ; cuts : bool array (* positions where no local is live *)
  ; local_types : W.value_type Code.Var.Hashtbl.t
  ; name : Code.Var.t
  ; mutable functions : W.module_field list
  }

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

(* Outline a run of instructions [body] with exits [exits]. Returns
   the instructions replacing it. *)
let outline st body exits =
  let c = { locals = Code.Var.Hashtbl.create 16; exit_codes = Some exits } in
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
  st.functions <-
    W.Function
      { name = f
      ; exported_name = None
      ; typ = None
      ; signature = { params = []; result }
      ; param_names = []
      ; locals = local_declarations st c.locals
      ; body
      }
    :: st.functions;
  let exit_instr e code =
    match e with
    | Br_exit d -> W.Br_if (d, code)
    | Return_exit ->
        If ({ params = []; result = [] }, code, [ Return (Some unit_value) ], [])
  in
  (match exits with
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
             exits)
  @ epilogue

let rec rewrite_expression st ~in_loop e =
  match e with
  | W.Const _ | GlobalGet _ | RefFunc _ | RefNull _ | LocalGet _ -> e, leaf
  | Pop _ -> e, unsupported
  | UnOp (op, e') ->
      let e', i = rewrite_expression st ~in_loop e' in
      UnOp (op, e'), node i
  | I32WrapI64 e' ->
      let e', i = rewrite_expression st ~in_loop e' in
      I32WrapI64 e', node i
  | I64ExtendI32 (s, e') ->
      let e', i = rewrite_expression st ~in_loop e' in
      I64ExtendI32 (s, e'), node i
  | F32DemoteF64 e' ->
      let e', i = rewrite_expression st ~in_loop e' in
      F32DemoteF64 e', node i
  | F64PromoteF32 e' ->
      let e', i = rewrite_expression st ~in_loop e' in
      F64PromoteF32 e', node i
  | LocalTee (x, e') ->
      let e', i = rewrite_expression st ~in_loop e' in
      LocalTee (x, e'), node i
  | RefI31 e' ->
      let e', i = rewrite_expression st ~in_loop e' in
      RefI31 e', node i
  | I31Get (s, e') ->
      let e', i = rewrite_expression st ~in_loop e' in
      I31Get (s, e'), node i
  | ArrayLen e' ->
      let e', i = rewrite_expression st ~in_loop e' in
      ArrayLen e', node i
  | StructGet (s, ty, idx, e') ->
      let e', i = rewrite_expression st ~in_loop e' in
      StructGet (s, ty, idx, e'), node i
  | RefCast (ty, e') ->
      let e', i = rewrite_expression st ~in_loop e' in
      RefCast (ty, e'), node i
  | RefTest (ty, e') ->
      let e', i = rewrite_expression st ~in_loop e' in
      RefTest (ty, e'), node i
  | Br_on_cast (lbl, ty, ty', e') ->
      let e', i = rewrite_expression st ~in_loop e' in
      Br_on_cast (lbl, ty, ty', e'), node { i with blocked = add_label lbl i.blocked }
  | Br_on_cast_fail (lbl, ty, ty', e') ->
      let e', i = rewrite_expression st ~in_loop e' in
      ( Br_on_cast_fail (lbl, ty, ty', e')
      , node { i with blocked = add_label lbl i.blocked } )
  | Br_on_null (lbl, e') ->
      let e', i = rewrite_expression st ~in_loop e' in
      Br_on_null (lbl, e'), node { i with blocked = add_label lbl i.blocked }
  | ExternConvertAny e' ->
      let e', i = rewrite_expression st ~in_loop e' in
      ExternConvertAny e', node i
  | AnyConvertExtern e' ->
      let e', i = rewrite_expression st ~in_loop e' in
      AnyConvertExtern e', node i
  | BinOp (op, e', e'') ->
      let e', i = rewrite_expression st ~in_loop e' in
      let e'', i' = rewrite_expression st ~in_loop e'' in
      BinOp (op, e', e''), node (merge i i')
  | ArrayNew (ty, e', e'') ->
      let e', i = rewrite_expression st ~in_loop e' in
      let e'', i' = rewrite_expression st ~in_loop e'' in
      ArrayNew (ty, e', e''), node (merge i i')
  | ArrayNewData (ty, d, e', e'') ->
      let e', i = rewrite_expression st ~in_loop e' in
      let e'', i' = rewrite_expression st ~in_loop e'' in
      ArrayNewData (ty, d, e', e''), node (merge i i')
  | ArrayGet (s, ty, e', e'') ->
      let e', i = rewrite_expression st ~in_loop e' in
      let e'', i' = rewrite_expression st ~in_loop e'' in
      ArrayGet (s, ty, e', e''), node (merge i i')
  | RefEq (e', e'') ->
      let e', i = rewrite_expression st ~in_loop e' in
      let e'', i' = rewrite_expression st ~in_loop e'' in
      RefEq (e', e''), node (merge i i')
  | Call (f, l) ->
      let l, i = rewrite_expressions st ~in_loop l in
      Call (f, l), node i
  | ArrayNewFixed (ty, l) ->
      let l, i = rewrite_expressions st ~in_loop l in
      ArrayNewFixed (ty, l), node i
  | StructNew (ty, l) ->
      let l, i = rewrite_expressions st ~in_loop l in
      StructNew (ty, l), node i
  | Call_ref (ty, e', l) ->
      let l, i = rewrite_expressions st ~in_loop l in
      let e', i' = rewrite_expression st ~in_loop e' in
      Call_ref (ty, e', l), node (merge i i')
  | BlockExpr (ty, l) ->
      let l, i = rewrite_instructions st ~in_loop l in
      BlockExpr (ty, l), node (shift i)
  | Try (ty, l, catches) ->
      let l, i = rewrite_instructions st ~in_loop l in
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
      Try (ty, l, catches), node i
  | Seq (l, e') ->
      let l, i = rewrite_instructions st ~in_loop l in
      let e', i' = rewrite_expression st ~in_loop e' in
      Seq (l, e'), node (merge i i')
  | IfExpr (ty, e1, e2, e3) ->
      let e1, i1 = rewrite_expression st ~in_loop e1 in
      st.position <- st.position + 1;
      let e2, i2 = rewrite_expression st ~in_loop e2 in
      st.position <- st.position + 1;
      let e3, i3 = rewrite_expression st ~in_loop e3 in
      IfExpr (ty, e1, e2, e3), node (merge i1 (shift (merge i2 i3)))

and rewrite_expressions st ~in_loop l =
  let l, i =
    List.fold_left
      ~f:(fun (acc, i) e ->
        let e, i' = rewrite_expression st ~in_loop e in
        e :: acc, merge i i')
      ~init:([], empty)
      l
  in
  List.rev l, i

and rewrite_instruction st ~in_loop i =
  st.position <- st.position + 1;
  let res =
    match i with
    | W.Drop e ->
        let e, i = rewrite_expression st ~in_loop e in
        W.Drop e, node i
    | LocalSet (x, e) ->
        let e, i = rewrite_expression st ~in_loop e in
        LocalSet (x, e), node i
    | GlobalSet (x, e) ->
        let e, i = rewrite_expression st ~in_loop e in
        GlobalSet (x, e), node i
    | Br (lbl, None) -> i, { leaf with exits = [ Br_exit lbl ] }
    | Br (lbl, Some e) ->
        let e, i = rewrite_expression st ~in_loop e in
        Br (lbl, Some e), node { i with blocked = add_label lbl i.blocked }
    | Br_if (lbl, e) ->
        let e, i = rewrite_expression st ~in_loop e in
        Br_if (lbl, e), node { i with exits = add_exit (Br_exit lbl) i.exits }
    | Br_table (e, l, lbl) ->
        let e, i = rewrite_expression st ~in_loop e in
        ( Br_table (e, l, lbl)
        , node
            { i with
              blocked =
                List.fold_left ~f:(fun l lbl -> add_label lbl l) ~init:i.blocked (lbl :: l)
            } )
    | Throw (t, e) ->
        let e, i = rewrite_expression st ~in_loop e in
        Throw (t, e), node i
    | Return (Some e) when Poly.equal e unit_value ->
        i, { leaf with weight = 2; exits = [ Return_exit ] }
    | Return (Some e) ->
        let e, i = rewrite_expression st ~in_loop e in
        Return (Some e), node { i with unsupported = true }
    | Push e ->
        let e, i = rewrite_expression st ~in_loop e in
        Push e, node i
    | Loop (ty, l) ->
        let l, i = rewrite_instructions st ~in_loop:true l in
        Loop (ty, l), node (shift i)
    | Block (ty, l) ->
        let l, i = rewrite_instructions st ~in_loop l in
        Block (ty, l), node (shift i)
    | If (ty, e, l1, l2) ->
        let e, i = rewrite_expression st ~in_loop e in
        st.position <- st.position + 1;
        let l1, i1 = rewrite_instructions st ~in_loop l1 in
        st.position <- st.position + 1;
        let l2, i2 = rewrite_instructions st ~in_loop l2 in
        If (ty, e, l1, l2), node (merge i (shift (merge i1 i2)))
    | CallInstr (f, l) ->
        let l, i = rewrite_expressions st ~in_loop l in
        CallInstr (f, l), node i
    | Return_call (f, l) ->
        let l, i = rewrite_expressions st ~in_loop l in
        Return_call (f, l), node { i with unsupported = true }
    | Return_call_ref (ty, e', l) ->
        let l, i = rewrite_expressions st ~in_loop l in
        let e', i' = rewrite_expression st ~in_loop e' in
        Return_call_ref (ty, e', l), node { (merge i i') with unsupported = true }
    | ArraySet (ty, e1, e2, e3) ->
        let e1, i1 = rewrite_expression st ~in_loop e1 in
        let e2, i2 = rewrite_expression st ~in_loop e2 in
        let e3, i3 = rewrite_expression st ~in_loop e3 in
        ArraySet (ty, e1, e2, e3), node (merge i1 (merge i2 i3))
    | StructSet (ty, idx, e1, e2) ->
        let e1, i1 = rewrite_expression st ~in_loop e1 in
        let e2, i2 = rewrite_expression st ~in_loop e2 in
        StructSet (ty, idx, e1, e2), node (merge i1 i2)
    | Return None | Rethrow _ -> i, unsupported
    | Nop | Unreachable | Event _ -> i, leaf
  in
  st.position <- st.position + 1;
  res

and rewrite_instructions st ~in_loop l =
  (* Rewrite each instruction (this outlines code nested inside it),
     remembering its position. *)
  let items =
    List.map
      ~f:(fun i ->
        let p = st.position in
        let i, info = rewrite_instruction st ~in_loop i in
        p, i, info)
      l
  in
  let p_end = st.position in
  if in_loop
  then
    let l, i =
      List.fold_left
        ~f:(fun (l, i) (_, instr, info) -> instr :: l, merge i info)
        ~init:([], empty)
        items
    in
    List.rev l, i
  else split st items p_end

(* Group the instructions of a sequence into runs delimited by
   positions where no local is live, and outline the runs which are
   large enough and only contain instructions that can be
   outlined. *)
and split st items p_end =
  (* Size (in AST nodes) above which a run of instructions is
     outlined. *)
  let threshold = Config.Param.toplevel_split_size () in
  let is_cut p = st.cuts.(p) in
  let outlinable i info =
    is_stack_neutral i && List.is_empty info.blocked && not info.unsupported
  in
  (* [acc] contains the output instructions, reversed; [pending] the
     current run, reversed, together with its accumulated info;
     [pending_ok] indicates whether the run started at a cut point
     and only contains outlinable instructions, in which case it is a
     candidate for outlining. *)
  let flush acc pending = List.rev_append (List.rev_map ~f:fst pending) acc in
  let outline_pending acc pending pending_info =
    let l = outline st (List.rev_map ~f:fst pending) pending_info.exits in
    let info = { empty with weight = List.length l + 1; exits = pending_info.exits } in
    List.rev_append l acc, info
  in
  let rec loop items acc acc_info pending pending_info pending_ok =
    match items with
    | [] ->
        if pending_ok && is_cut p_end && pending_info.weight >= threshold
        then
          let acc, info = outline_pending acc pending pending_info in
          List.rev acc, merge acc_info info
        else List.rev (flush acc pending), merge acc_info pending_info
    | (p, i, info) :: rem ->
        let acc, acc_info, pending, pending_info, pending_ok =
          if is_cut p
          then
            if pending_ok && pending_info.weight >= threshold
            then
              let acc, info = outline_pending acc pending pending_info in
              acc, merge acc_info info, [], empty, true
            else if not pending_ok
            then flush acc pending, merge acc_info pending_info, [], empty, true
            else acc, acc_info, pending, pending_info, true
          else acc, acc_info, pending, pending_info, pending_ok
        in
        if outlinable i info
        then
          loop
            rem
            acc
            acc_info
            ((i, info) :: pending)
            (merge pending_info info)
            pending_ok
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
  in
  (* The run can only start at a cut point; the first instruction is
     handled like any other one. *)
  loop items [] empty [] empty false

(****)

let f ~name ~locals body =
  let count, cuts = compute_liveness body in
  let local_types = Code.Var.Hashtbl.create 256 in
  List.iter ~f:(fun (x, ty) -> Code.Var.Hashtbl.add local_types x ty) locals;
  let st = { position = 0; cuts; local_types; name; functions = [] } in
  let body, info = rewrite_instructions st ~in_loop:false body in
  assert (st.position = count);
  if debug ()
  then
    Format.eprintf
      "Toplevel function split into %d pieces (residual size: %d)@."
      (List.length st.functions)
      info.weight;
  (* Only keep the locals still used in the residual function. *)
  let c = { locals = Code.Var.Hashtbl.create 256; exit_codes = None } in
  let body = collect_instructions c 0 body in
  local_declarations st c.locals, body, List.rev st.functions
