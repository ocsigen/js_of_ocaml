(* Js_of_ocaml compiler
 * http://www.ocsigen.org/js_of_ocaml/
 * Copyright (C) 2010 Jérôme Vouillon
 * Laboratoire PPS - CNRS Université Paris Diderot
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

(*XXX FIX: avoid the need of global datastructures for analyse_blocks *)

open Code
open Instr

let debug = Util.debug "parser"

(****)

let blocks = ref AddrSet.empty

let add_jump info pc = blocks := AddrSet.add pc !blocks

let rec scan info code pc len =
  if pc < len then begin
    match (get_instr code pc).kind with
      KNullary ->
        scan info code (pc + 1) len
    | KUnary ->
        scan info code (pc + 2) len
    | KBinary ->
        scan info code (pc + 3) len
    | KJump ->
        let offset = gets code (pc + 1) in
        add_jump info (pc + offset + 1);
        scan info code (pc + 2) len
    | KCond_jump ->
        let offset = gets code (pc + 1) in
        add_jump info (pc + offset + 1);
        scan info code (pc + 2) len
    | KCmp_jump ->
        let offset = gets code (pc + 2) in
        add_jump info (pc + offset + 2);
        scan info code (pc + 3) len
    | KSwitch ->
        let sz = getu code (pc + 1) in
        for i = 0 to sz land 0xffff + sz lsr 16 - 1 do
          let offset = gets code (pc + 2 + i) in
          add_jump info (pc + offset + 2)
        done;
        scan info code (pc + 2 + sz land 0xffff + sz lsr 16) len
    | KClosurerec ->
        let nfuncs = getu code (pc + 1) in
        scan info code (pc + nfuncs + 3) len
    | KClosure ->
        scan info code (pc + 3) len
    | KStop n ->
        scan info code (pc + n + 1) len
  end

let rec next_block len pc =
  let pc = pc + 1 in
  if pc = len || AddrSet.mem pc !blocks then pc else next_block len pc

let analyse_blocks code =
  blocks := AddrSet.empty;
  let len = String.length code  / 4 in
  scan () code 0 len

(****)

let same_custom x y =
  Obj.field x 0 == Obj.field (Obj.repr y) 0

let rec parse_const x =
  if Obj.is_block x then begin
    let tag = Obj.tag x in
    if tag = Obj.string_tag then
      String (Obj.magic x : string)
    else if tag = Obj.double_tag then
      Float (Obj.magic x : float)
    else if tag = Obj.double_array_tag then
      Float_array (Obj.magic x : float array)
    else if tag = Obj.custom_tag && same_custom x 0l then
      Int32 (Obj.magic x : int32)
    else if tag = Obj.custom_tag && same_custom x 0n then
      Nativeint (Obj.magic x : nativeint)
    else if tag = Obj.custom_tag && same_custom x 0L then
      Int64 (Obj.magic x : int64)
    else if tag < Obj.no_scan_tag then
      Tuple (tag,
             Array.init (Obj.size x) (fun i -> parse_const (Obj.field x i)))
    else
      assert false
  end else
    Int (Obj.magic x : int)

let inlined_const x =
  not (Obj.is_block x)
    ||
  (let tag = Obj.tag x in
   (tag = Obj.double_tag)
      ||
   (tag = Obj.custom_tag && (same_custom x 0l || same_custom x 0n)))

(****)

module Ident = struct
  type t = { stamp: int; name: string; mutable flags: int }
  type 'a tbl =
      Empty
    | Node of 'a tbl * 'a data * 'a tbl * int
  and 'a data =
    { ident: t;
      data: 'a;
      previous: 'a data option }

  let rec table_contents_rec sz t rem =
    match t with
      Empty ->
        rem
    | Node (l, v, r, _) ->
        table_contents_rec sz l
          ((sz - v.data, v.ident.name) :: table_contents_rec sz r rem)

  let table_contents sz t =
    List.sort (fun (i, _) (j, _) -> compare i j)
      (table_contents_rec sz t [])

end

module Tbl = struct
  type ('a, 'b) t =
      Empty
    | Node of ('a, 'b) t * 'a * 'b * ('a, 'b) t * int

  let rec iter f = function
      Empty -> ()
    | Node(l, v, d, r, _) ->
        iter f l; f v d; iter f r
end

type 'a numtable =
  { num_cnt: int;
    num_tbl: ('a, int) Tbl.t }

module Debug = struct

  type compilation_env =
    { ce_stack: int Ident.tbl; (* Positions of variables in the stack *)
      ce_heap: int Ident.tbl;  (* Structure of the heap-allocated env *)
      ce_rec: int Ident.tbl }  (* Functions bound by the same let rec *)

  type debug_event =
    { mutable ev_pos: int;                (* Position in bytecode *)
      ev_module: string;                  (* Name of defining module *)
      ev_loc: unit;                       (* Location in source file *)
      ev_kind: unit;                      (* Before/after event *)
      ev_info: unit;                      (* Extra information *)
      ev_typenv: unit;                    (* Typing environment *)
      ev_typsubst: unit;                  (* Substitution over types *)
      ev_compenv: compilation_env;        (* Compilation environment *)
      ev_stacksize: int;                  (* Size of stack frame *)
      ev_repr: unit }                     (* Position of the representative *)

  let relocate_event orig ev = ev.ev_pos <- (orig + ev.ev_pos) / 4

  let events_by_pc =
    (Hashtbl.create 257 : (int, debug_event) Hashtbl.t)

  let read ic =
    let len = input_binary_int ic in
    for i = 0 to len - 1 do
      let orig = input_binary_int ic in
      let evl : debug_event list = input_value ic in
      List.iter
        (fun ev ->
           relocate_event orig ev; Hashtbl.add events_by_pc ev.ev_pos ev)
        evl
    done

  let find pc =
    try
      let ev = Hashtbl.find events_by_pc pc in
      Ident.table_contents ev.ev_stacksize ev.ev_compenv.ce_stack
    with Not_found ->
      []

  let rec propagate l1 l2 =
    match l1, l2 with
      v1 :: r1, v2 :: r2 -> Var.propagate_name v1 v2; propagate r1 r2
    | _                  -> ()

end

let keep_variable_names = ref false

let set_pretty () = keep_variable_names := true; Code.Var.set_pretty ()

(****)

type globals =
  { mutable vars : Var.t option array;
    mutable is_const : bool array;
    mutable is_exported : bool array;
    constants : Obj.t array;
    primitives : string array }

let make_globals size constants primitives =
  { vars = Array.create size None;
    is_const = Array.create size false;
    is_exported = Array.create size false;
    constants = constants; primitives = primitives }

let resize_array a len def =
  let b = Array.make len def in
  Array.blit a 0 b 0 (Array.length a);
  b

let resize_globals g size =
  g.vars <- resize_array g.vars size None;
  g.is_const <- resize_array g.is_const size false;
  g.is_exported <- resize_array g.is_exported size false

module State = struct

  type elt = Var of Var.t | Dummy

  let elt_to_var e = match e with Var x -> x | _ -> assert false
  let opt_elt_to_var e = match e with Var x -> Some x | _ -> None

  let print_elt f v =
    match v with
    | Var x   -> Format.fprintf f "%a" Var.print x
(*    | Addr x  -> Format.fprintf f "[%d]" x*)
    | Dummy   -> Format.fprintf f "???"

  type t =
    { accu : elt; stack : elt list; env : elt array; env_offset : int;
      handlers : (Var.t * addr * int) list;
      var_stream : Var.stream; globals : globals }

  let fresh_var state =
    let (x, stream) = Var.next state.var_stream in
    (x, {state with var_stream = stream; accu = Var x})

  let globals st = st.globals

  let size_globals st size =
    if size > Array.length st.globals.vars then resize_globals st.globals size

  let rec list_start n l =
    if n = 0 then [] else
    match l with
      []     -> assert false
    | v :: r -> v :: list_start (n - 1) r

  let rec st_pop n st =
    if n = 0 then
      st
    else
      match st with
        []     -> assert false
      | v :: r -> st_pop (n - 1) r

  let push st = {st with stack = st.accu :: st.stack}

  let pop n st = {st with stack = st_pop n st.stack}

  let acc n st = {st with accu = List.nth st.stack n}

  let env_acc n st = {st with accu = st.env.(st.env_offset + n)}

  let has_accu st = st.accu <> Dummy

  let accu st = elt_to_var st.accu

  let opt_accu st = opt_elt_to_var st.accu

  let stack_vars st =
    List.fold_left
      (fun l e -> match e with Var x -> x :: l | Dummy -> l)
      [] (st.accu :: st.stack)

  let set_accu st x = {st with accu = Var x}

  let clear_accu st = {st with accu = Dummy}

  let peek n st = elt_to_var (List.nth st.stack n)

  let grab n st = (List.map elt_to_var (list_start n st.stack), pop n st)

  let rec st_unalias s x n y =
    match s with
      [] ->
        []
    | z :: rem ->
        (if n <> 0 && z = Var x then Var y else z) ::
        st_unalias rem x (n - 1) y

  let unalias st x n y =
    assert (List.nth st.stack n = Var x);
    {st with stack = st_unalias st.stack x n y }

  let rec st_assign s n x =
    match s with
      [] ->
        assert false
    | y :: rem ->
        if n = 0 then x :: rem else y :: st_assign rem (n - 1) x

  let assign st n =
    {st with stack = st_assign st.stack n st.accu }

  let start_function state env offset =
    {state with accu = Dummy; stack = []; env = env; env_offset = offset;
                handlers = []}

  let start_block state =
    let (stack, stream) =
      List.fold_right
        (fun e (stack, stream) ->
           match e with
             Dummy ->
               (Dummy :: stack, stream)
           | Var _ ->
               let (x, stream) = Var.next stream in
               (Var x :: stack, stream))
        state.stack ([], state.var_stream)
    in
    let state = { state with stack = stack; var_stream = stream } in
    match state.accu with
      Dummy -> state
    | Var _ -> snd (fresh_var state)

  let push_handler state x addr =
    { state
      with handlers = (x, addr, List.length state.stack) :: state.handlers }

  let pop_handler state =
    { state with handlers = List.tl state.handlers }

  let current_handler state =
    match state.handlers with
      [] ->
        None
    | (x, addr, len) :: _ ->
        let state =
          { state
            with accu = Var x;
                 stack = st_pop (List.length state.stack - len) state.stack}
        in
        Some (x, (addr, stack_vars state))

  let initial g =
    { accu = Dummy; stack = []; env = [||]; env_offset = 0; handlers = [];
      var_stream = Var.make_stream (); globals = g }

  let rec print_stack f l =
    match l with
      [] -> ()
    | v :: r -> Format.fprintf f "%a %a" print_elt v print_stack r

  let print_env f e =
    Array.iteri
      (fun i v ->
         if i > 0 then Format.fprintf f " ";
         Format.fprintf f "%a" print_elt v) e

  let print st =
    Format.eprintf "{ %a | %a | (%d) %a }@."
      print_elt st.accu print_stack st.stack st.env_offset print_env st.env

  let rec name_rec i l s =
    match l, s with
      [], _ ->
        ()
    | (j, nm) :: lrem, Var v :: srem when i = j ->
        Var.name v nm; name_rec (i + 1) lrem srem
    | (j, _) :: _, _ :: srem when i < j ->
        name_rec (i + 1) l srem
    | _ ->
        assert false

  let name_vars st l = name_rec 0 l st.stack
end

let primitive_name state i =
  let g = State.globals state in
  assert (i >= 0 && i <= Array.length g.primitives);
  g.primitives.(i)

let access_global g i =
  match g.vars.(i) with
    Some x ->
      x
  | None ->
      g.is_const.(i) <- true;
      let x = Var.fresh () in
      g.vars.(i) <- Some x;
      x

let get_global state instrs i =
  State.size_globals state (i + 1);
  let g = State.globals state in
  match g.vars.(i) with
    Some x ->
      if debug () then Format.printf "(global access %a)@." Var.print x;
      (x, State.set_accu state x, instrs)
  | None ->
      if
        i < Array.length g.constants && inlined_const g.constants.(i)
      then begin
        let (x, state) = State.fresh_var state in
        (x, state, Let (x, Constant (parse_const g.constants.(i))) :: instrs)
      end else begin
        g.is_const.(i) <- true;
        let (x, state) = State.fresh_var state in
          if debug () then Format.printf "%a = CONST(%d)@." Var.print x i;
          g.vars.(i) <- Some x;
          (x, state, instrs)
      end

let compiled_blocks = ref AddrMap.empty

let rec compile_block code pc state =
  if not (AddrMap.mem pc !compiled_blocks) then begin
    let len = String.length code  / 4 in
    let limit = next_block len pc in
    if debug () then Format.eprintf "Compiling from %d to %d@." pc (limit - 1);
    let state = State.start_block state in
    let (instr, last, state') = compile code limit pc state [] in
    compiled_blocks :=
      AddrMap.add pc (state, List.rev instr, last) !compiled_blocks;
    begin match last with
      Branch (pc', _) | Poptrap (pc', _) ->
        compile_block code pc' state'
    | Cond (_, _, (pc1, _), (pc2, _)) ->
        compile_block code pc1 state';
        compile_block code pc2 state'
    | Switch (_, l1, l2) ->
        Array.iter (fun (pc', _) -> compile_block code pc' state') l1;
        Array.iter (fun (pc', _) -> compile_block code pc' state') l2
    | Pushtrap _ | Raise _ | Return _ | Stop ->
        ()
    end
  end

and compile code limit pc state instrs =
  if debug () then State.print state;
  if pc = limit then
    (instrs, Branch (pc, State.stack_vars state), state)
  else begin
  if debug () then Format.eprintf "%4d " pc;

  State.name_vars state (Debug.find pc);

  let instr =
    try
      get_instr code pc
    with Bad_instruction op ->
      if debug () then Format.eprintf "%08x@." op;
      assert false
  in
  if debug () then Format.eprintf "%08x %s@." instr.opcode instr.name;
  match instr.code with
  | ACC0 ->
      compile code limit (pc + 1) (State.acc 0 state) instrs
  | ACC1 ->
      compile code limit (pc + 1) (State.acc 1 state) instrs
  | ACC2 ->
      compile code limit (pc + 1) (State.acc 2 state) instrs
  | ACC3 ->
      compile code limit (pc + 1) (State.acc 3 state) instrs
  | ACC4 ->
      compile code limit (pc + 1) (State.acc 4 state) instrs
  | ACC5 ->
      compile code limit (pc + 1) (State.acc 5 state) instrs
  | ACC6 ->
      compile code limit (pc + 1) (State.acc 6 state) instrs
  | ACC7 ->
      compile code limit (pc + 1) (State.acc 7 state) instrs
  | ACC ->
      let n = getu code (pc + 1) in
      compile code limit (pc + 2) (State.acc n state) instrs
  | PUSH ->
      compile code limit (pc + 1) (State.push state) instrs
  | PUSHACC0 ->
      compile code limit (pc + 1) (State.acc 0 (State.push state)) instrs
  | PUSHACC1 ->
      compile code limit (pc + 1) (State.acc 1 (State.push state)) instrs
  | PUSHACC2 ->
      compile code limit (pc + 1) (State.acc 2 (State.push state)) instrs
  | PUSHACC3 ->
      compile code limit (pc + 1) (State.acc 3 (State.push state)) instrs
  | PUSHACC4 ->
      compile code limit (pc + 1) (State.acc 4 (State.push state)) instrs
  | PUSHACC5 ->
      compile code limit (pc + 1) (State.acc 5 (State.push state)) instrs
  | PUSHACC6 ->
      compile code limit (pc + 1) (State.acc 6 (State.push state)) instrs
  | PUSHACC7 ->
      compile code limit (pc + 1) (State.acc 7 (State.push state)) instrs
  | PUSHACC ->
      let n = getu code (pc + 1) in
      compile code limit (pc + 2) (State.acc n (State.push state)) instrs
  | POP ->
      let n = getu code (pc + 1) in
      compile code limit (pc + 2) (State.pop n state) instrs
  | ASSIGN ->
      let n = getu code (pc + 1) in
      let state = State.assign state n in
      let (x, state) = State.fresh_var state in
      if debug () then Format.printf "%a = 0@." Var.print x;
      (* We switch to a different block as this may have
         changed the exception handler continuation *)
      compile_block code (pc + 2) state;
      (Let (x, Const 0) :: instrs,
       Branch (pc + 2, State.stack_vars state),
       state)
  | ENVACC1 ->
      compile code limit (pc + 1) (State.env_acc 1 state) instrs
  | ENVACC2 ->
      compile code limit (pc + 1) (State.env_acc 2 state) instrs
  | ENVACC3 ->
      compile code limit (pc + 1) (State.env_acc 3 state) instrs
  | ENVACC4 ->
      compile code limit (pc + 1) (State.env_acc 4 state) instrs
  | ENVACC ->
      let n = getu code (pc + 1) in
      compile code limit (pc + 2) (State.env_acc n state) instrs
  | PUSHENVACC1 ->
      compile code limit (pc + 1) (State.env_acc 1 (State.push state)) instrs
  | PUSHENVACC2 ->
      compile code limit (pc + 1) (State.env_acc 2 (State.push state)) instrs
  | PUSHENVACC3 ->
      compile code limit (pc + 1) (State.env_acc 3 (State.push state)) instrs
  | PUSHENVACC4 ->
      compile code limit (pc + 1) (State.env_acc 4 (State.push state)) instrs
  | PUSHENVACC ->
      let n = getu code (pc + 1) in
      compile code limit (pc + 2) (State.env_acc n (State.push state)) instrs
  | PUSH_RETADDR ->
      compile code limit (pc + 2)
        {state with State.stack =
            State.Dummy :: State.Dummy :: State.Dummy :: state.State.stack}
        instrs
  | APPLY ->
      let n = getu code (pc + 1) in
      let f = State.accu state in
      let (x, state) = State.fresh_var state in
      let (args, state) = State.grab n state in
      if debug () then begin
        Format.printf "%a = %a(" Var.print x Var.print f;
        for i = 0 to n - 1 do
          if i > 0 then Format.printf ", ";
          Format.printf "%a" Var.print (List.nth args i)
        done;
        Format.printf ")@."
      end;
      compile code limit (pc + 2) (State.pop 3 state)
        (Let (x, Apply (f, args, None)) :: instrs)
  | APPLY1 ->
      let f = State.accu state in
      let (x, state) = State.fresh_var state in
      let y = State.peek 0 state in
      if debug () then
        Format.printf "%a = %a(%a)@." Var.print x
          Var.print f Var.print y;
      compile code limit (pc + 1) (State.pop 1 state)
        (Let (x, Apply (f, [y], None)) :: instrs)
  | APPLY2 ->
      let f = State.accu state in
      let (x, state) = State.fresh_var state in
      let y = State.peek 0 state in
      let z = State.peek 1 state in
      if debug () then Format.printf "%a = %a(%a, %a)@." Var.print x
        Var.print f Var.print y Var.print z;
      compile code limit (pc + 1) (State.pop 2 state)
        (Let (x, Apply (f, [y; z], None)) :: instrs)
  | APPLY3 ->
      let f = State.accu state in
      let (x, state) = State.fresh_var state in
      let y = State.peek 0 state in
      let z = State.peek 1 state in
      let t = State.peek 2 state in
      if debug () then Format.printf "%a = %a(%a, %a, %a)@." Var.print x
        Var.print f Var.print y Var.print z Var.print t;
      compile code limit (pc + 1) (State.pop 3 state)
        (Let (x, Apply (f, [y; z; t], None)) :: instrs)
  | APPTERM ->
      let n = getu code (pc + 1) in
      let f = State.accu state in
      let (l, state) = State.grab n state in
      if debug () then begin
        Format.printf "return %a(" Var.print f;
        for i = 0 to n - 1 do
          if i > 0 then Format.printf ", ";
          Format.printf "%a" Var.print (List.nth l i)
        done;
        Format.printf ")@."
      end;
      let (x, state) = State.fresh_var state in
      (Let (x, Apply (f, l, None)) :: instrs, Return x, state)
  | APPTERM1 ->
      let f = State.accu state in
      let x = State.peek 0 state in
      if debug () then Format.printf "return %a(%a)@." Var.print f Var.print x;
      let (y, state) = State.fresh_var state in
      (Let (y, Apply (f, [x], None)) :: instrs, Return y, state)
  | APPTERM2 ->
      let f = State.accu state in
      let x = State.peek 0 state in
      let y = State.peek 1 state in
      if debug () then Format.printf "return %a(%a, %a)@."
        Var.print f Var.print x Var.print y;
      let (z, state) = State.fresh_var state in
      (Let (z, Apply (f, [x; y], None)) :: instrs, Return z, state)
  | APPTERM3 ->
      let f = State.accu state in
      let x = State.peek 0 state in
      let y = State.peek 1 state in
      let z = State.peek 2 state in
      if debug () then Format.printf "return %a(%a, %a, %a)@."
        Var.print f Var.print x Var.print y Var.print z;
      let (t, state) = State.fresh_var state in
      (Let (t, Apply (f, [x; y; z], None)) :: instrs, Return t, state)
  | RETURN ->
      let x = State.accu state in
      if debug () then Format.printf "return %a@." Var.print x;
      (instrs, Return x, state)
  | RESTART ->
      assert false
  | GRAB ->
      compile code limit (pc + 2) state instrs
  | CLOSURE ->
      let nvars = getu code (pc + 1) in
      let addr = pc + gets code (pc + 2) + 2 in
      let state = if nvars > 0 then State.push state else state in
      let (vals, state) = State.grab nvars state in
      let (x, state) = State.fresh_var state in
      let env =
        Array.of_list (State.Dummy :: List.map (fun x -> State.Var x) vals) in
      if debug () then Format.printf "fun %a (" Var.print x;
      let nparams =
        match (get_instr code addr).code with
          GRAB -> getu code (addr + 1) + 1
        | _    -> 1
      in
      let state' = State.start_function state env 0 in
      let rec make_stack i state =
        if i = nparams then ([], state) else begin
          let (x, state) = State.fresh_var state in
          let (params, state) =
            make_stack (i + 1) (State.push state) in
          if debug () then if i < nparams - 1 then Format.printf ", ";
          if debug () then Format.printf "%a" Var.print x;
          (x :: params, state)
        end
      in
      let (params, state') = make_stack 0 state' in
      if debug () then Format.printf ") {@.";
      let state' = State.clear_accu state' in
      compile_block code addr state';
      if debug () then Format.printf "}@.";
      let args = State.stack_vars state' in

      let (state'', _, _) = AddrMap.find addr !compiled_blocks in
      Debug.propagate (State.stack_vars state'') args;

      compile code limit (pc + 3) state
        (Let (x, Closure (List.rev params, (addr, args))) :: instrs)
  | CLOSUREREC ->
      let nfuncs = getu code (pc + 1) in
      let nvars = getu code (pc + 2) in
      let state = if nvars > 0 then (State.push state) else state in
      let (vals, state) = State.grab nvars state in
      let state = ref state in
      let vars = ref [] in
      for i = 0 to nfuncs - 1 do
        let (x, st) = State.fresh_var !state in
        vars := (i, x) :: !vars;
        state := State.push st
      done;
      let env = ref (List.map (fun x -> State.Var x) vals) in
      List.iter
        (fun (i, x) ->
           env := State.Var x :: !env;
           if i > 0 then env := State.Dummy :: !env)
        !vars;
      let env = Array.of_list !env in
      let state = !state in
      let instrs =
        List.fold_left
          (fun instr (i, x) ->
             let addr = pc + 3 + gets code (pc + 3 + i) in
             if debug () then Format.printf "fun %a (" Var.print x;
             let nparams =
               match (get_instr code addr).code with
                 GRAB -> getu code (addr + 1) + 1
               | _    -> 1
             in
             let state' = State.start_function state env (i * 2) in
             let rec make_stack i state =
               if i = nparams then ([], state) else begin
                 let (x, state) = State.fresh_var state in
                 let (params, state) =
                   make_stack (i + 1) (State.push state) in
                 if debug () then if i < nparams - 1 then Format.printf ", ";
                 if debug () then Format.printf "%a" Var.print x;
                 (x :: params, state)
               end
             in
             let (params, state') = make_stack 0 state' in
             if debug () then Format.printf ") {@.";
             let state' = State.clear_accu state' in
             compile_block code addr state';
             if debug () then Format.printf "}@.";
             let args = State.stack_vars state' in

             let (state'', _, _) = AddrMap.find addr !compiled_blocks in
             Debug.propagate (State.stack_vars state'') args;

             Let (x, Closure (List.rev params, (addr, args))) :: instr)
         instrs (List.rev !vars)
      in
      compile
        code limit (pc + 3 + nfuncs) (State.acc (nfuncs - 1) state) instrs
  | OFFSETCLOSUREM2 ->
      compile code limit (pc + 1) (State.env_acc (-2) state) instrs
  | OFFSETCLOSURE0 ->
      compile code limit (pc + 1) (State.env_acc 0 state) instrs
  | OFFSETCLOSURE2 ->
      compile code limit (pc + 1) (State.env_acc 2 state) instrs
  | OFFSETCLOSURE ->
      let n = gets code (pc + 1) in
      compile code limit (pc + 2) (State.env_acc n state) instrs
  | PUSHOFFSETCLOSUREM2 ->
      let state = State.push state in
      compile code limit (pc + 1) (State.env_acc (-2) state) instrs
  | PUSHOFFSETCLOSURE0 ->
      let state = State.push state in
      compile code limit (pc + 1) (State.env_acc 0 state) instrs
  | PUSHOFFSETCLOSURE2 ->
      let state = State.push state in
      compile code limit (pc + 1) (State.env_acc 2 state) instrs
  | PUSHOFFSETCLOSURE ->
      let state = State.push state in
      let n = gets code (pc + 1) in
      compile code limit (pc + 2) (State.env_acc n state) instrs
  | GETGLOBAL ->
      let i = getu code (pc + 1) in
      let (_, state, instrs) = get_global state instrs i in
      compile code limit (pc + 2) state instrs
  | PUSHGETGLOBAL ->
      let state = State.push state in
      let i = getu code (pc + 1) in
      let (_, state, instrs) = get_global state instrs i in
      compile code limit (pc + 2) state instrs
  | GETGLOBALFIELD ->
      let i = getu code (pc + 1) in
      let (x, state, instrs) = get_global state instrs i in
      let j = getu code (pc + 2) in
      let (y, state) = State.fresh_var state in
      if debug () then Format.printf "%a = %a[%d]@." Var.print y Var.print x j;
      compile code limit (pc + 3) state (Let (y, Field (x, j)) :: instrs)
  | PUSHGETGLOBALFIELD ->
      let state = State.push state in
      let i = getu code (pc + 1) in
      let (x, state, instrs) = get_global state instrs i in
      let j = getu code (pc + 2) in
      let (y, state) = State.fresh_var state in
      if debug () then Format.printf "%a = %a[%d]@." Var.print y Var.print x j;
      compile code limit (pc + 3) state (Let (y, Field (x, j)) :: instrs)
  | SETGLOBAL ->
      let i = getu code (pc + 1) in
      let y = State.accu state in
      let g = State.globals state in
      assert (g.vars.(i) = None);
      if debug () then Format.printf "(global %d) = %a@." i Var.print y;
      g.vars.(i) <- Some y;
      let (x, state) = State.fresh_var state in
      if debug () then Format.printf "%a = 0@." Var.print x;
      let instrs =
        if g.is_exported.(i) then begin
          let x = Var.fresh () in
          Let (Var.fresh (),
               Prim (Extern "caml_register_global",
                     [Pv x ; Pv (access_global g i)])) ::
          Let (x, Const i) ::
          instrs
        end else
          instrs
      in
      compile code limit (pc + 2) state (Let (x, Const 0) :: instrs)
  | ATOM0 ->
      let (x, state) = State.fresh_var state in
      if debug () then Format.printf "%a = ATOM(0)@." Var.print x;
      compile code limit (pc + 1) state (Let (x, Block (0, [||])) :: instrs)
  | ATOM ->
      let i = getu code (pc + 1) in
      let (x, state) = State.fresh_var state in
      if debug () then Format.printf "%a = ATOM(%d)@." Var.print x i;
      compile code limit (pc + 2) state (Let (x, Block (i, [||])) :: instrs)
  | PUSHATOM0 ->
      let state = State.push state in
      let (x, state) = State.fresh_var state in
      if debug () then Format.printf "%a = ATOM(0)@." Var.print x;
      compile code limit (pc + 1) state (Let (x, Block (0, [||])) :: instrs)
  | PUSHATOM ->
      let state = State.push state in
      let i = getu code (pc + 1) in
      let (x, state) = State.fresh_var state in
      if debug () then Format.printf "%a = ATOM(%d)@." Var.print x i;
      compile code limit (pc + 2) state (Let (x, Block (i, [||])) :: instrs)
  | MAKEBLOCK ->
      let size = getu code (pc + 1) in
      let tag = getu code (pc + 2) in
      let state = State.push state in
      let (x, state) = State.fresh_var state in
      let (contents, state) = State.grab size state in
      if debug () then begin
        Format.printf "%a = { " Var.print x;
        for i = 0 to size - 1 do
          Format.printf "%d = %a; " i Var.print (List.nth contents i);
        done;
        Format.printf "}@."
      end;
      compile code limit (pc + 3) state
        (Let (x, Block (tag, Array.of_list contents)) :: instrs)
  | MAKEBLOCK1 ->
      let tag = getu code (pc + 1) in
      let y = State.accu state in
      let (x, state) = State.fresh_var state in
      if debug () then Format.printf "%a = { 0 = %a; }@." Var.print x Var.print y;
      compile code limit (pc + 2) state (Let (x, Block (tag, [|y|])) :: instrs)
  | MAKEBLOCK2 ->
      let tag = getu code (pc + 1) in
      let y = State.accu state in
      let z = State.peek 0 state in
      let (x, state) = State.fresh_var state in
      if debug () then Format.printf "%a = { 0 = %a; 1 = %a; }@."
        Var.print x Var.print y Var.print z;
      compile code limit (pc + 2) (State.pop 1 state)
        (Let (x, Block (tag, [|y; z|])) :: instrs)
  | MAKEBLOCK3 ->
      let tag = getu code (pc + 1) in
      let y = State.accu state in
      let z = State.peek 0 state in
      let t = State.peek 1 state in
      let (x, state) = State.fresh_var state in
      if debug () then Format.printf "%a = { 0 = %a; 1 = %a; 2 = %a }@."
        Var.print x Var.print y Var.print z Var.print t;
      compile code limit (pc + 2) (State.pop 2 state)
        (Let (x, Block (tag, [|y; z; t|])) :: instrs)
  | MAKEFLOATBLOCK ->
      let size = getu code (pc + 1) in
      let state = State.push state in
      let (x, state) = State.fresh_var state in
      let (contents, state) = State.grab size state in
      if debug () then begin
        Format.printf "%a = { " Var.print x;
        for i = 0 to size - 1 do
          Format.printf "%d = %a; " i Var.print (List.nth contents i);
        done;
        Format.printf "}@."
      end;
      compile code limit (pc + 2) state
        (Let (x, Block (-1, Array.of_list contents)) :: instrs)
  | GETFIELD0 ->
      let y = State.accu state in
      let (x, state) = State.fresh_var state in
      if debug () then Format.printf "%a = %a[0]@." Var.print x Var.print y;
      compile code limit (pc + 1) state (Let (x, Field (y, 0)) :: instrs)
  | GETFIELD1 ->
      let y = State.accu state in
      let (x, state) = State.fresh_var state in
      if debug () then Format.printf "%a = %a[1]@." Var.print x Var.print y;
      compile code limit (pc + 1) state (Let (x, Field (y, 1)) :: instrs)
  | GETFIELD2 ->
      let y = State.accu state in
      let (x, state) = State.fresh_var state in
      if debug () then Format.printf "%a = %a[2]@." Var.print x Var.print y;
      compile code limit (pc + 1) state (Let (x, Field (y, 2)) :: instrs)
  | GETFIELD3 ->
      let y = State.accu state in
      let (x, state) = State.fresh_var state in
      if debug () then Format.printf "%a = %a[3]@." Var.print x Var.print y;
      compile code limit (pc + 1) state (Let (x, Field (y, 3)) :: instrs)
  | GETFIELD ->
      let y = State.accu state in
      let n = getu code (pc + 1) in
      let (x, state) = State.fresh_var state in
      if debug () then Format.printf "%a = %a[%d]@." Var.print x Var.print y n;
      compile code limit (pc + 2) state (Let (x, Field (y, n)) :: instrs)
  | GETFLOATFIELD ->
      let y = State.accu state in
      let n = getu code (pc + 1) in
      let (x, state) = State.fresh_var state in
      if debug () then Format.printf "%a = %a[%d]@." Var.print x Var.print y n;
      compile code limit (pc + 2) state (Let (x, Field (y, n)) :: instrs)
  | SETFIELD0 ->
      let y = State.accu state in
      let z = State.peek 0 state in
      if debug () then Format.printf "%a[0] = %a@." Var.print y Var.print z;
      let (x, state) = State.fresh_var state in
      if debug () then Format.printf "%a = 0@." Var.print x;
      compile code limit (pc + 1) (State.pop 1 state)
        (Let (x, Const 0) :: Set_field (y, 0, z) :: instrs)
  | SETFIELD1 ->
      let y = State.accu state in
      let z = State.peek 0 state in
      if debug () then Format.printf "%a[1] = %a@." Var.print y Var.print z;
      let (x, state) = State.fresh_var state in
      if debug () then Format.printf "%a = 0@." Var.print x;
      compile code limit (pc + 1) (State.pop 1 state)
        (Let (x, Const 0) :: Set_field (y, 1, z) :: instrs)
  | SETFIELD2 ->
      let y = State.accu state in
      let z = State.peek 0 state in
      if debug () then Format.printf "%a[2] = %a@." Var.print y Var.print z;
      let (x, state) = State.fresh_var state in
      if debug () then Format.printf "%a = 0@." Var.print x;
      compile code limit (pc + 1) (State.pop 1 state)
        (Let (x, Const 0) :: Set_field (y, 2, z) :: instrs)
  | SETFIELD3 ->
      let y = State.accu state in
      let z = State.peek 0 state in
      if debug () then Format.printf "%a[3] = %a@." Var.print y Var.print z;
      let (x, state) = State.fresh_var state in
      if debug () then Format.printf "%a = 0@." Var.print x;
      compile code limit (pc + 1) (State.pop 1 state)
        (Let (x, Const 0) :: Set_field (y, 3, z) :: instrs)
  | SETFIELD ->
      let y = State.accu state in
      let z = State.peek 0 state in
      let n = getu code (pc + 1) in
      if debug () then Format.printf "%a[%d] = %a@." Var.print y n Var.print z;
      let (x, state) = State.fresh_var state in
      if debug () then Format.printf "%a = 0@." Var.print x;
      compile code limit (pc + 2) (State.pop 1 state)
        (Let (x, Const 0) :: Set_field (y, n, z) :: instrs)
  | SETFLOATFIELD ->
      let y = State.accu state in
      let z = State.peek 0 state in
      let n = getu code (pc + 1) in
      if debug () then Format.printf "%a[%d] = %a@." Var.print y n Var.print z;
      let (x, state) = State.fresh_var state in
      if debug () then Format.printf "%a = 0@." Var.print x;
      compile code limit (pc + 2) (State.pop 1 state)
        (Let (x, Const 0) :: Set_field (y, n, z) :: instrs)
  | VECTLENGTH ->
      let y = State.accu state in
      let (x, state) = State.fresh_var state in
      if debug () then Format.printf "%a = %a.length@."
        Var.print x Var.print y;
      compile code limit (pc + 1) state
        (Let (x, Prim (Vectlength, [Pv y])) :: instrs)
  | GETVECTITEM ->
      let y = State.accu state in
      let z = State.peek 0 state in
      let (x, state) = State.fresh_var state in
      if debug () then Format.printf "%a = %a[%a]@."
        Var.print x Var.print y Var.print z;
      compile code limit (pc + 1) (State.pop 1 state)
        (Let (x, Prim (Array_get, [Pv y; Pv z])) :: instrs)
  | SETVECTITEM ->
      if debug () then Format.printf "%a[%a] = %a@." Var.print (State.accu state)
        Var.print (State.peek 0 state)
        Var.print (State.peek 1 state);
      let instrs =
        Array_set (State.accu state, State.peek 0 state, State.peek 1 state)
        :: instrs
      in
      let (x, state) = State.fresh_var state in
      if debug () then Format.printf "%a = 0@." Var.print x;
      compile code limit (pc + 1) (State.pop 2 state)
        (Let (x, Const 0) :: instrs)
  | GETSTRINGCHAR ->
      let y = State.accu state in
      let z = State.peek 0 state in
      let (x, state) = State.fresh_var state in
      if debug () then Format.printf "%a = %a[%a]@."
        Var.print x Var.print y Var.print z;
      compile code limit (pc + 1) (State.pop 1 state)
        (Let (x, Prim (Extern "caml_string_get", [Pv y; Pv z])) :: instrs)
  | SETSTRINGCHAR ->
      if debug () then Format.printf "%a[%a] = %a@." Var.print (State.accu state)
        Var.print (State.peek 0 state)
        Var.print (State.peek 1 state);
      let x = State.accu state in
      let y = State.peek 0 state in
      let z = State.peek 1 state in
      let (t, state) = State.fresh_var state in
      let instrs =
        Let (t, Prim (Extern "caml_string_set", [Pv x; Pv y; Pv z])) ::
        instrs in
      let (x, state) = State.fresh_var state in
      if debug () then Format.printf "%a = 0@." Var.print x;
      compile code limit (pc + 1) (State.pop 2 state)
        (Let (x, Const 0) :: instrs)
  | BRANCH ->
      let offset = gets code (pc + 1) in
      if debug () then Format.printf "... (branch)@.";
      (instrs, Branch (pc + offset + 1, State.stack_vars state), state)
  | BRANCHIF ->
      let offset = gets code (pc + 1) in
      let x = State.accu state in
      let args = State.stack_vars state in
      (instrs,
       Cond (IsTrue, x, (pc + offset + 1, args), (pc + 2, args)), state)
  | BRANCHIFNOT ->
      let offset = gets code (pc + 1) in
      let x = State.accu state in
      let args = State.stack_vars state in
      (instrs,
       Cond (IsTrue, x, (pc + 2, args), (pc + offset + 1, args)), state)
  | SWITCH ->
      if debug () then Format.printf "switch ...@.";
      let sz = getu code (pc + 1) in
      let x = State.accu state in
      let args = State.stack_vars state in
      let l = sz land 0xFFFF in
      let it =
        Array.init (sz land 0XFFFF)
          (fun i -> (pc + 2 + gets code (pc + 2 + i), args))
      in
      let bt =
        Array.init (sz lsr 16)
          (fun i -> (pc + 2 + gets code (pc + 2 + l + i), args))
      in
      (instrs, Switch (x, it, bt), state)
  | BOOLNOT ->
      let y = State.accu state in
      let (x, state) = State.fresh_var state in
      if debug () then Format.printf "%a = !%a@." Var.print x Var.print y;
      compile code limit (pc + 1) state (Let (x, Prim (Not, [Pv y])) :: instrs)
  | PUSHTRAP ->
      let addr = pc + 1 + gets code (pc + 1) in
      let (x, state') = State.fresh_var state in
      compile_block code addr state';
      compile_block code (pc + 2)
        {(State.push_handler state x addr)
         with State.stack =
            State.Dummy :: State.Dummy :: State.Dummy :: State.Dummy ::
            state.State.stack};
      (instrs,
       Pushtrap ((pc + 2, State.stack_vars state), x,
                 (addr, State.stack_vars state'), -1), state)
  | POPTRAP ->
      compile_block code (pc + 1) (State.pop 4 (State.pop_handler state));
      (instrs, Poptrap (pc + 1, State.stack_vars state), state)
  | RAISE ->
      if debug () then Format.printf "throw(%a)@." Var.print (State.accu state);
      (instrs, Raise (State.accu state), state)
  | CHECK_SIGNALS ->
      compile code limit (pc + 1) state instrs
  | C_CALL1 ->
      let prim = primitive_name state (getu code (pc + 1)) in
      if Primitive.resolve prim = "%identity" then
        (* This is a no-op *)
        compile code limit (pc + 2) state instrs
      else begin
        let y = State.accu state in
        let (x, state) = State.fresh_var state in
        if debug () then Format.printf "%a = ccall \"%s\" (%a)@."
          Var.print x prim Var.print y;
        compile code limit (pc + 2) state
          (Let (x, Prim (Extern prim, [Pv y])) :: instrs)
      end
  | C_CALL2 ->
      let prim = primitive_name state (getu code (pc + 1)) in
      let y = State.accu state in
      let z = State.peek 0 state in
      let (x, state) = State.fresh_var state in
      if debug () then Format.printf "%a = ccall \"%s\" (%a, %a)@."
        Var.print x prim Var.print y Var.print z;
      compile code limit (pc + 2) (State.pop 1 state)
        (Let (x, Prim (Extern prim, [Pv y; Pv z])) :: instrs)
  | C_CALL3 ->
      let prim = primitive_name state (getu code (pc + 1)) in
      let y = State.accu state in
      let z = State.peek 0 state in
      let t = State.peek 1 state in
      let (x, state) = State.fresh_var state in
      if debug () then Format.printf "%a = ccall \"%s\" (%a, %a, %a)@."
        Var.print x prim Var.print y Var.print z Var.print t;
      compile code limit (pc + 2) (State.pop 2 state)
        (Let (x, Prim (Extern prim, [Pv y; Pv z; Pv t])) :: instrs)
  | C_CALL4 ->
      let nargs = 4 in
      let prim = primitive_name state (getu code (pc + 1)) in
      let state = State.push state in
      let (x, state) = State.fresh_var state in
      let (args, state) = State.grab nargs state in
      if debug () then begin
        Format.printf "%a = ccal \"%s\" (" Var.print x prim;
        for i = 0 to nargs - 1 do
          if i > 0 then Format.printf ", ";
          Format.printf "%a" Var.print (List.nth args i);
        done;
        Format.printf ")@."
      end;
      compile code limit (pc + 2) state
        (Let (x, Prim (Extern prim, List.map (fun x -> Pv x) args)) :: instrs)
  | C_CALL5 ->
      let nargs = 5 in
      let prim = primitive_name state (getu code (pc + 1)) in
      let state = State.push state in
      let (x, state) = State.fresh_var state in
      let (args, state) = State.grab nargs state in
      if debug () then begin
        Format.printf "%a = ccal \"%s\" (" Var.print x prim;
        for i = 0 to nargs - 1 do
          if i > 0 then Format.printf ", ";
          Format.printf "%a" Var.print (List.nth args i);
        done;
        Format.printf ")@."
      end;
      compile code limit (pc + 2) state
        (Let (x, Prim (Extern prim, List.map (fun x -> Pv x) args)) :: instrs)
  | C_CALLN ->
      let nargs = getu code (pc + 1) in
      let prim = primitive_name state (getu code (pc + 2)) in
      let state = State.push state in
      let (x, state) = State.fresh_var state in
      let (args, state) = State.grab nargs state in
      if debug () then begin
        Format.printf "%a = ccal \"%s\" (" Var.print x prim;
        for i = 0 to nargs - 1 do
          if i > 0 then Format.printf ", ";
          Format.printf "%a" Var.print (List.nth args i);
        done;
        Format.printf ")@."
      end;
      compile code limit (pc + 3) state
        (Let (x, Prim (Extern prim, List.map (fun x -> Pv x) args)) :: instrs)
  | CONST0 ->
      let (x, state) = State.fresh_var state in
      if debug () then Format.printf "%a = 0@." Var.print x;
      compile code limit (pc + 1) state (Let (x, Const 0) :: instrs)
  | CONST1 ->
      let (x, state) = State.fresh_var state in
      if debug () then Format.printf "%a = 1@." Var.print x;
      compile code limit (pc + 1) state (Let (x, Const 1) :: instrs)
  | CONST2 ->
      let (x, state) = State.fresh_var state in
      if debug () then Format.printf "%a = 2@." Var.print x;
      compile code limit (pc + 1) state (Let (x, Const 2) :: instrs)
  | CONST3 ->
      let (x, state) = State.fresh_var state in
      if debug () then Format.printf "%a = 3@." Var.print x;
      compile code limit (pc + 1) state (Let (x, Const 3) :: instrs)
  | CONSTINT ->
      let n = gets code (pc + 1) in
      let (x, state) = State.fresh_var state in
      if debug () then Format.printf "%a = %d@." Var.print x n;
      compile code limit (pc + 2) state (Let (x, Const n) :: instrs)
  | PUSHCONST0 ->
      let state = State.push state in
      let (x, state) = State.fresh_var state in
      if debug () then Format.printf "%a = 0@." Var.print x;
      compile code limit (pc + 1) state (Let (x, Const 0) :: instrs)
  | PUSHCONST1 ->
      let state = State.push state in
      let (x, state) = State.fresh_var state in
      if debug () then Format.printf "%a = 1@." Var.print x;
      compile code limit (pc + 1) state (Let (x, Const 1) :: instrs)
  | PUSHCONST2 ->
      let state = State.push state in
      let (x, state) = State.fresh_var state in
      if debug () then Format.printf "%a = 2@." Var.print x;
      compile code limit (pc + 1) state (Let (x, Const 2) :: instrs)
  | PUSHCONST3 ->
      let state = State.push state in
      let (x, state) = State.fresh_var state in
      if debug () then Format.printf "%a = 3@." Var.print x;
      compile code limit (pc + 1) state (Let (x, Const 3) :: instrs)
  | PUSHCONSTINT ->
      let state = State.push state in
      let n = gets code (pc + 1) in
      let (x, state) = State.fresh_var state in
      if debug () then Format.printf "%a = %d@." Var.print x n;
      compile code limit (pc + 2) state (Let (x, Const n) :: instrs)
  | NEGINT ->
      let y = State.accu state in
      let (x', state) = State.fresh_var state in
      let (x, state) = State.fresh_var state in
      if debug () then Format.printf "%a = -%a@." Var.print x Var.print y;
      compile code limit (pc + 1) state
        (Let (x, Prim (WrapInt, [Pv x'])) ::
         Let (x', Prim (Extern "%int_neg", [Pv y])) :: instrs)
  | ADDINT ->
      let y = State.accu state in
      let z = State.peek 0 state in
      let (x', state) = State.fresh_var state in
      let (x, state) = State.fresh_var state in
      if debug () then Format.printf "%a = %a + %a@."
        Var.print x Var.print y Var.print z;
      compile code limit (pc + 1) (State.pop 1 state)
        (Let (x, Prim (WrapInt, [Pv x'])) ::
         Let (x', Prim (Extern "%int_add", [Pv y; Pv z])) :: instrs)
  | SUBINT ->
      let y = State.accu state in
      let z = State.peek 0 state in
      let (x', state) = State.fresh_var state in
      let (x, state) = State.fresh_var state in
      if debug () then
        Format.printf "%a = %a - %a@." Var.print x Var.print y Var.print z;
      compile code limit (pc + 1) (State.pop 1 state)
        (Let (x, Prim (WrapInt, [Pv x'])) ::
         Let (x', Prim (Extern "%int_sub", [Pv y; Pv z])) :: instrs)
  | MULINT ->
      let y = State.accu state in
      let z = State.peek 0 state in
      let (x, state) = State.fresh_var state in
      if debug () then
        Format.printf "%a = %a * %a@." Var.print x Var.print y Var.print z;
      compile code limit (pc + 1) (State.pop 1 state)
        (Let (x, Prim (Extern "%int_mul", [Pv y; Pv z])) :: instrs)
  | DIVINT ->
      let y = State.accu state in
      let z = State.peek 0 state in
      let (x, state) = State.fresh_var state in
      if debug () then
        Format.printf "%a = %a / %a@." Var.print x Var.print y Var.print z;
      compile code limit (pc + 1) (State.pop 1 state)
        (Let (x, Prim (Extern "%int_div", [Pv y; Pv z])) :: instrs)
  | MODINT ->
      let y = State.accu state in
      let z = State.peek 0 state in
      let (x, state) = State.fresh_var state in
      if debug () then
        Format.printf "%a = %a %% %a@." Var.print x Var.print y Var.print z;
      compile code limit (pc + 1) (State.pop 1 state)
        (Let (x, Prim (Extern "%int_mod", [Pv y; Pv z])) :: instrs)
  | ANDINT ->
      let y = State.accu state in
      let z = State.peek 0 state in
      let (x, state) = State.fresh_var state in
      if debug () then
        Format.printf "%a = %a & %a@." Var.print x Var.print y Var.print z;
      compile code limit (pc + 1) (State.pop 1 state)
        (Let (x, Prim (Extern "%int_and", [Pv y; Pv z])) :: instrs)
  | ORINT ->
      let y = State.accu state in
      let z = State.peek 0 state in
      let (x, state) = State.fresh_var state in
      if debug () then
        Format.printf "%a = %a | %a@." Var.print x Var.print y Var.print z;
      compile code limit (pc + 1) (State.pop 1 state)
        (Let (x, Prim (Extern "%int_or", [Pv y; Pv z])) :: instrs)
  | XORINT ->
      let y = State.accu state in
      let z = State.peek 0 state in
      let (x, state) = State.fresh_var state in
      if debug () then
        Format.printf "%a = %a ^ %a@." Var.print x Var.print y Var.print z;
      compile code limit (pc + 1) (State.pop 1 state)
        (Let (x, Prim (Extern "%int_xor", [Pv y; Pv z])) :: instrs)
  | LSLINT ->
      let y = State.accu state in
      let z = State.peek 0 state in
      let (x, state) = State.fresh_var state in
      if debug () then
        Format.printf "%a = %a << %a@." Var.print x Var.print y Var.print z;
      compile code limit (pc + 1) (State.pop 1 state)
        (Let (x, Prim (Extern "%int_lsl", [Pv y; Pv z])) :: instrs)
  | LSRINT ->
      let y = State.accu state in
      let z = State.peek 0 state in
      let (x, state) = State.fresh_var state in
      if debug () then
        Format.printf "%a = %a >>> %a@." Var.print x Var.print y Var.print z;
      compile code limit (pc + 1) (State.pop 1 state)
        (Let (x, Prim (Extern "%int_lsr", [Pv y; Pv z])) :: instrs)
  | ASRINT ->
      let y = State.accu state in
      let z = State.peek 0 state in
      let (x, state) = State.fresh_var state in
      if debug () then
        Format.printf "%a = %a >> %a@." Var.print x Var.print y Var.print z;
      compile code limit (pc + 1) (State.pop 1 state)
        (Let (x, Prim (Extern "%int_asr", [Pv y; Pv z])) :: instrs)
  | EQ ->
      let y = State.accu state in
      let z = State.peek 0 state in
      let (x, state) = State.fresh_var state in
      if debug () then Format.printf "%a = mk_bool(%a == %a)@."
        Var.print x Var.print y Var.print z;
      compile code limit (pc + 1) (State.pop 1 state)
        (Let (x, Prim (Eq, [Pv y; Pv z])) :: instrs)
  | NEQ ->
      let y = State.accu state in
      let z = State.peek 0 state in
      let (x, state) = State.fresh_var state in
      if debug () then Format.printf "%a = mk_bool(%a != %a)@."
        Var.print x Var.print y Var.print z;
      compile code limit (pc + 1) (State.pop 1 state)
        (Let (x, Prim (Neq, [Pv y; Pv z])) :: instrs)
  | LTINT ->
      let y = State.accu state in
      let z = State.peek 0 state in
      let (x, state) = State.fresh_var state in
      if debug () then Format.printf "%a = mk_bool(%a < %a)@." Var.print x
        Var.print y Var.print (State.peek 0 state);
      compile code limit (pc + 1) (State.pop 1 state)
        (Let (x, Prim (Lt, [Pv y; Pv z])) :: instrs)
  | LEINT ->
      let y = State.accu state in
      let z = State.peek 0 state in
      let (x, state) = State.fresh_var state in
      if debug () then Format.printf "%a = mk_bool(%a <= %a)@."
        Var.print x Var.print y Var.print z;
      compile code limit (pc + 1) (State.pop 1 state)
        (Let (x, Prim (Le, [Pv y; Pv z])) :: instrs)
  | GTINT ->
      let y = State.accu state in
      let z = State.peek 0 state in
      let (x, state) = State.fresh_var state in
      if debug () then Format.printf "%a = mk_bool(%a > %a)@."
        Var.print x Var.print y Var.print z;
      compile code limit (pc + 1) (State.pop 1 state)
        (Let (x, Prim (Lt, [Pv z; Pv y])) :: instrs)
  | GEINT ->
      let y = State.accu state in
      let z = State.peek 0 state in
      let (x, state) = State.fresh_var state in
      if debug () then Format.printf "%a = mk_bool(%a >= %a)@."
        Var.print x Var.print y Var.print z;
      compile code limit (pc + 1) (State.pop 1 state)
        (Let (x, Prim (Le, [Pv z; Pv y])) :: instrs)
  | OFFSETINT ->
      let n = gets code (pc + 1) in
      let y = State.accu state in
      let (z, state) = State.fresh_var state in
      let (x', state) = State.fresh_var state in
      let (x, state) = State.fresh_var state in
      if debug () then Format.printf "%a = %a + %d@." Var.print x Var.print y n;
      compile code limit (pc + 2) state
        (Let (x, Prim (WrapInt, [Pv x'])) ::
         Let (x', Prim (Extern "%int_add", [Pv y; Pv z])) ::
         Let (z, Const n) :: instrs)
  | OFFSETREF ->
      let n = gets code (pc + 1) in
      let x = State.accu state in
      if debug () then Format.printf "%a += %d@." Var.print x n;
      let instrs = Offset_ref (x, n) :: instrs in
      let (x, state) = State.fresh_var state in
      if debug () then Format.printf "x = 0@.";
      compile code limit (pc + 2) state (Let (x, Const 0) :: instrs)
  | ISINT ->
      let y = State.accu state in
      let (x, state) = State.fresh_var state in
      if debug () then Format.printf "%a = !%a@." Var.print x Var.print y;
      compile code limit (pc + 1) state (Let (x, Prim (IsInt, [Pv y])) :: instrs)
  | BEQ ->
      let n = gets code (pc + 1) in
      let offset = gets code (pc + 2) in
      let x = State.accu state in
      let args = State.stack_vars state in
      (instrs,
       Cond (CEq n, x, (pc + offset + 2, args), (pc + 3, args)), state)
  | BNEQ ->
      let n = gets code (pc + 1) in
      let offset = gets code (pc + 2) in
      let x = State.accu state in
      let args = State.stack_vars state in
      (instrs,
       Cond (CEq n, x, (pc + 3, args), (pc + offset + 2, args)), state)
  | BLTINT ->
      let n = gets code (pc + 1) in
      let offset = gets code (pc + 2) in
      let x = State.accu state in
      let args = State.stack_vars state in
      (instrs,
       Cond (CLt n, x, (pc + offset + 2, args), (pc + 3, args)), state)
  | BLEINT ->
      let n = gets code (pc + 1) in
      let offset = gets code (pc + 2) in
      let x = State.accu state in
      let args = State.stack_vars state in
      (instrs,
       Cond (CLe n, x, (pc + offset + 2, args), (pc + 3, args)), state)
  | BGTINT ->
      let n = gets code (pc + 1) in
      let offset = gets code (pc + 2) in
      let x = State.accu state in
      let args = State.stack_vars state in
      (instrs,
       Cond (CLe n, x, (pc + 3, args), (pc + offset + 2, args)), state)
  | BGEINT ->
      let n = gets code (pc + 1) in
      let offset = gets code (pc + 2) in
      let x = State.accu state in
      let args = State.stack_vars state in
      (instrs,
       Cond (CLt n, x, (pc + 3, args), (pc + offset + 2, args)), state)
  | BULTINT ->
      let n = getu code (pc + 1) in
      let offset = gets code (pc + 2) in
      let x = State.accu state in
      let args = State.stack_vars state in
      (instrs,
       Cond (CUlt n, x, (pc + offset + 2, args), (pc + 3, args)), state)
  | BUGEINT ->
      let n = getu code (pc + 1) in
      let offset = gets code (pc + 2) in
      let x = State.accu state in
      let args = State.stack_vars state in
      (instrs,
       Cond (CUlt n, x, (pc + 3, args), (pc + offset + 2, args)), state)
  | ULTINT ->
      let y = State.accu state in
      let z = State.peek 0 state in
      let (x, state) = State.fresh_var state in
      if debug () then Format.printf "%a = mk_bool(%a <= %a) (unsigned)@."
        Var.print x Var.print y Var.print z;
      compile code limit (pc + 1) (State.pop 1 state)
        (Let (x, Prim (Ult, [Pv y; Pv z])) :: instrs)
  | UGEINT ->
      let y = State.accu state in
      let z = State.peek 0 state in
      let (x, state) = State.fresh_var state in
      if debug () then Format.printf "%a = mk_bool(%a >= %a)@."
        Var.print x Var.print y Var.print z;
      compile code limit (pc + 1) (State.pop 1 state)
        (Let (x, Prim (Ult, [Pv z; Pv y])) :: instrs)
  | GETPUBMET ->
(*FIX: should cache*)
      let n = gets code (pc + 1) in
      let obj = State.accu state in
      let state = State.push state in
      let (tag, state) = State.fresh_var state in
      let (m, state) = State.fresh_var state in
if debug () then Format.printf "%a = %d@." Var.print tag n;
if debug () then Format.printf "%a = caml_get_public_method(%a, %a)@."
        Var.print m Var.print obj Var.print tag;
      compile code limit (pc + 3) state
        (Let (m, Prim (Extern "caml_get_public_method", [Pv obj; Pv tag])) ::
         Let (tag, Const n) :: instrs)
  | GETDYNMET ->
      let tag = State.accu state in
      let obj = State.peek 0 state in
      let (m, state) = State.fresh_var state in
if debug () then Format.printf "%a = caml_get_public_method(%a, %a)@."
        Var.print m Var.print obj Var.print tag;
      compile code limit (pc + 1) state
        (Let (m, Prim (Extern "caml_get_public_method", [Pv obj; Pv tag])) ::
         instrs)
  | GETMETHOD ->
      let lab = State.accu state in
      let obj = State.peek 0 state in
      let (meths, state) = State.fresh_var state in
      let (m, state) = State.fresh_var state in
if debug () then Format.printf "%a = lookup(%a, %a)@."
  Var.print m Var.print obj Var.print lab;
      compile code limit (pc + 1) state
        (Let (m, Prim (Array_get, [Pv meths; Pv lab])) ::
         Let (meths, Field (obj, 0)) :: instrs)
  | STOP ->
      (instrs, Stop, state)
  end

(****)

let merge_path p1 p2 =
  match p1, p2 with
    [], _ -> p2
  | _, [] -> p1
  | _     -> assert (p1 = p2); p1

let (>>) x f = f x

let fold_children blocks pc f accu =
  let block = AddrMap.find pc blocks in
  match block.branch with
    Return _ | Raise _ | Stop ->
      accu
  | Branch (pc', _) | Poptrap (pc', _) ->
      f pc' accu
  | Cond (_, _, (pc1, _), (pc2, _)) | Pushtrap ((pc1, _), _, (pc2, _), _) ->
      f pc1 accu >> f pc1 >> f pc2
  | Switch (_, a1, a2) ->
      accu >> Array.fold_right (fun (pc, _) accu -> f pc accu) a1
           >> Array.fold_right (fun (pc, _) accu -> f pc accu) a2

let rec traverse blocks pc visited blocks' =
  if not (AddrSet.mem pc visited) then begin
    let visited = AddrSet.add pc visited in
    let (visited, blocks', path) =
      fold_children blocks pc
        (fun pc (visited, blocks', path) ->
           let (visited, blocks', path') =
             traverse blocks pc visited blocks' in
           (visited, blocks', merge_path path path'))
        (visited, blocks', [])
    in
    let block = AddrMap.find pc blocks in
    let (blocks', path) =
      (* Note that there is no matching poptrap when an exception is always
         raised in the [try ... with ...] body. *)
      match block.branch, path with
        Pushtrap (cont1, x, cont2, _), pc3 :: rem ->
          (AddrMap.add
             pc { block with branch = Pushtrap (cont1, x, cont2, pc3) }
             blocks',
           rem)
      | Poptrap (pc, _), _ ->
          (blocks', pc :: path)
      | _ ->
          (blocks', path)
    in
    (visited, blocks', path)
  end else
    (visited, blocks', [])

let match_exn_traps ((_, blocks, _) as p) =
  fold_closures p
    (fun _ _ (pc, _) blocks' ->
       let (_, blocks', path) = traverse blocks pc AddrSet.empty blocks' in
       assert (path = []);
       blocks')
    blocks

(****)

let is_toplevel = ref false

let build_toplevel () = is_toplevel := true

let set_global x v rem =
  let globals = Var.fresh () in
  Let (globals,
       Prim (Extern "caml_js_var", [Pc (String "caml_global_data")])) ::
  Let (Var.fresh (),
       Prim (Extern "caml_js_set", [Pv globals; Pc (String x); Pv v])) ::
  rem

let parse_bytecode code state standalone_info =
  Code.Var.reset ();
  analyse_blocks code;
  compile_block code 0 state;

  let blocks =
    AddrMap.mapi
      (fun pc (state, instr, last) ->
         { params = State.stack_vars state;
           handler = State.current_handler state;
           body = instr; branch = last })
      !compiled_blocks
  in
  compiled_blocks := AddrMap.empty;

  let free_pc = String.length code / 4 in
  let g = State.globals state in
  let body =
    match standalone_info with
      Some (symb, crcs, prim, paths) ->
        let l = ref [] in

        let register_global n =
          l :=
            let x = Var.fresh () in
            Let (x, Const n) ::
            Let (Var.fresh (),
                 Prim (Extern "caml_register_global",
                       [Pv x ; Pv (access_global g n)])) ::
            !l
        in
        register_global 2; (* Failure *)
        register_global 3; (* Invalid_argument *)
        register_global 5; (* Division_by_zero *)
        for i = Array.length g.constants - 1  downto 0 do
          match g.vars.(i) with
            Some x when g.is_const.(i) ->
              if g.is_exported.(i) then register_global i;
              l := Let (x, Constant (parse_const g.constants.(i))) :: !l
          | _ ->
              ()
        done;
        if !is_toplevel then begin
          (* Include linking information *)
          let toc =
            [("SYMB", Obj.repr symb); ("CRCS", crcs); ("PRIM", Obj.repr prim)]
          in
          l :=
            (let x = Var.fresh () in
             Let (x, Constant (parse_const (Obj.repr toc))) ::
             set_global "toc" x !l);
          (* Include interface files *)
          let fields = ref [] in
          Tbl.iter
            (fun id num ->
               if id.Ident.flags = 1 then begin
                 let name = String.uncapitalize id.Ident.name ^ ".cmi" in
                 let file =
                   try
                     Util.find_in_paths paths name
                   with Not_found ->
                     Format.eprintf "%s: interface file '%s' not found@."
                       Sys.argv.(0) name;
                     exit 1
                 in
                 let s = Util.read_file file in
                 fields := Pc (String name) :: Pc (String s) :: !fields
               end) symb.num_tbl;
          l :=
            (let x = Var.fresh () in
             Let (x, Prim (Extern "%object_literal", !fields)) ::
             set_global "interfaces" x !l)
        end;

        !l
    | None ->
        let globals = Var.fresh () in
        let l =
          ref [Let (globals,
                    Prim (Extern "caml_js_var",
                          [Pc (String "caml_global_data")]))]
        in
        for i = 0 to Array.length g.vars - 1 do
          match g.vars.(i) with
            Some x when g.is_const.(i) ->
              l := Let (x, Field (globals, i)) :: !l
          | _ ->
              ()
        done;
        List.rev !l
  in
  let last = Branch (0, []) in
  let pc = free_pc in
  let blocks =
    AddrMap.add free_pc
      { params = []; handler = None; body = body; branch = last }
      blocks
  in
  let free_pc = free_pc + 1 in
  let blocks = match_exn_traps (pc, blocks, free_pc) in
  (pc, blocks, free_pc)

(****)

exception Bad_magic_number

let exec_magic_number = "Caml1999X008"

let seek_section toc ic name =
  let rec seek_sec curr_ofs = function
    [] -> raise Not_found
  | (n, len) :: rem ->
      if n = name
      then begin seek_in ic (curr_ofs - len); len end
      else seek_sec (curr_ofs - len) rem in
  seek_sec (in_channel_length ic - 16 - 8 * List.length toc) toc

let read_toc ic =
  let pos_trailer = in_channel_length ic - 16 in
  seek_in ic pos_trailer;
  let num_sections = input_binary_int ic in
  let header = String.create(String.length exec_magic_number) in
  really_input ic header 0 (String.length exec_magic_number);
  if header <> exec_magic_number then raise Bad_magic_number;
  seek_in ic (pos_trailer - 8 * num_sections);
  let section_table = ref [] in
  for i = 1 to num_sections do
    let name = String.create 4 in
    really_input ic name 0 4;
    let len = input_binary_int ic in
    section_table := (name, len) :: !section_table
  done;
  !section_table

let read_primitive_table toc ic =
  let len = seek_section toc ic "PRIM" in
  let p = String.create len in
  really_input ic p 0 len;
  let rec split beg cur =
    if cur >= len then []
    else if p.[cur] = '\000' then
      String.sub p beg (cur - beg) :: split (cur + 1) (cur + 1)
    else
      split beg (cur + 1) in
  Array.of_list(split 0 0)

(****)

let orig_code = Str.regexp_string
  ("\x6c\x00\x00\x00\x1f\x00\x00\x00" ^ (* pushconstint 31 *)
   "\x69\x00\x00\x00" ^                 (* pushconst1 *)
   "\x76\x00\x00\x00" ^                 (* lslint *)
   "\x84\x00\x00\x00\x00\x00\x00\x00\x05\x00\x00\x00" ^
                                        (* bneq +5    (overwrite from here) *)
   "\x67\x00\x00\x00\x1e\x00\x00\x00" ^ (* constint 30 *)
   "\x54\x00\x00\x00\x03\x00\x00\x00" ^ (* branch +3 *)
   "\x67\x00\x00\x00\x3e\x00\x00\x00" ^ (* constint 62 *)
   "\x69\x00\x00\x00" ^                 (* pushconst1 *)
   "\x76\x00\x00\x00")                  (* lslint *)

let fixed_code =
  "\x67\x00\x00\x00\x1f\x00\x00\x00" ^ (* constint 31 *)
  "\x54\x00\x00\x00\x06\x00\x00\x00" ^ (* branch   +6 *)
  "\x69\x00\x00\x00"                   (* pushconst1  *)

let fix_min_max_int code =
  begin try
    let i = Str.search_forward orig_code code 0 in
    String.blit fixed_code 0 code (i + 16) (String.length fixed_code)
  with Not_found ->
    Format.eprintf
      "Warning: could not fix min_int/max_int definition \
       (bytecode not found).@."
  end

(****)

let from_channel ~paths ic =
  let toc = read_toc ic in
  let primitives = read_primitive_table toc ic in
  let code_size = seek_section toc ic "CODE" in
  let code = String.create code_size in
  really_input ic code 0 code_size;

  ignore(seek_section toc ic "DATA");
  let init_data = (input_value ic : Obj.t array) in

  ignore(seek_section toc ic "SYMB");
  let symbols = (input_value ic : Ident.t numtable) in

  if !keep_variable_names then begin
    try
      ignore(seek_section toc ic "DBUG");
      Debug.read ic;
    with Not_found -> ()
  end;

  let globals = make_globals (Array.length init_data) init_data primitives in
  if !is_toplevel then begin
    Tbl.iter (fun _ n -> globals.is_exported.(n) <- true) symbols.num_tbl;
    Primitive.mark_used "caml_string_greaterthan"
  end;

  fix_min_max_int code;

  let state = State.initial globals in

  ignore(seek_section toc ic "CRCS");
  let crcs = (input_value ic : Obj.t) in
  let len = seek_section toc ic "PRIM" in
  let prim = String.create len in
  really_input ic prim 0 len;

  parse_bytecode code state (Some (symbols, crcs, prim, paths))

(* As input: list of primitives + size of global table *)
let from_string primitives code =
  let globals = make_globals 0 [||] primitives in
  let state = State.initial globals in
  parse_bytecode code state None
