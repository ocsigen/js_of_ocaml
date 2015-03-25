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

open Code
open Instr

let debug_parser = Option.Debug.find "parser"

type code = string

(* Copied from ocaml/typing/ident.ml *)
module Ident = struct
  type t = { stamp: int; name: string; mutable flags: int }
  type 'a tbl =
    | Empty
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
(* Copied from ocaml/utils/tbl.ml *)
module Tbl = struct
  type ('a, 'b) t =
    | Empty
    | Node of ('a, 'b) t * 'a * 'b * ('a, 'b) t * int

  let rec iter f = function
      Empty -> ()
    | Node(l, v, d, r, _) ->
      iter f l; f v d; iter f r

  let rec find compare x = function
      Empty ->
      raise Not_found
    | Node(l, v, d, r, _) ->
      let c = compare x v in
      if c = 0 then d
      else find compare x (if c < 0 then l else r)

  let rec fold f m accu =
    match m with
    | Empty -> accu
    | Node(l, v, d, r, _) ->
      fold f r (f v d (fold f l accu))
end

(* Copied from ocaml/bytecomp/symtable.ml *)
type 'a numtable =
  { num_cnt: int;
    num_tbl: ('a, int) Tbl.t }

(* Read and manipulate debug section *)
module Debug : sig

  (* instruct.ml *)
  type compilation_env =
    { ce_stack: int Ident.tbl; (* Positions of variables in the stack *)
      ce_heap: int Ident.tbl;  (* Structure of the heap-allocated env *)
      ce_rec: int Ident.tbl }  (* Functions bound by the same let rec *)

  (* lexing.ml *)
  type position =
    { pos_fname: string;
      pos_lnum: int;
      pos_bol: int;
      pos_cnum: int }

  (* location.ml *)
  type location =
    { loc_start: position;
      loc_end: position;
      loc_ghost: bool }

  (* instruct.ml *)
  type debug_event =
    { mutable ev_pos: int;                (* Position in bytecode *)
      ev_module: string;                  (* Name of defining module *)
      ev_loc: location;                   (* Location in source file *)
      ev_kind: debug_event_kind;          (* Before/after event *)
      ev_info: debug_event_info;          (* Extra information *)
      ev_typenv: unit;                    (* Typing environment *)
      ev_typsubst: unit;                  (* Substitution over types *)
      ev_compenv: compilation_env;        (* Compilation environment *)
      ev_stacksize: int;                  (* Size of stack frame *)
      ev_repr: debug_event_repr }         (* Position of the representative *)

  and debug_event_kind =
      Event_before
    | Event_after of unit
    | Event_pseudo

  and debug_event_info =
    | Event_function
    | Event_return of int
    | Event_other

  and debug_event_repr =
    | Event_none
    | Event_parent of int ref
    | Event_child of int ref


  type data
  val is_empty : data -> bool
  val propagate : Code.Var.t list -> Code.Var.t list -> unit
  val find : data -> Code.addr -> (int * string) list
  val find_loc : data -> ?after:bool -> int -> Parse_info.t option
  val mem : data -> Code.addr -> bool
  val paths : data -> string list
  val read : crcs:(string * string option) list -> in_channel -> data
  val no_data : unit -> data
  val fold : data -> (Code.addr -> debug_event -> 'a -> 'a) -> 'a -> 'a
end = struct

  type compilation_env =
    { ce_stack: int Ident.tbl; (* Positions of variables in the stack *)
      ce_heap: int Ident.tbl;  (* Structure of the heap-allocated env *)
      ce_rec: int Ident.tbl }  (* Functions bound by the same let rec *)

  type position =
    { pos_fname: string;
      pos_lnum: int;
      pos_bol: int;
      pos_cnum: int }

  type location =
    { loc_start: position;
      loc_end: position;
      loc_ghost: bool }

  type debug_event =
    { mutable ev_pos: int;                (* Position in bytecode *)
      ev_module: string;                  (* Name of defining module *)
      ev_loc: location;                   (* Location in source file *)
      ev_kind: debug_event_kind;          (* Before/after event *)
      ev_info: debug_event_info;          (* Extra information *)
      ev_typenv: unit;                    (* Typing environment *)
      ev_typsubst: unit;                  (* Substitution over types *)
      ev_compenv: compilation_env;        (* Compilation environment *)
      ev_stacksize: int;                  (* Size of stack frame *)
      ev_repr: debug_event_repr }         (* Position of the representative *)

  and debug_event_kind =
    | Event_before
    | Event_after of unit
    | Event_pseudo

  and debug_event_info =
    | Event_function
    | Event_return of int
    | Event_other

  and debug_event_repr =
    | Event_none
    | Event_parent of int ref
    | Event_child of int ref


  type ml_unit = {
    name   : string;
    crc    : string option;
    paths   : string list;
    source : string option;
  }
	
  type data = (int, debug_event) Hashtbl.t * (string, ml_unit) Hashtbl.t

  let relocate_event orig ev = ev.ev_pos <- (orig + ev.ev_pos) / 4

  let no_data () = Hashtbl.create 17, Hashtbl.create 17

  let is_empty (a,_) = Hashtbl.length a = 0

  let find_ml_in_paths paths name =
    let uname = String.uncapitalize name in
    try
      uname, Some (Util.find_in_path paths (uname^".ml"))
    with Not_found ->
      try
	name, Some (Util.find_in_path paths (name^".ml"))
      with Not_found ->
	uname, None
			   
  let read ~crcs ic =
    let events_by_pc = Hashtbl.create 257 in
    let units = Hashtbl.create 257 in
    let read_paths : unit -> string list =
      match Util.Version.v with
      | `V3 -> (fun () -> [])
      | `V4_02 -> (fun () -> (input_value ic : string list)) in
    let len = input_binary_int ic in
    for _i = 0 to len - 1 do
      let orig = input_binary_int ic in
      let evl : debug_event list = input_value ic in

      (* Work arround a bug in ocaml 4.02 *)
      (* debug section in pack module may be wrong *)
      (* containing no debug_info. *)
      (* In this case, evl in not a debug_info list but a *)
      (* string list (see read_paths) *)

      (* save the current position *)
      let pos = pos_in ic in
      let paths =
	try Some (read_paths ())
	with Failure _ ->
          (* restore position *)
          seek_in ic pos; None in
      match paths with
      | None -> ()
      | Some (paths : string list) ->
         let u = List.map (fun {ev_module} -> ev_module) evl in
         let u = Util.StringSet.(elements (of_list u)) in
         let u =
           List.map
             (fun name ->
	      let crc =
		try List.assoc name crcs
		with Not_found -> None in
	      let name, source = find_ml_in_paths paths name in
	      { name; crc; source; paths }) u
         in
         List.iter
	   (fun unit ->
	    Hashtbl.add units unit.name unit) u;
         List.iter
           (fun ev ->
            relocate_event orig ev;
            Hashtbl.add events_by_pc ev.ev_pos ev)
           evl
    done;
    events_by_pc, units

  let find (events_by_pc,_) pc =
    try
      let ev = Hashtbl.find events_by_pc pc in
      Ident.table_contents ev.ev_stacksize ev.ev_compenv.ce_stack
    with Not_found ->
      []

  let mem (tbl,_) = Hashtbl.mem tbl
 
  let find_loc (events_by_pc,units) ?(after = false) pc =
    try
      let (before, ev) =
        try false, Hashtbl.find events_by_pc pc with Not_found ->
        true,
        try Hashtbl.find events_by_pc (pc + 1) with Not_found ->
        try Hashtbl.find events_by_pc (pc + 2) with Not_found ->
        Hashtbl.find events_by_pc (pc + 3)
      in
      let loc = ev.ev_loc in
      let pos =
        if after then loc.loc_end else
        if before then loc.loc_start else
        match ev.ev_kind with Event_after _ -> loc.loc_end | _ -> loc.loc_start in
      let name =
	let uname = Filename.(basename (chop_extension pos.pos_fname)) in  
	try
	  let unit = Hashtbl.find units uname in
	  try Util.find_in_path unit.paths pos.pos_fname with
	  | Not_found ->
	     match unit.source with
	     | Some x -> x
	     | None   -> raise Not_found 
	with Not_found -> pos.pos_fname
      in
      Some {Parse_info.name;
            line=pos.pos_lnum - 1;
            col=pos.pos_cnum - pos.pos_bol;
            (* loc.li_end.pos_cnum - loc.li_end.pos_bol *)
            idx=0;
            fol=None}
    with Not_found ->
      None

  let paths ((_,units) : data) =
    Hashtbl.fold (fun name _ acc -> name :: acc ) units []

  let rec propagate l1 l2 =
    match l1, l2 with
      v1 :: r1, v2 :: r2 ->
      Var.propagate_name v1 v2;
      propagate r1 r2
    | _                  -> ()

(*  let iter events_by_pc f = Hashtbl.iter f events_by_pc *)

  let fold (events_by_pc,_) f acc = Hashtbl.fold f events_by_pc acc
end

(* Block analysis *)
(* Detect each block *)
module Blocks : sig
  type t
  val add  : t -> int -> t
  val next : t -> int -> int
  val analyse : Debug.data -> code -> t
end = struct
  type t = AddrSet.t * int

  let add (blocks,len) pc =  AddrSet.add pc blocks,len
  let rec scan debug blocks code pc len =
    if pc < len then begin
      match (get_instr code pc).kind with
        KNullary ->
        scan debug blocks code (pc + 1) len
      | KUnary ->
        scan debug blocks code (pc + 2) len
      | KBinary ->
        scan debug blocks code (pc + 3) len
      | KNullaryCall ->
        let blocks =
          if Debug.mem debug (pc + 1) then AddrSet.add pc blocks else blocks in
        scan debug blocks code (pc + 1) len
      | KUnaryCall ->
        let blocks =
          if Debug.mem debug (pc + 2) then AddrSet.add pc blocks else blocks in
        scan debug blocks code (pc + 2) len
      | KBinaryCall ->
        let blocks =
          if Debug.mem debug (pc + 3) then AddrSet.add pc blocks else blocks in
        scan debug blocks code (pc + 3) len
      | KJump ->
        let offset = gets code (pc + 1) in
        let blocks = AddrSet.add (pc + offset + 1) blocks in
        scan debug blocks code (pc + 2) len
      | KCond_jump ->
        let offset = gets code (pc + 1) in
        let blocks = AddrSet.add (pc + offset + 1) blocks in
        scan debug blocks code (pc + 2) len
      | KCmp_jump ->
        let offset = gets code (pc + 2) in
        let blocks = AddrSet.add (pc + offset + 2) blocks in
        scan debug blocks code (pc + 3) len
      | KSwitch ->
        let sz = getu code (pc + 1) in
        let blocks = ref blocks in
        for i = 0 to sz land 0xffff + sz lsr 16 - 1 do
          let offset = gets code (pc + 2 + i) in
          blocks := AddrSet.add (pc + offset + 2) !blocks
        done;
        scan debug !blocks code (pc + 2 + sz land 0xffff + sz lsr 16) len
      | KClosurerec ->
        let nfuncs = getu code (pc + 1) in
        scan debug blocks code (pc + nfuncs + 3) len
      | KClosure ->
        scan debug blocks code (pc + 3) len
      | KStop n ->
        scan debug blocks code (pc + n + 1) len
      | K_will_not_happen -> assert false
    end
    else blocks

  let rec next ((blocks,len) as info) pc =
    let pc = pc + 1 in
    if pc = len || AddrSet.mem pc blocks then pc else next info pc

  let analyse debug_data code =
    let blocks = AddrSet.empty in
    let len = String.length code  / 4 in
    (scan debug_data blocks code 0 len,len)

end

(* Parse constants *)
module Constants : sig
  val parse : Obj.t -> Code.constant
  val inlined : Obj.t -> bool
end = struct
  let same_custom x y =
    Obj.field x 0 == Obj.field (Obj.repr y) 0

  let warn_overflow i i32 =
    Util.warn
      "Warning: integer overflow: integer 0x%s truncated to 0x%lx; \
       the generated code might be incorrect.@." i i32

  let rec parse x =
    if Obj.is_block x then begin
      let tag = Obj.tag x in
      if tag = Obj.string_tag then
        String (Obj.magic x : string)
      else if tag = Obj.double_tag then
        Float (Obj.magic x : float)
      else if tag = Obj.double_array_tag then
        Float_array (Obj.magic x : float array)
      else if tag = Obj.custom_tag && same_custom x 0l then
        Int (Obj.magic x : int32)
      else if tag = Obj.custom_tag && same_custom x 0n then
        let i : nativeint = Obj.magic x in
        let i32 = Nativeint.to_int32 i in
        let i' = Nativeint.of_int32 i32 in
        if i' <> i then warn_overflow (Printf.sprintf "%nx" i) i32;
        Int i32
      else if tag = Obj.custom_tag && same_custom x 0L then
        Int64 (Obj.magic x : int64)
      else if tag < Obj.no_scan_tag then
        Tuple (tag,
               Array.init (Obj.size x) (fun i -> parse (Obj.field x i)))
      else
        assert false
    end else
      let i : int = Obj.magic x in
      let i32 = Int32.of_int i in
      let i' = Int32.to_int i32 in
      if i' <> i then warn_overflow (Printf.sprintf "%x" i) i32;
      Int i32

  let inlined x =
    not (Obj.is_block x)
    ||
    (let tag = Obj.tag x in
     (tag = Obj.double_tag)
     ||
     (tag = Obj.custom_tag && (same_custom x 0l || same_custom x 0n)))
end

(* Globals *)
type globals =
  { mutable vars : Var.t option array;
    mutable is_const : bool array;
    mutable is_exported : bool array;
    mutable override : (Var.t -> Code.instr list -> (Var.t * Code.instr list)) option array;
    constants : Obj.t array;
    primitives : string array }

let make_globals size constants primitives =
  { vars = Array.make size None;
    is_const = Array.make size false;
    is_exported = Array.make size false;
    override = Array.make size None;
    constants = constants; primitives = primitives }

let resize_array a len def =
  let b = Array.make len def in
  Array.blit a 0 b 0 (Array.length a);
  b

let resize_globals g size =
  g.vars <- resize_array g.vars size None;
  g.is_const <- resize_array g.is_const size false;
  g.is_exported <- resize_array g.is_exported size true;
  g.override <- resize_array g.override size None


(* State of the VM *)
module State = struct

  type elt = Var of Var.t | Dummy

  let elt_to_var e = match e with Var x -> x | _ -> assert false

  let print_elt f v =
    match v with
    | Var x   -> Format.fprintf f "%a" Var.print x
    (*    | Addr x  -> Format.fprintf f "[%d]" x*)
    | Dummy   -> Format.fprintf f "???"

  type t =
    { accu : elt; stack : elt list; env : elt array; env_offset : int;
      handlers : (Var.t * addr * int) list; globals : globals }

  let fresh_var state =
    let x = Var.fresh () in
    (x, {state with accu = Var x})

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
      | _ :: r -> st_pop (n - 1) r

  let push st = {st with stack = st.accu :: st.stack}

  let pop n st = {st with stack = st_pop n st.stack}

  let acc n st = {st with accu = List.nth st.stack n}

  let env_acc n st = {st with accu = st.env.(st.env_offset + n)}

  let accu st = elt_to_var st.accu

  let stack_vars st =
    List.fold_left
      (fun l e -> match e with Var x -> x :: l | Dummy -> l)
      [] (st.accu :: st.stack)

  let set_accu st x = {st with accu = Var x}

  let clear_accu st = {st with accu = Dummy}

  let peek n st = elt_to_var (List.nth st.stack n)

  let grab n st = (List.map elt_to_var (list_start n st.stack), pop n st)

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
    let stack =
      List.fold_right
        (fun e stack ->
           match e with
             Dummy ->
             Dummy :: stack
           | Var x ->
             let y = Var.fresh () in
             Var.propagate_name x y;
             Var y :: stack)
        state.stack []
    in
    let state = { state with stack = stack } in
    match state.accu with
      Dummy -> state
    | Var x ->
      let y,state = fresh_var state in
      Var.propagate_name x y;
      state

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
      globals = g }

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

  let rec make_stack i state =
    if i = 0
    then ([], state)
    else
      let (x, state) = fresh_var state in
      let (params, state) = make_stack (pred i) (push state) in
      if debug_parser () then if i > 1 then Format.printf ", ";
      if debug_parser () then Format.printf "%a" Var.print x;
      (x :: params, state)
end

let primitive_name state i =
  let g = State.globals state in
  assert (i >= 0 && i <= Array.length g.primitives);
  let prim = g.primitives.(i) in
  Primitive.add_external prim;
  prim

let access_global g i =
  match g.vars.(i) with
    Some x ->
    x
  | None ->
    g.is_const.(i) <- true;
    let x = Var.fresh () in
    g.vars.(i) <- Some x;
    x

let register_global ?(force=false) g i rem =
  if force || g.is_exported.(i)
  then
    Let (Var.fresh (),
         Prim (Extern "caml_register_global",
               [Pc (Int (Int32.of_int i)) ;
                Pv (access_global g i)])) :: rem
  else rem

let get_global state instrs i =
  State.size_globals state (i + 1);
  let g = State.globals state in
  match g.vars.(i) with
    Some x ->
    if debug_parser () then Format.printf "(global access %a)@." Var.print x;
    (x, State.set_accu state x, instrs)
  | None ->
    if
      i < Array.length g.constants && Constants.inlined g.constants.(i)
    then begin
      let (x, state) = State.fresh_var state in
      (x, state, Let (x, Constant (Constants.parse g.constants.(i))) :: instrs)
    end else begin
      g.is_const.(i) <- true;
      let (x, state) = State.fresh_var state in
      if debug_parser () then Format.printf "%a = CONST(%d)@." Var.print x i;
      g.vars.(i) <- Some x;
      (x, state, instrs)
    end

let tagged_blocks = ref AddrSet.empty
let compiled_blocks = ref AddrMap.empty
let method_cache_id = ref 1

type compile_info =
  { blocks : Blocks.t;
    code : string;
    limit : int;
    debug : Debug.data }

let rec compile_block blocks debug code pc state =
  if not (AddrSet.mem pc !tagged_blocks) then begin
    let limit = Blocks.next blocks pc in
    if debug_parser () then Format.eprintf "Compiling from %d to %d@." pc (limit - 1);
    let state = State.start_block state in
    tagged_blocks := AddrSet.add pc !tagged_blocks;
    let (instr, last, state') =
      compile {blocks; code; limit; debug} pc state [] in
    compiled_blocks :=
      AddrMap.add pc (state, List.rev instr, last) !compiled_blocks;
    begin match last with
      Branch (pc', _) | Poptrap (pc', _) ->
        compile_block blocks debug code pc' state'
    | Cond (_, _, (pc1, _), (pc2, _)) ->
        compile_block blocks debug code pc1 state';
        compile_block blocks debug code pc2 state'
    | Switch (_, l1, l2) ->
        Array.iter
          (fun (pc', _) -> compile_block blocks debug code pc' state') l1;
        Array.iter
          (fun (pc', _) -> compile_block blocks debug code pc' state') l2
    | Pushtrap _ | Raise _ | Return _ | Stop ->
        ()
    end
  end

and compile infos pc state instrs =
  if debug_parser () then State.print state;
  if pc = infos.limit then
    (instrs, Branch (pc, State.stack_vars state), state)
  else begin
    if debug_parser () then Format.eprintf "%4d " pc;

    State.name_vars state (Debug.find infos.debug pc);

    let code = infos.code in
    let instr =
      try
        get_instr code pc
      with Bad_instruction op ->
        if debug_parser () then Format.eprintf "%08x@." op;
        assert false
    in
    if debug_parser () then Format.eprintf "%08x %s@." instr.opcode instr.name;
    match instr.Instr.code with
    | ACC0 ->
      compile infos (pc + 1) (State.acc 0 state) instrs
    | ACC1 ->
      compile infos (pc + 1) (State.acc 1 state) instrs
    | ACC2 ->
      compile infos (pc + 1) (State.acc 2 state) instrs
    | ACC3 ->
      compile infos (pc + 1) (State.acc 3 state) instrs
    | ACC4 ->
      compile infos (pc + 1) (State.acc 4 state) instrs
    | ACC5 ->
      compile infos (pc + 1) (State.acc 5 state) instrs
    | ACC6 ->
      compile infos (pc + 1) (State.acc 6 state) instrs
    | ACC7 ->
      compile infos (pc + 1) (State.acc 7 state) instrs
    | ACC ->
      let n = getu code (pc + 1) in
      compile infos (pc + 2) (State.acc n state) instrs
    | PUSH ->
      compile infos (pc + 1) (State.push state) instrs
    | PUSHACC0 ->
      compile infos (pc + 1) (State.acc 0 (State.push state)) instrs
    | PUSHACC1 ->
      compile infos (pc + 1) (State.acc 1 (State.push state)) instrs
    | PUSHACC2 ->
      compile infos (pc + 1) (State.acc 2 (State.push state)) instrs
    | PUSHACC3 ->
      compile infos (pc + 1) (State.acc 3 (State.push state)) instrs
    | PUSHACC4 ->
      compile infos (pc + 1) (State.acc 4 (State.push state)) instrs
    | PUSHACC5 ->
      compile infos (pc + 1) (State.acc 5 (State.push state)) instrs
    | PUSHACC6 ->
      compile infos (pc + 1) (State.acc 6 (State.push state)) instrs
    | PUSHACC7 ->
      compile infos (pc + 1) (State.acc 7 (State.push state)) instrs
    | PUSHACC ->
      let n = getu code (pc + 1) in
      compile infos (pc + 2) (State.acc n (State.push state)) instrs
    | POP ->
      let n = getu code (pc + 1) in
      compile infos (pc + 2) (State.pop n state) instrs
    | ASSIGN ->
      let n = getu code (pc + 1) in
      let state = State.assign state n in
      let (x, state) = State.fresh_var state in
      if debug_parser () then Format.printf "%a = 0@." Var.print x;
      (* We switch to a different block as this may have
         changed the exception handler continuation *)
      compile_block infos.blocks infos.debug code (pc + 2) state;
      (Let (x, Const 0l) :: instrs,
       Branch (pc + 2, State.stack_vars state),
       state)
    | ENVACC1 ->
      compile infos (pc + 1) (State.env_acc 1 state) instrs
    | ENVACC2 ->
      compile infos (pc + 1) (State.env_acc 2 state) instrs
    | ENVACC3 ->
      compile infos (pc + 1) (State.env_acc 3 state) instrs
    | ENVACC4 ->
      compile infos (pc + 1) (State.env_acc 4 state) instrs
    | ENVACC ->
      let n = getu code (pc + 1) in
      compile infos (pc + 2) (State.env_acc n state) instrs
    | PUSHENVACC1 ->
      compile infos (pc + 1) (State.env_acc 1 (State.push state)) instrs
    | PUSHENVACC2 ->
      compile infos (pc + 1) (State.env_acc 2 (State.push state)) instrs
    | PUSHENVACC3 ->
      compile infos (pc + 1) (State.env_acc 3 (State.push state)) instrs
    | PUSHENVACC4 ->
      compile infos (pc + 1) (State.env_acc 4 (State.push state)) instrs
    | PUSHENVACC ->
      let n = getu code (pc + 1) in
      compile infos (pc + 2) (State.env_acc n (State.push state)) instrs
    | PUSH_RETADDR ->
      compile infos (pc + 2)
        {state with State.stack =
                      State.Dummy :: State.Dummy :: State.Dummy :: state.State.stack}
        instrs
    | APPLY ->
      let n = getu code (pc + 1) in
      let f = State.accu state in
      let (x, state) = State.fresh_var state in
      let (args, state) = State.grab n state in
      if debug_parser () then begin
        Format.printf "%a = %a(" Var.print x Var.print f;
        for i = 0 to n - 1 do
          if i > 0 then Format.printf ", ";
          Format.printf "%a" Var.print (List.nth args i)
        done;
        Format.printf ")@."
      end;
      compile infos (pc + 2) (State.pop 3 state)
        (Let (x, Apply (f, args, false)) :: instrs)
    | APPLY1 ->
      let f = State.accu state in
      let (x, state) = State.fresh_var state in
      let y = State.peek 0 state in
      if debug_parser () then
        Format.printf "%a = %a(%a)@." Var.print x
          Var.print f Var.print y;
      compile infos (pc + 1) (State.pop 1 state)
        (Let (x, Apply (f, [y], false)) :: instrs)
    | APPLY2 ->
      let f = State.accu state in
      let (x, state) = State.fresh_var state in
      let y = State.peek 0 state in
      let z = State.peek 1 state in
      if debug_parser () then Format.printf "%a = %a(%a, %a)@." Var.print x
          Var.print f Var.print y Var.print z;
      compile infos (pc + 1) (State.pop 2 state)
        (Let (x, Apply (f, [y; z], false)) :: instrs)
    | APPLY3 ->
      let f = State.accu state in
      let (x, state) = State.fresh_var state in
      let y = State.peek 0 state in
      let z = State.peek 1 state in
      let t = State.peek 2 state in
      if debug_parser () then Format.printf "%a = %a(%a, %a, %a)@." Var.print x
          Var.print f Var.print y Var.print z Var.print t;
      compile infos (pc + 1) (State.pop 3 state)
        (Let (x, Apply (f, [y; z; t], false)) :: instrs)
    | APPTERM ->
      let n = getu code (pc + 1) in
      let f = State.accu state in
      let (l, state) = State.grab n state in
      if debug_parser () then begin
        Format.printf "return %a(" Var.print f;
        for i = 0 to n - 1 do
          if i > 0 then Format.printf ", ";
          Format.printf "%a" Var.print (List.nth l i)
        done;
        Format.printf ")@."
      end;
      let (x, state) = State.fresh_var state in
      (Let (x, Apply (f, l, false)) :: instrs, Return x, state)
    | APPTERM1 ->
      let f = State.accu state in
      let x = State.peek 0 state in
      if debug_parser () then Format.printf "return %a(%a)@." Var.print f Var.print x;
      let (y, state) = State.fresh_var state in
      (Let (y, Apply (f, [x], false)) :: instrs, Return y, state)
    | APPTERM2 ->
      let f = State.accu state in
      let x = State.peek 0 state in
      let y = State.peek 1 state in
      if debug_parser () then Format.printf "return %a(%a, %a)@."
          Var.print f Var.print x Var.print y;
      let (z, state) = State.fresh_var state in
      (Let (z, Apply (f, [x; y], false)) :: instrs, Return z, state)
    | APPTERM3 ->
      let f = State.accu state in
      let x = State.peek 0 state in
      let y = State.peek 1 state in
      let z = State.peek 2 state in
      if debug_parser () then Format.printf "return %a(%a, %a, %a)@."
          Var.print f Var.print x Var.print y Var.print z;
      let (t, state) = State.fresh_var state in
      (Let (t, Apply (f, [x; y; z], false)) :: instrs, Return t, state)
    | RETURN ->
      let x = State.accu state in
      if debug_parser () then Format.printf "return %a@." Var.print x;
      (instrs, Return x, state)
    | RESTART ->
      assert false
    | GRAB ->
      compile infos (pc + 2) state instrs
    | CLOSURE ->
      let nvars = getu code (pc + 1) in
      let addr = pc + gets code (pc + 2) + 2 in
      let state = if nvars > 0 then State.push state else state in
      let (vals, state) = State.grab nvars state in
      let (x, state) = State.fresh_var state in
      let env =
        Array.of_list (State.Dummy :: List.map (fun x -> State.Var x) vals) in
      if debug_parser () then Format.printf "fun %a (" Var.print x;
      let nparams =
        match (get_instr code addr).Instr.code with
          GRAB -> getu code (addr + 1) + 1
        | _    -> 1
      in
      let state' = State.start_function state env 0 in
      let (params, state') = State.make_stack nparams state' in
      if debug_parser () then Format.printf ") {@.";
      let state' = State.clear_accu state' in
      compile_block infos.blocks infos.debug code addr state';
      if debug_parser () then Format.printf "}@.";
      let args = State.stack_vars state' in

      let (state'', _, _) = AddrMap.find addr !compiled_blocks in
      Debug.propagate (State.stack_vars state'') args;

      compile infos (pc + 3) state
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
             if debug_parser () then Format.printf "fun %a (" Var.print x;
             let nparams =
               match (get_instr code addr).Instr.code with
                 GRAB -> getu code (addr + 1) + 1
               | _    -> 1
             in
             let state' = State.start_function state env (i * 2) in
             let (params, state') = State.make_stack nparams state' in
             if debug_parser () then Format.printf ") {@.";
             let state' = State.clear_accu state' in
             compile_block infos.blocks infos.debug code addr state';
             if debug_parser () then Format.printf "}@.";
             let args = State.stack_vars state' in

             let (state'', _, _) = AddrMap.find addr !compiled_blocks in
             Debug.propagate (State.stack_vars state'') args;

             Let (x, Closure (List.rev params, (addr, args))) :: instr)
          instrs (List.rev !vars)
      in
      compile infos (pc + 3 + nfuncs) (State.acc (nfuncs - 1) state) instrs
    | OFFSETCLOSUREM2 ->
      compile infos (pc + 1) (State.env_acc (-2) state) instrs
    | OFFSETCLOSURE0 ->
      compile infos (pc + 1) (State.env_acc 0 state) instrs
    | OFFSETCLOSURE2 ->
      compile infos (pc + 1) (State.env_acc 2 state) instrs
    | OFFSETCLOSURE ->
      let n = gets code (pc + 1) in
      compile infos (pc + 2) (State.env_acc n state) instrs
    | PUSHOFFSETCLOSUREM2 ->
      let state = State.push state in
      compile infos (pc + 1) (State.env_acc (-2) state) instrs
    | PUSHOFFSETCLOSURE0 ->
      let state = State.push state in
      compile infos (pc + 1) (State.env_acc 0 state) instrs
    | PUSHOFFSETCLOSURE2 ->
      let state = State.push state in
      compile infos (pc + 1) (State.env_acc 2 state) instrs
    | PUSHOFFSETCLOSURE ->
      let state = State.push state in
      let n = gets code (pc + 1) in
      compile infos (pc + 2) (State.env_acc n state) instrs
    | GETGLOBAL ->
      let i = getu code (pc + 1) in
      let (_, state, instrs) = get_global state instrs i in
      compile infos (pc + 2) state instrs
    | PUSHGETGLOBAL ->
      let state = State.push state in
      let i = getu code (pc + 1) in
      let (_, state, instrs) = get_global state instrs i in
      compile infos (pc + 2) state instrs
    | GETGLOBALFIELD ->
      let i = getu code (pc + 1) in
      let (x, state, instrs) = get_global state instrs i in
      let j = getu code (pc + 2) in
      let (y, state) = State.fresh_var state in
      if debug_parser () then Format.printf "%a = %a[%d]@." Var.print y Var.print x j;
      compile infos (pc + 3) state (Let (y, Field (x, j)) :: instrs)
    | PUSHGETGLOBALFIELD ->
      let state = State.push state in
      let i = getu code (pc + 1) in
      let (x, state, instrs) = get_global state instrs i in
      let j = getu code (pc + 2) in
      let (y, state) = State.fresh_var state in
      if debug_parser () then Format.printf "%a = %a[%d]@." Var.print y Var.print x j;
      compile infos (pc + 3) state (Let (y, Field (x, j)) :: instrs)
    | SETGLOBAL ->
      let i = getu code (pc + 1) in
      State.size_globals state (i + 1);
      let y = State.accu state in
      let g = State.globals state in
      assert (g.vars.(i) = None);
      if debug_parser () then Format.printf "(global %d) = %a@." i Var.print y;
      let instrs = match g.override.(i) with
        | Some f ->
          let v,instrs = f y instrs in
          g.vars.(i) <- Some v;
          instrs
        | None ->
          g.vars.(i) <- Some y;
          instrs in
      let (x, state) = State.fresh_var state in
      if debug_parser () then Format.printf "%a = 0@." Var.print x;
      let instrs = register_global g i instrs in
      compile infos (pc + 2) state (Let (x, Const 0l) :: instrs)
    | ATOM0 ->
      let (x, state) = State.fresh_var state in
      if debug_parser () then Format.printf "%a = ATOM(0)@." Var.print x;
      compile infos (pc + 1) state (Let (x, Block (0, [||])) :: instrs)
    | ATOM ->
      let i = getu code (pc + 1) in
      let (x, state) = State.fresh_var state in
      if debug_parser () then Format.printf "%a = ATOM(%d)@." Var.print x i;
      compile infos (pc + 2) state (Let (x, Block (i, [||])) :: instrs)
    | PUSHATOM0 ->
      let state = State.push state in
      let (x, state) = State.fresh_var state in
      if debug_parser () then Format.printf "%a = ATOM(0)@." Var.print x;
      compile infos (pc + 1) state (Let (x, Block (0, [||])) :: instrs)
    | PUSHATOM ->
      let state = State.push state in
      let i = getu code (pc + 1) in
      let (x, state) = State.fresh_var state in
      if debug_parser () then Format.printf "%a = ATOM(%d)@." Var.print x i;
      compile infos (pc + 2) state (Let (x, Block (i, [||])) :: instrs)
    | MAKEBLOCK ->
      let size = getu code (pc + 1) in
      let tag = getu code (pc + 2) in
      let state = State.push state in
      let (x, state) = State.fresh_var state in
      let (contents, state) = State.grab size state in
      if debug_parser () then begin
        Format.printf "%a = { " Var.print x;
        for i = 0 to size - 1 do
          Format.printf "%d = %a; " i Var.print (List.nth contents i);
        done;
        Format.printf "}@."
      end;
      compile infos (pc + 3) state
        (Let (x, Block (tag, Array.of_list contents)) :: instrs)
    | MAKEBLOCK1 ->
      let tag = getu code (pc + 1) in
      let y = State.accu state in
      let (x, state) = State.fresh_var state in
      if debug_parser () then Format.printf "%a = { 0 = %a; }@." Var.print x Var.print y;
      compile infos (pc + 2) state (Let (x, Block (tag, [|y|])) :: instrs)
    | MAKEBLOCK2 ->
      let tag = getu code (pc + 1) in
      let y = State.accu state in
      let z = State.peek 0 state in
      let (x, state) = State.fresh_var state in
      if debug_parser () then Format.printf "%a = { 0 = %a; 1 = %a; }@."
          Var.print x Var.print y Var.print z;
      compile infos (pc + 2) (State.pop 1 state)
        (Let (x, Block (tag, [|y; z|])) :: instrs)
    | MAKEBLOCK3 ->
      let tag = getu code (pc + 1) in
      let y = State.accu state in
      let z = State.peek 0 state in
      let t = State.peek 1 state in
      let (x, state) = State.fresh_var state in
      if debug_parser () then Format.printf "%a = { 0 = %a; 1 = %a; 2 = %a }@."
          Var.print x Var.print y Var.print z Var.print t;
      compile infos (pc + 2) (State.pop 2 state)
        (Let (x, Block (tag, [|y; z; t|])) :: instrs)
    | MAKEFLOATBLOCK ->
      let size = getu code (pc + 1) in
      let state = State.push state in
      let (x, state) = State.fresh_var state in
      let (contents, state) = State.grab size state in
      if debug_parser () then begin
        Format.printf "%a = { " Var.print x;
        for i = 0 to size - 1 do
          Format.printf "%d = %a; " i Var.print (List.nth contents i);
        done;
        Format.printf "}@."
      end;
      compile infos (pc + 2) state
        (Let (x, Block (254, Array.of_list contents)) :: instrs)
    | GETFIELD0 ->
      let y = State.accu state in
      let (x, state) = State.fresh_var state in
      if debug_parser () then Format.printf "%a = %a[0]@." Var.print x Var.print y;
      compile infos (pc + 1) state (Let (x, Field (y, 0)) :: instrs)
    | GETFIELD1 ->
      let y = State.accu state in
      let (x, state) = State.fresh_var state in
      if debug_parser () then Format.printf "%a = %a[1]@." Var.print x Var.print y;
      compile infos (pc + 1) state (Let (x, Field (y, 1)) :: instrs)
    | GETFIELD2 ->
      let y = State.accu state in
      let (x, state) = State.fresh_var state in
      if debug_parser () then Format.printf "%a = %a[2]@." Var.print x Var.print y;
      compile infos (pc + 1) state (Let (x, Field (y, 2)) :: instrs)
    | GETFIELD3 ->
      let y = State.accu state in
      let (x, state) = State.fresh_var state in
      if debug_parser () then Format.printf "%a = %a[3]@." Var.print x Var.print y;
      compile infos (pc + 1) state (Let (x, Field (y, 3)) :: instrs)
    | GETFIELD ->
      let y = State.accu state in
      let n = getu code (pc + 1) in
      let (x, state) = State.fresh_var state in
      if debug_parser () then Format.printf "%a = %a[%d]@." Var.print x Var.print y n;
      compile infos (pc + 2) state (Let (x, Field (y, n)) :: instrs)
    | GETFLOATFIELD ->
      let y = State.accu state in
      let n = getu code (pc + 1) in
      let (x, state) = State.fresh_var state in
      if debug_parser () then Format.printf "%a = %a[%d]@." Var.print x Var.print y n;
      compile infos (pc + 2) state (Let (x, Field (y, n)) :: instrs)
    | SETFIELD0 ->
      let y = State.accu state in
      let z = State.peek 0 state in
      if debug_parser () then Format.printf "%a[0] = %a@." Var.print y Var.print z;
      let (x, state) = State.fresh_var state in
      if debug_parser () then Format.printf "%a = 0@." Var.print x;
      compile infos (pc + 1) (State.pop 1 state)
        (Let (x, Const 0l) :: Set_field (y, 0, z) :: instrs)
    | SETFIELD1 ->
      let y = State.accu state in
      let z = State.peek 0 state in
      if debug_parser () then Format.printf "%a[1] = %a@." Var.print y Var.print z;
      let (x, state) = State.fresh_var state in
      if debug_parser () then Format.printf "%a = 0@." Var.print x;
      compile infos (pc + 1) (State.pop 1 state)
        (Let (x, Const 0l) :: Set_field (y, 1, z) :: instrs)
    | SETFIELD2 ->
      let y = State.accu state in
      let z = State.peek 0 state in
      if debug_parser () then Format.printf "%a[2] = %a@." Var.print y Var.print z;
      let (x, state) = State.fresh_var state in
      if debug_parser () then Format.printf "%a = 0@." Var.print x;
      compile infos (pc + 1) (State.pop 1 state)
        (Let (x, Const 0l) :: Set_field (y, 2, z) :: instrs)
    | SETFIELD3 ->
      let y = State.accu state in
      let z = State.peek 0 state in
      if debug_parser () then Format.printf "%a[3] = %a@." Var.print y Var.print z;
      let (x, state) = State.fresh_var state in
      if debug_parser () then Format.printf "%a = 0@." Var.print x;
      compile infos (pc + 1) (State.pop 1 state)
        (Let (x, Const 0l) :: Set_field (y, 3, z) :: instrs)
    | SETFIELD ->
      let y = State.accu state in
      let z = State.peek 0 state in
      let n = getu code (pc + 1) in
      if debug_parser () then Format.printf "%a[%d] = %a@." Var.print y n Var.print z;
      let (x, state) = State.fresh_var state in
      if debug_parser () then Format.printf "%a = 0@." Var.print x;
      compile infos (pc + 2) (State.pop 1 state)
        (Let (x, Const 0l) :: Set_field (y, n, z) :: instrs)
    | SETFLOATFIELD ->
      let y = State.accu state in
      let z = State.peek 0 state in
      let n = getu code (pc + 1) in
      if debug_parser () then Format.printf "%a[%d] = %a@." Var.print y n Var.print z;
      let (x, state) = State.fresh_var state in
      if debug_parser () then Format.printf "%a = 0@." Var.print x;
      compile infos (pc + 2) (State.pop 1 state)
        (Let (x, Const 0l) :: Set_field (y, n, z) :: instrs)
    | VECTLENGTH ->
      let y = State.accu state in
      let (x, state) = State.fresh_var state in
      if debug_parser () then Format.printf "%a = %a.length@."
          Var.print x Var.print y;
      compile infos (pc + 1) state
        (Let (x, Prim (Vectlength, [Pv y])) :: instrs)
    | GETVECTITEM ->
      let y = State.accu state in
      let z = State.peek 0 state in
      let (x, state) = State.fresh_var state in
      if debug_parser () then Format.printf "%a = %a[%a]@."
          Var.print x Var.print y Var.print z;
      compile infos (pc + 1) (State.pop 1 state)
        (Let (x, Prim (Array_get, [Pv y; Pv z])) :: instrs)
    | SETVECTITEM ->
      if debug_parser () then Format.printf "%a[%a] = %a@." Var.print (State.accu state)
          Var.print (State.peek 0 state)
          Var.print (State.peek 1 state);
      let instrs =
        Array_set (State.accu state, State.peek 0 state, State.peek 1 state)
        :: instrs
      in
      let (x, state) = State.fresh_var state in
      if debug_parser () then Format.printf "%a = 0@." Var.print x;
      compile infos (pc + 1) (State.pop 2 state)
        (Let (x, Const 0l) :: instrs)
    | GETSTRINGCHAR ->
      let y = State.accu state in
      let z = State.peek 0 state in
      let (x, state) = State.fresh_var state in
      if debug_parser () then Format.printf "%a = %a[%a]@."
          Var.print x Var.print y Var.print z;
      compile infos (pc + 1) (State.pop 1 state)
        (Let (x, Prim (Extern "caml_string_unsafe_get", [Pv y; Pv z])) ::
         instrs)
    | SETSTRINGCHAR ->
      if debug_parser () then Format.printf "%a[%a] = %a@." Var.print (State.accu state)
          Var.print (State.peek 0 state)
          Var.print (State.peek 1 state);
      let x = State.accu state in
      let y = State.peek 0 state in
      let z = State.peek 1 state in
      let (t, state) = State.fresh_var state in
      let instrs =
        Let (t, Prim (Extern "caml_string_unsafe_set", [Pv x; Pv y; Pv z])) ::
        instrs in
      let (x, state) = State.fresh_var state in
      if debug_parser () then Format.printf "%a = 0@." Var.print x;
      compile infos (pc + 1) (State.pop 2 state)
        (Let (x, Const 0l) :: instrs)
    | BRANCH ->
      let offset = gets code (pc + 1) in
      if debug_parser () then Format.printf "... (branch)@.";
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
      if debug_parser () then Format.printf "switch ...@.";
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
      if debug_parser () then Format.printf "%a = !%a@." Var.print x Var.print y;
      compile infos (pc + 1) state (Let (x, Prim (Not, [Pv y])) :: instrs)
    | PUSHTRAP ->
      let addr = pc + 1 + gets code (pc + 1) in
      let (x, state') = State.fresh_var state in
      compile_block infos.blocks infos.debug code addr state';
      compile_block infos.blocks infos.debug code (pc + 2)
        {(State.push_handler state x addr)
         with State.stack =
                State.Dummy :: State.Dummy :: State.Dummy :: State.Dummy ::
                state.State.stack};
      (instrs,
       Pushtrap ((pc + 2, State.stack_vars state), x,
                 (addr, State.stack_vars state'), -1), state)
    | POPTRAP ->
      compile_block infos.blocks infos.debug code
        (pc + 1) (State.pop 4 (State.pop_handler state));
      (instrs, Poptrap (pc + 1, State.stack_vars state), state)
    | RERAISE
    | RAISE_NOTRACE
    | RAISE ->
      if debug_parser () then
        Format.printf "throw(%a)@." Var.print (State.accu state);
      (instrs, Raise (State.accu state), state)
    | CHECK_SIGNALS ->
      compile infos (pc + 1) state instrs
    | C_CALL1 ->
      let prim = primitive_name state (getu code (pc + 1)) in
      if Primitive.resolve prim = "%identity" then
        (* This is a no-op *)
        compile infos (pc + 2) state instrs
      else begin
        let y = State.accu state in
        let (x, state) = State.fresh_var state in
        if debug_parser () then Format.printf "%a = ccall \"%s\" (%a)@."
            Var.print x prim Var.print y;
        compile infos (pc + 2) state
          (Let (x, Prim (Extern prim, [Pv y])) :: instrs)
      end
    | C_CALL2 ->
      let prim = primitive_name state (getu code (pc + 1)) in
      let y = State.accu state in
      let z = State.peek 0 state in
      let (x, state) = State.fresh_var state in
      if debug_parser () then Format.printf "%a = ccall \"%s\" (%a, %a)@."
          Var.print x prim Var.print y Var.print z;
      compile infos (pc + 2) (State.pop 1 state)
        (Let (x, Prim (Extern prim, [Pv y; Pv z])) :: instrs)
    | C_CALL3 ->
      let prim = primitive_name state (getu code (pc + 1)) in
      let y = State.accu state in
      let z = State.peek 0 state in
      let t = State.peek 1 state in
      let (x, state) = State.fresh_var state in
      if debug_parser () then Format.printf "%a = ccall \"%s\" (%a, %a, %a)@."
          Var.print x prim Var.print y Var.print z Var.print t;
      compile infos (pc + 2) (State.pop 2 state)
        (Let (x, Prim (Extern prim, [Pv y; Pv z; Pv t])) :: instrs)
    | C_CALL4 ->
      let nargs = 4 in
      let prim = primitive_name state (getu code (pc + 1)) in
      let state = State.push state in
      let (x, state) = State.fresh_var state in
      let (args, state) = State.grab nargs state in
      if debug_parser () then begin
        Format.printf "%a = ccal \"%s\" (" Var.print x prim;
        for i = 0 to nargs - 1 do
          if i > 0 then Format.printf ", ";
          Format.printf "%a" Var.print (List.nth args i);
        done;
        Format.printf ")@."
      end;
      compile infos (pc + 2) state
        (Let (x, Prim (Extern prim, List.map (fun x -> Pv x) args)) :: instrs)
    | C_CALL5 ->
      let nargs = 5 in
      let prim = primitive_name state (getu code (pc + 1)) in
      let state = State.push state in
      let (x, state) = State.fresh_var state in
      let (args, state) = State.grab nargs state in
      if debug_parser () then begin
        Format.printf "%a = ccal \"%s\" (" Var.print x prim;
        for i = 0 to nargs - 1 do
          if i > 0 then Format.printf ", ";
          Format.printf "%a" Var.print (List.nth args i);
        done;
        Format.printf ")@."
      end;
      compile infos (pc + 2) state
        (Let (x, Prim (Extern prim, List.map (fun x -> Pv x) args)) :: instrs)
    | C_CALLN ->
      let nargs = getu code (pc + 1) in
      let prim = primitive_name state (getu code (pc + 2)) in
      let state = State.push state in
      let (x, state) = State.fresh_var state in
      let (args, state) = State.grab nargs state in
      if debug_parser () then begin
        Format.printf "%a = ccal \"%s\" (" Var.print x prim;
        for i = 0 to nargs - 1 do
          if i > 0 then Format.printf ", ";
          Format.printf "%a" Var.print (List.nth args i);
        done;
        Format.printf ")@."
      end;
      compile infos (pc + 3) state
        (Let (x, Prim (Extern prim, List.map (fun x -> Pv x) args)) :: instrs)
    | ( CONST0 | CONST1 | CONST2 | CONST3 ) as cc ->
      let (x, state) = State.fresh_var state in
      let n = match cc with
        | CONST0 -> 0l
        | CONST1 -> 1l
        | CONST2 -> 2l
        | CONST3 -> 3l
        | _ -> assert false in
      if debug_parser () then Format.printf "%a = %ld@." Var.print x n;
      compile infos (pc + 1) state (Let (x, Const n) :: instrs)
    | CONSTINT ->
      let n = gets32 code (pc + 1) in
      let (x, state) = State.fresh_var state in
      if debug_parser () then Format.printf "%a = %ld@." Var.print x n;
      compile infos (pc + 2) state (Let (x, Const n) :: instrs)
    | ( PUSHCONST0 | PUSHCONST1 | PUSHCONST2 | PUSHCONST3 ) as cc ->
      let state = State.push state in
      let (x, state) = State.fresh_var state in
      let n = match cc with
        | PUSHCONST0 -> 0l
        | PUSHCONST1 -> 1l
        | PUSHCONST2 -> 2l
        | PUSHCONST3 -> 3l
        | _ -> assert false in
      if debug_parser () then Format.printf "%a = %ld@." Var.print x n;
      compile infos (pc + 1) state (Let (x, Const n) :: instrs)
    | PUSHCONSTINT ->
      let state = State.push state in
      let n = gets32 code (pc + 1) in
      let (x, state) = State.fresh_var state in
      if debug_parser () then Format.printf "%a = %ld@." Var.print x n;
      compile infos (pc + 2) state (Let (x, Const n) :: instrs)
    | NEGINT ->
      let y = State.accu state in
      let (x, state) = State.fresh_var state in
      if debug_parser () then Format.printf "%a = -%a@." Var.print x Var.print y;
      compile infos (pc + 1) state
        (Let (x, Prim (Extern "%int_neg", [Pv y])) :: instrs)
    | ADDINT ->
      let y = State.accu state in
      let z = State.peek 0 state in
      let (x, state) = State.fresh_var state in
      if debug_parser () then Format.printf "%a = %a + %a@."
          Var.print x Var.print y Var.print z;
      compile infos (pc + 1) (State.pop 1 state)
        (Let (x, Prim (Extern "%int_add", [Pv y; Pv z])) :: instrs)
    | SUBINT ->
      let y = State.accu state in
      let z = State.peek 0 state in
      let (x, state) = State.fresh_var state in
      if debug_parser () then
        Format.printf "%a = %a - %a@." Var.print x Var.print y Var.print z;
      compile infos (pc + 1) (State.pop 1 state)
        (Let (x, Prim (Extern "%int_sub", [Pv y; Pv z])) :: instrs)
    | MULINT ->
      let y = State.accu state in
      let z = State.peek 0 state in
      let (x, state) = State.fresh_var state in
      if debug_parser () then
        Format.printf "%a = %a * %a@." Var.print x Var.print y Var.print z;
      compile infos (pc + 1) (State.pop 1 state)
        (Let (x, Prim (Extern "%int_mul", [Pv y; Pv z])) :: instrs)
    | DIVINT ->
      let y = State.accu state in
      let z = State.peek 0 state in
      let (x, state) = State.fresh_var state in
      if debug_parser () then
        Format.printf "%a = %a / %a@." Var.print x Var.print y Var.print z;
      compile infos (pc + 1) (State.pop 1 state)
        (Let (x, Prim (Extern "%int_div", [Pv y; Pv z])) :: instrs)
    | MODINT ->
      let y = State.accu state in
      let z = State.peek 0 state in
      let (x, state) = State.fresh_var state in
      if debug_parser () then
        Format.printf "%a = %a %% %a@." Var.print x Var.print y Var.print z;
      compile infos (pc + 1) (State.pop 1 state)
        (Let (x, Prim (Extern "%int_mod", [Pv y; Pv z])) :: instrs)
    | ANDINT ->
      let y = State.accu state in
      let z = State.peek 0 state in
      let (x, state) = State.fresh_var state in
      if debug_parser () then
        Format.printf "%a = %a & %a@." Var.print x Var.print y Var.print z;
      compile infos (pc + 1) (State.pop 1 state)
        (Let (x, Prim (Extern "%int_and", [Pv y; Pv z])) :: instrs)
    | ORINT ->
      let y = State.accu state in
      let z = State.peek 0 state in
      let (x, state) = State.fresh_var state in
      if debug_parser () then
        Format.printf "%a = %a | %a@." Var.print x Var.print y Var.print z;
      compile infos (pc + 1) (State.pop 1 state)
        (Let (x, Prim (Extern "%int_or", [Pv y; Pv z])) :: instrs)
    | XORINT ->
      let y = State.accu state in
      let z = State.peek 0 state in
      let (x, state) = State.fresh_var state in
      if debug_parser () then
        Format.printf "%a = %a ^ %a@." Var.print x Var.print y Var.print z;
      compile infos (pc + 1) (State.pop 1 state)
        (Let (x, Prim (Extern "%int_xor", [Pv y; Pv z])) :: instrs)
    | LSLINT ->
      let y = State.accu state in
      let z = State.peek 0 state in
      let (x, state) = State.fresh_var state in
      if debug_parser () then
        Format.printf "%a = %a << %a@." Var.print x Var.print y Var.print z;
      compile infos (pc + 1) (State.pop 1 state)
        (Let (x, Prim (Extern "%int_lsl", [Pv y; Pv z])) :: instrs)
    | LSRINT ->
      let y = State.accu state in
      let z = State.peek 0 state in
      let (x, state) = State.fresh_var state in
      if debug_parser () then
        Format.printf "%a = %a >>> %a@." Var.print x Var.print y Var.print z;
      compile infos (pc + 1) (State.pop 1 state)
        (Let (x, Prim (Extern "%int_lsr", [Pv y; Pv z])) :: instrs)
    | ASRINT ->
      let y = State.accu state in
      let z = State.peek 0 state in
      let (x, state) = State.fresh_var state in
      if debug_parser () then
        Format.printf "%a = %a >> %a@." Var.print x Var.print y Var.print z;
      compile infos (pc + 1) (State.pop 1 state)
        (Let (x, Prim (Extern "%int_asr", [Pv y; Pv z])) :: instrs)
    | EQ ->
      let y = State.accu state in
      let z = State.peek 0 state in
      let (x, state) = State.fresh_var state in
      if debug_parser () then Format.printf "%a = mk_bool(%a == %a)@."
          Var.print x Var.print y Var.print z;
      compile infos (pc + 1) (State.pop 1 state)
        (Let (x, Prim (Eq, [Pv y; Pv z])) :: instrs)
    | NEQ ->
      let y = State.accu state in
      let z = State.peek 0 state in
      let (x, state) = State.fresh_var state in
      if debug_parser () then Format.printf "%a = mk_bool(%a != %a)@."
          Var.print x Var.print y Var.print z;
      compile infos (pc + 1) (State.pop 1 state)
        (Let (x, Prim (Neq, [Pv y; Pv z])) :: instrs)
    | LTINT ->
      let y = State.accu state in
      let z = State.peek 0 state in
      let (x, state) = State.fresh_var state in
      if debug_parser () then Format.printf "%a = mk_bool(%a < %a)@." Var.print x
          Var.print y Var.print (State.peek 0 state);
      compile infos (pc + 1) (State.pop 1 state)
        (Let (x, Prim (Lt, [Pv y; Pv z])) :: instrs)
    | LEINT ->
      let y = State.accu state in
      let z = State.peek 0 state in
      let (x, state) = State.fresh_var state in
      if debug_parser () then Format.printf "%a = mk_bool(%a <= %a)@."
          Var.print x Var.print y Var.print z;
      compile infos (pc + 1) (State.pop 1 state)
        (Let (x, Prim (Le, [Pv y; Pv z])) :: instrs)
    | GTINT ->
      let y = State.accu state in
      let z = State.peek 0 state in
      let (x, state) = State.fresh_var state in
      if debug_parser () then Format.printf "%a = mk_bool(%a > %a)@."
          Var.print x Var.print y Var.print z;
      compile infos (pc + 1) (State.pop 1 state)
        (Let (x, Prim (Lt, [Pv z; Pv y])) :: instrs)
    | GEINT ->
      let y = State.accu state in
      let z = State.peek 0 state in
      let (x, state) = State.fresh_var state in
      if debug_parser () then Format.printf "%a = mk_bool(%a >= %a)@."
          Var.print x Var.print y Var.print z;
      compile infos (pc + 1) (State.pop 1 state)
        (Let (x, Prim (Le, [Pv z; Pv y])) :: instrs)
    | OFFSETINT ->
      let n = gets32 code (pc + 1) in
      let y = State.accu state in
      let (z, state) = State.fresh_var state in
      let (x, state) = State.fresh_var state in
      if debug_parser () then Format.printf "%a = %a + %ld@." Var.print x Var.print y n;
      compile infos (pc + 2) state
        (Let (x, Prim (Extern "%int_add", [Pv y; Pv z])) ::
         Let (z, Const n) :: instrs)
    | OFFSETREF ->
      let n = gets code (pc + 1) in
      let x = State.accu state in
      if debug_parser () then Format.printf "%a += %d@." Var.print x n;
      let instrs = Offset_ref (x, n) :: instrs in
      let (x, state) = State.fresh_var state in
      if debug_parser () then Format.printf "x = 0@.";
      compile infos (pc + 2) state (Let (x, Const 0l) :: instrs)
    | ISINT ->
      let y = State.accu state in
      let (x, state) = State.fresh_var state in
      if debug_parser () then Format.printf "%a = !%a@." Var.print x Var.print y;
      compile infos (pc + 1) state (Let (x, Prim (IsInt, [Pv y])) :: instrs)
    | BEQ ->
      let n = gets32 code (pc + 1) in
      let offset = gets code (pc + 2) in
      let x = State.accu state in
      let args = State.stack_vars state in
      (instrs,
       Cond (CEq n, x, (pc + offset + 2, args), (pc + 3, args)), state)
    | BNEQ ->
      let n = gets32 code (pc + 1) in
      let offset = gets code (pc + 2) in
      let x = State.accu state in
      let args = State.stack_vars state in
      (instrs,
       Cond (CEq n, x, (pc + 3, args), (pc + offset + 2, args)), state)
    | BLTINT ->
      let n = gets32 code (pc + 1) in
      let offset = gets code (pc + 2) in
      let x = State.accu state in
      let args = State.stack_vars state in
      (instrs,
       Cond (CLt n, x, (pc + offset + 2, args), (pc + 3, args)), state)
    | BLEINT ->
      let n = gets32 code (pc + 1) in
      let offset = gets code (pc + 2) in
      let x = State.accu state in
      let args = State.stack_vars state in
      (instrs,
       Cond (CLe n, x, (pc + offset + 2, args), (pc + 3, args)), state)
    | BGTINT ->
      let n = gets32 code (pc + 1) in
      let offset = gets code (pc + 2) in
      let x = State.accu state in
      let args = State.stack_vars state in
      (instrs,
       Cond (CLe n, x, (pc + 3, args), (pc + offset + 2, args)), state)
    | BGEINT ->
      let n = gets32 code (pc + 1) in
      let offset = gets code (pc + 2) in
      let x = State.accu state in
      let args = State.stack_vars state in
      (instrs,
       Cond (CLt n, x, (pc + 3, args), (pc + offset + 2, args)), state)
    | BULTINT ->
      let n = getu32 code (pc + 1) in
      let offset = gets code (pc + 2) in
      let x = State.accu state in
      let args = State.stack_vars state in
      (instrs,
       Cond (CUlt n, x, (pc + offset + 2, args), (pc + 3, args)), state)
    | BUGEINT ->
      let n = getu32 code (pc + 1) in
      let offset = gets code (pc + 2) in
      let x = State.accu state in
      let args = State.stack_vars state in
      (instrs,
       Cond (CUlt n, x, (pc + 3, args), (pc + offset + 2, args)), state)
    | ULTINT ->
      let y = State.accu state in
      let z = State.peek 0 state in
      let (x, state) = State.fresh_var state in
      if debug_parser () then Format.printf "%a = mk_bool(%a <= %a) (unsigned)@."
          Var.print x Var.print y Var.print z;
      compile infos (pc + 1) (State.pop 1 state)
        (Let (x, Prim (Ult, [Pv y; Pv z])) :: instrs)
    | UGEINT ->
      let y = State.accu state in
      let z = State.peek 0 state in
      let (x, state) = State.fresh_var state in
      if debug_parser () then Format.printf "%a = mk_bool(%a >= %a)@."
          Var.print x Var.print y Var.print z;
      compile infos (pc + 1) (State.pop 1 state)
        (Let (x, Prim (Ult, [Pv z; Pv y])) :: instrs)
    | GETPUBMET ->
      let n = gets32 code (pc + 1) in
      let cache = !method_cache_id in
      incr method_cache_id;
      let obj = State.accu state in
      let state = State.push state in
      let (tag, state) = State.fresh_var state in
      let (m, state) = State.fresh_var state in
      if debug_parser () then Format.printf "%a = %ld@." Var.print tag n;
      if debug_parser () then Format.printf "%a = caml_get_public_method(%a, %a)@."
          Var.print m Var.print obj Var.print tag;
      compile infos (pc + 3) state
        (Let (m, Prim (Extern "caml_get_public_method",
                       [Pv obj; Pv tag; Pc (Int (Int32.of_int cache))])) ::
         Let (tag, Const n) :: instrs)
    | GETDYNMET ->
      let tag = State.accu state in
      let obj = State.peek 0 state in
      let (m, state) = State.fresh_var state in
      if debug_parser () then Format.printf "%a = caml_get_public_method(%a, %a)@."
          Var.print m Var.print obj Var.print tag;
      compile infos (pc + 1) state
        (Let (m, Prim (Extern "caml_get_public_method",
                       [Pv obj; Pv tag; Pc (Int 0l)])) ::
         instrs)
    | GETMETHOD ->
      let lab = State.accu state in
      let obj = State.peek 0 state in
      let (meths, state) = State.fresh_var state in
      let (m, state) = State.fresh_var state in
      if debug_parser () then Format.printf "%a = lookup(%a, %a)@."
          Var.print m Var.print obj Var.print lab;
      compile infos (pc + 1) state
        (Let (m, Prim (Array_get, [Pv meths; Pv lab])) ::
         Let (meths, Field (obj, 0)) :: instrs)
    | STOP ->
      (instrs, Stop, state)
    | EVENT
    | BREAK
    | FIRST_UNIMPLEMENTED_OP -> assert false
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

let parse_bytecode ?(debug=`No) code globals debug_data =
  let state = State.initial globals in
  Code.Var.reset ();
  let blocks =
    Blocks.analyse
      (if debug = `Full then debug_data else Debug.no_data ()) code in
  let blocks =
    if debug = `Full
    then Debug.fold debug_data (fun pc _ blocks -> Blocks.add blocks pc) blocks
    else blocks in
  compile_block blocks debug_data code 0 state;

  let blocks =
    AddrMap.mapi
      (fun _ (state, instr, last) ->
         { params = State.stack_vars state;
           handler = State.current_handler state;
           body = instr; branch = last })
      !compiled_blocks
  in
  compiled_blocks := AddrMap.empty;
  tagged_blocks := AddrSet.empty;

  let free_pc = String.length code / 4 in
  let blocks = match_exn_traps (0, blocks, free_pc) in
  (0, blocks, free_pc)


(* HACK 1 - fix bytecode *)

let orig_code_bytes =
  [`I PUSHCONSTINT; `C 31;
   `I PUSHCONST1;
   `I LSLINT;
   `I BNEQ; `C 0; `C 5; (* overwrite from here *)
   `I CONSTINT; `C 30;
   `I BRANCH; `C 3;
   `I CONSTINT; `C 62;
   `I PUSHCONST1;
   `I LSLINT ]

let fixed_code_bytes =
  [`I CONSTINT; `C 31;
   `I BRANCH; `C 6;
   `I PUSHCONST1]

let orig_code = lazy (Instr.compile_to_string orig_code_bytes)
let fixed_code = lazy (Instr.compile_to_string fixed_code_bytes)

let fix_min_max_int code =
  begin
    try
      let i = Util.find (Lazy.force orig_code) code in
      String.blit (Lazy.force fixed_code) 0 code (i + 16) (String.length (Lazy.force fixed_code))
    with Not_found ->
      Util.warn
        "Warning: could not fix min_int/max_int definition \
         (bytecode not found).@."
  end

(* HACK 2 - override module *)

let override_global =
  let jsmodule name func =
    Prim(Extern "%overrideMod",[Pc (String name);Pc (String func)]) in
  [
    "CamlinternalMod",(fun _orig instrs ->
        let x = Var.fresh () in
        Var.name x "internalMod";
        let init_mod = Var.fresh () in
        let update_mod = Var.fresh () in
        x, Let(x,Block(0,[| init_mod; update_mod |]))::
           Let(init_mod,jsmodule "CamlinternalMod" "init_mod")::
           Let(update_mod,jsmodule "CamlinternalMod" "update_mod")::
           instrs)
  ]

(* HACK 3 - really input string *)

let really_input_string ic size =
  let b = Bytes.create size in
  really_input ic b 0 size;
  Bytes.unsafe_to_string b

let _ = really_input_string

let really_input_string = (* the one above or the one in Pervasives *)
  let open Pervasives in really_input_string

(* HACK END *)

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
  let header = really_input_string ic Util.MagicNumber.size in
  Util.MagicNumber.assert_current header;
  seek_in ic (pos_trailer - 8 * num_sections);
  let section_table = ref [] in
  for _i = 1 to num_sections do
    let name = really_input_string ic 4 in
    let len = input_binary_int ic in
    section_table := (name, len) :: !section_table
  done;
  !section_table

let from_channel ?(toplevel=false) ?(debug=`No) ic =

  let toc = read_toc ic in

  let prim_size = seek_section toc ic "PRIM" in
  let prim = really_input_string ic prim_size in
  let primitive_table = Array.of_list(Util.split_char '\000' prim) in

  let code_size = seek_section toc ic "CODE" in
  let code =
    match Util.Version.v with
    | `V3 ->
      let code = Bytes.create code_size in
      really_input ic code 0 code_size;
      (* We fix the bytecode to replace max_int/min_int *)
      fix_min_max_int code;
      Bytes.to_string code
    | `V4_02 -> really_input_string ic code_size in

  ignore(seek_section toc ic "DATA");
  let init_data = (input_value ic : Obj.t array) in

  ignore(seek_section toc ic "SYMB");
  let symbols = (input_value ic : Ident.t numtable) in

  ignore(seek_section toc ic "CRCS");
  let crcs = (input_value ic : (string * Digest.t option) list) in

  let debug_data =
    if debug = `No then
      Debug.no_data ()
    else
      try
        ignore(seek_section toc ic "DBUG");
        let debug = Debug.read ~crcs ic in
	debug
      with Not_found ->
        Debug.no_data ()
  in

  let globals = make_globals (Array.length init_data) init_data primitive_table in

  (* Initialize module override mechanism *)
  List.iter (fun (name, v) ->
      try
        let nn = { Ident.stamp= 0; name; flags= 0 } in
        let i = Tbl.find (fun x1 x2 -> String.compare x1.Ident.name x2.Ident.name) nn symbols.num_tbl in
        globals.override.(i) <- Some v;
        if debug_parser () then Format.eprintf "overriding global %s@." name
      with Not_found -> ()
    ) override_global;

  if toplevel then
    begin
      (* export globals *)
      Tbl.iter (fun _ n -> globals.is_exported.(n) <- true) symbols.num_tbl;
      (* @vouillon: *)
      (* we should then use the -linkall option to build the toplevel. *)
      (* The OCaml compiler can generate code using this primitive but *)
      (* does not use it itself. This is the only primitive in this case. *)
      (* Ideally, Js_of_ocaml should parse the .mli files for primitives as *)
      (* well as marking this primitive as potentially used. But *)
      (* the -linkall option is probably good enough. *)
      (* Primitive.mark_used "caml_string_greaterthan" *)
    end;

  let p = parse_bytecode ~debug code globals debug_data in

  (* register predefined exception *)
  let body = ref [] in
  for i = 0 to 11 do (* see ocaml/byterun/fail.h *)
    body := register_global ~force:true globals i !body;
    globals.is_exported.(i) <- false;
  done;
  let body = Util.array_fold_right_i (fun i _ l ->
      match globals.vars.(i) with
        Some x when globals.is_const.(i) ->
        let l = register_global globals i l in
        Let (x, Constant (Constants.parse globals.constants.(i))) :: l
      | _ -> l) globals.constants !body in


  let body =
    if toplevel
    then
      begin
        (* Include linking information *)
        let toc = [
          ("SYMB", Obj.repr symbols);
          ("CRCS", Obj.repr crcs);
          ("PRIM", Obj.repr prim)
        ] in
        let gdata = Var.fresh () in
        let infos = [
          "toc",(Constants.parse (Obj.repr toc));
          "prim_count",(Int (Int32.of_int (Array.length globals.primitives)))] in
        let body = List.fold_left (fun rem (name,const) ->
            let c = Var.fresh () in
            Let (c, Constant const) ::
            Let (Var.fresh (),
                 Prim (Extern "caml_js_set", [Pv gdata; Pc (String name); Pv c])) ::
            rem) body infos in
        Let (gdata, Prim (Extern "caml_get_global_data", [])) :: body
      end
    else body in

  (* List interface files *)
  let cmis =
    if toplevel && Option.Optim.include_cmis ()
    then Tbl.fold (fun id _num acc ->
        if id.Ident.flags = 1
        then Util.StringSet.add id.Ident.name acc
        else acc) symbols.num_tbl Util.StringSet.empty
    else Util.StringSet.empty in
  prepend p body, cmis, debug_data

(* As input: list of primitives + size of global table *)
let from_bytes primitives (code : code) =
  let globals = make_globals 0 [||] primitives in
  let debug_data = Debug.no_data () in
  let p = parse_bytecode code globals debug_data in

  let gdata = Var.fresh () in
  let body = Util.array_fold_right_i (fun i var l ->
      match var with
      | Some x when globals.is_const.(i) ->
        Let (x, Field (gdata, i)) :: l
      | _ -> l) globals.vars [] in
  let body = Let (gdata, Prim (Extern "caml_get_global_data", [])) :: body in
  prepend p body, debug_data

let from_string primitives (code : string) =
  from_bytes primitives code
