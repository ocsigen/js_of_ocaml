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

open! Stdlib
open Code
open Instr

let debug_parser = Debug.find "parser"

let debug_sourcemap = Debug.find "sourcemap"

type bytecode = string

let predefined_exceptions =
  [ 0, "Out_of_memory"
  ; 1, "Sys_error"
  ; 2, "Failure"
  ; 3, "Invalid_argument"
  ; 4, "End_of_file"
  ; 5, "Division_by_zero"
  ; 6, "Not_found"
  ; 7, "Match_failure"
  ; 8, "Stack_overflow"
  ; 9, "Sys_blocked_io"
  ; 10, "Assert_failure"
  ; 11, "Undefined_recursive_module"
  ]

(* Read and manipulate debug section *)
module Debug : sig
  type t

  val names : t -> bool

  val toplevel : t -> bool

  val enabled : t -> bool

  val is_empty : t -> bool

  val dbg_section_needed : t -> bool

  val propagate : Code.Var.t list -> Code.Var.t list -> unit

  val find : t -> Code.Addr.t -> (int * string * Ident.t) list * Env.summary

  val find_loc : t -> ?after:bool -> int -> Parse_info.t option

  val find_source : t -> string -> string option

  val mem : t -> Code.Addr.t -> bool

  val read :
    t -> crcs:(string * string option) list -> includes:string list -> in_channel -> unit

  val read_event_list :
       t
    -> crcs:(string * string option) list
    -> includes:string list
    -> orig:int
    -> in_channel
    -> unit

  val create : toplevel:bool -> bool -> t

  val fold : t -> (Code.Addr.t -> Instruct.debug_event -> 'a -> 'a) -> 'a -> 'a

  val paths : t -> units:StringSet.t -> StringSet.t
end = struct
  open Instruct

  type ml_unit =
    { module_name : string
    ; fname : string
    ; crc : string option
    ; paths : string list
    ; source : string option
    }

  module String_table = Hashtbl.Make (String)
  module Int_table = Hashtbl.Make (Int)

  type t =
    { events_by_pc : (debug_event * ml_unit) Int_table.t
    ; units : (string * string, ml_unit) Hashtbl.t
    ; pos_fname_to_source : string String_table.t
    ; toplevel : bool
    ; names : bool
    ; enabled : bool
    }

  let names t = t.names

  let toplevel t = t.toplevel

  let enabled t = t.enabled

  let dbg_section_needed t = t.names || t.toplevel || t.enabled

  let relocate_event orig ev = ev.ev_pos <- (orig + ev.ev_pos) / 4

  let create ~toplevel enabled =
    let names = enabled || Config.Flag.pretty () in
    { events_by_pc = Int_table.create 17
    ; units = Hashtbl.create 17
    ; pos_fname_to_source = String_table.create 17
    ; names
    ; toplevel
    ; enabled
    }

  let is_empty t = Int_table.length t.events_by_pc = 0

  let find_ml_in_paths paths name =
    let uname = String.uncapitalize_ascii name in
    try Some (Fs.find_in_path paths (uname ^ ".ml"))
    with Not_found -> (
      try Some (Fs.find_in_path paths (name ^ ".ml")) with Not_found -> None)

  let read_event_list =
    let rewrite_path path =
      if Filename.is_relative path
      then path
      else
        match Build_path_prefix_map.get_build_path_prefix_map () with
        | Some map -> Build_path_prefix_map.rewrite (Build_path_prefix_map.flip map) path
        | None -> path
    in
    let read_paths ic : string list = List.map (input_value ic) ~f:rewrite_path in
    fun { events_by_pc; units; pos_fname_to_source; toplevel = _; names; enabled }
        ~crcs
        ~includes
        ~orig
        ic ->
      let crcs =
        let t = Hashtbl.create 17 in
        List.iter crcs ~f:(fun (m, crc) -> Hashtbl.add t m crc);
        t
      in
      let evl : debug_event list = input_value ic in
      let paths = read_paths ic @ includes in
      List.iter
        evl
        ~f:(fun
             ({ ev_module
              ; ev_loc = { Location.loc_start = { Lexing.pos_fname; _ }; _ }
              ; _
              } as ev)
           ->
          let unit =
            try Hashtbl.find units (ev_module, pos_fname)
            with Not_found ->
              let crc = try Hashtbl.find crcs ev_module with Not_found -> None in
              let source =
                try Some (Fs.find_in_path paths pos_fname)
                with Not_found -> (
                  try Some (Fs.find_in_path paths (Filename.basename pos_fname))
                  with Not_found -> find_ml_in_paths paths ev_module)
              in
              let source =
                match source with
                | None -> None
                | Some source -> Some (Fs.absolute_path source)
              in
              if debug_sourcemap ()
              then
                Format.eprintf
                  "module:%s - source:%s - name:%s\n%!"
                  ev_module
                  (match source with
                  | None -> "NONE"
                  | Some x -> x)
                  pos_fname;
              let u =
                { module_name = ev_module; fname = pos_fname; crc; source; paths }
              in
              (match pos_fname, source with
              | "_none_", _ | _, None -> ()
              | pos_fname, Some source ->
                  String_table.add pos_fname_to_source pos_fname source);
              Hashtbl.add units (ev_module, pos_fname) u;
              u
          in
          relocate_event orig ev;
          if enabled || names then Int_table.add events_by_pc ev.ev_pos (ev, unit);
          ())

  let find_source { pos_fname_to_source; _ } pos_fname =
    match String_table.find_all pos_fname_to_source pos_fname with
    | [ x ] -> Some x
    | [] | _ :: _ :: _ -> None

  let read t ~crcs ~includes ic =
    let len = input_binary_int ic in
    for _i = 0 to len - 1 do
      let orig = input_binary_int ic in
      read_event_list t ~crcs ~includes ~orig ic
    done

  let find { events_by_pc; _ } pc =
    try
      let ev, _ = Int_table.find events_by_pc pc in
      ( Ocaml_compiler.Ident.table_contents ev.ev_stacksize ev.ev_compenv.ce_stack
      , ev.ev_typenv )
    with Not_found -> [], Env.Env_empty

  let mem { events_by_pc; _ } = Int_table.mem events_by_pc

  let find_loc { events_by_pc; _ } ?(after = false) pc =
    try
      let before, (ev, unit) =
        try false, Int_table.find events_by_pc pc
        with Not_found -> (
          ( true
          , try Int_table.find events_by_pc (pc + 1)
            with Not_found -> (
              try Int_table.find events_by_pc (pc + 2)
              with Not_found -> Int_table.find events_by_pc (pc + 3)) ))
      in
      let loc = ev.ev_loc in
      if loc.Location.loc_ghost
      then None
      else
        let pos =
          if after
          then loc.Location.loc_end
          else if before
          then loc.Location.loc_start
          else
            match ev.ev_kind with
            | Event_after _ -> loc.Location.loc_end
            | _ -> loc.Location.loc_start
        in
        let src = unit.source in
        Some (Parse_info.t_of_position ~src pos)
    with Not_found -> None

  let rec propagate l1 l2 =
    match l1, l2 with
    | v1 :: r1, v2 :: r2 ->
        Var.propagate_name v1 v2;
        propagate r1 r2
    | _ -> ()

  let fold t f acc = Int_table.fold (fun k (e, _u) acc -> f k e acc) t.events_by_pc acc

  let paths t ~units =
    let paths =
      Hashtbl.fold
        (fun _ u acc -> if StringSet.mem u.module_name units then u.paths :: acc else acc)
        t.units
        []
    in
    StringSet.of_list (List.concat paths)
end

(* Block analysis *)
(* Detect each block *)
module Blocks : sig
  type t

  val analyse : Debug.t -> bytecode -> t

  val add : t -> int -> t

  type u

  val finish_analysis : t -> u

  val next : u -> int -> int

  val is_empty : u -> bool
end = struct
  type t = Addr.Set.t

  type u = int array

  let add blocks pc = Addr.Set.add pc blocks

  let rec scan debug blocks code pc len =
    if pc < len
    then
      match (get_instr_exn code pc).kind with
      | KNullary -> scan debug blocks code (pc + 1) len
      | KUnary -> scan debug blocks code (pc + 2) len
      | KBinary -> scan debug blocks code (pc + 3) len
      | KNullaryCall ->
          let blocks =
            if Debug.mem debug (pc + 1) then Addr.Set.add pc blocks else blocks
          in
          scan debug blocks code (pc + 1) len
      | KUnaryCall ->
          let blocks =
            if Debug.mem debug (pc + 2) then Addr.Set.add pc blocks else blocks
          in
          scan debug blocks code (pc + 2) len
      | KBinaryCall ->
          let blocks =
            if Debug.mem debug (pc + 3) then Addr.Set.add pc blocks else blocks
          in
          scan debug blocks code (pc + 3) len
      | KJump ->
          let offset = gets code (pc + 1) in
          let blocks = Addr.Set.add (pc + offset + 1) blocks in
          scan debug blocks code (pc + 2) len
      | KCond_jump ->
          let offset = gets code (pc + 1) in
          let blocks = Addr.Set.add (pc + offset + 1) blocks in
          scan debug blocks code (pc + 2) len
      | KCmp_jump ->
          let offset = gets code (pc + 2) in
          let blocks = Addr.Set.add (pc + offset + 2) blocks in
          scan debug blocks code (pc + 3) len
      | KSwitch ->
          let sz = getu code (pc + 1) in
          let blocks = ref blocks in
          for i = 0 to (sz land 0xffff) + (sz lsr 16) - 1 do
            let offset = gets code (pc + 2 + i) in
            blocks := Addr.Set.add (pc + offset + 2) !blocks
          done;
          scan debug !blocks code (pc + 2 + (sz land 0xffff) + (sz lsr 16)) len
      | KClosurerec ->
          let nfuncs = getu code (pc + 1) in
          scan debug blocks code (pc + nfuncs + 3) len
      | KClosure -> scan debug blocks code (pc + 3) len
      | KStop n -> scan debug blocks code (pc + n + 1) len
      | K_will_not_happen -> assert false
    else (
      assert (pc = len);
      blocks)

  let finish_analysis blocks = Array.of_list (Addr.Set.elements blocks)

  (* invariant: a.(i) <= x < a.(j) *)
  let rec find a i j x =
    assert (i < j);
    if i + 1 = j
    then a.(j)
    else
      let k = (i + j) / 2 in
      if a.(k) <= x then find a k j x else find a i k x

  let next blocks pc = find blocks 0 (Array.length blocks - 1) pc

  let is_empty x = Array.length x <= 1

  let analyse debug_data code =
    let debug_data =
      if Debug.enabled debug_data then debug_data else Debug.create ~toplevel:false false
    in
    let blocks = Addr.Set.empty in
    let len = String.length code / 4 in
    let blocks = add blocks 0 in
    let blocks = add blocks len in
    scan debug_data blocks code 0 len
end

(* Parse constants *)
module Constants : sig
  val parse : Obj.t -> Code.constant

  val inlined : Code.constant -> bool
end = struct
  (* In order to check that two custom objects share the same kind, we
     compare their identifier.  The identifier is currently extracted
     from the marshaled value. *)
  let ident_of_custom x =
    (* Make sure tags are equal to custom_tag.
       Note that in javascript [0l] and [0n] are not encoded as custom blocks. *)
    if Obj.tag x <> Obj.custom_tag
    then None
    else
      try
        let bin = Marshal.to_string x [] in
        match Char.code bin.[20] with
        | 0x12 | 0x18 | 0x19 ->
            let last = String.index_from bin 21 '\000' in
            let name = String.sub bin ~pos:21 ~len:(last - 21) in
            Some name
        | _ -> assert false
      with _ -> assert false

  let same_ident x y =
    match y with
    | Some y -> String.equal x y
    | None -> false

  let ident_32 = ident_of_custom (Obj.repr 0l)

  let ident_64 = ident_of_custom (Obj.repr 0L)

  let ident_native = ident_of_custom (Obj.repr 0n)

  let rec parse x =
    if Obj.is_block x
    then
      let tag = Obj.tag x in
      if tag = Obj.string_tag
      then String (Obj.magic x : string)
      else if tag = Obj.double_tag
      then Float (Obj.magic x : float)
      else if tag = Obj.double_array_tag
      then Float_array (Array.init (Obj.size x) ~f:(fun i -> Obj.double_field x i))
      else if tag = Obj.custom_tag
      then
        match ident_of_custom x with
        | Some name when same_ident name ident_32 -> Int (Obj.magic x : int32)
        | Some name when same_ident name ident_native ->
            let i : nativeint = Obj.magic x in
            Int (Int32.of_nativeint_warning_on_overflow i)
        | Some name when same_ident name ident_64 -> Int64 (Obj.magic x : int64)
        | Some name ->
            failwith
              (Printf.sprintf
                 "parse_bytecode: Don't know what to do with custom block (%s)"
                 name)
        | None -> assert false
      else if tag < Obj.no_scan_tag
      then
        Tuple (tag, Array.init (Obj.size x) ~f:(fun i -> parse (Obj.field x i)), Unknown)
      else assert false
    else
      let i : int = Obj.magic x in
      Int (Int32.of_int_warning_on_overflow i)

  let inlined = function
    | String _ | IString _ -> false
    | Float _ -> true
    | Float_array _ -> false
    | Int64 _ -> false
    | Tuple _ -> false
    | Int _ -> true
end

let const i = Constant (Int i)

(* Globals *)
type globals =
  { mutable vars : Var.t option array
  ; mutable is_const : bool array
  ; mutable is_exported : bool array
  ; mutable named_value : string option array
  ; mutable override : (Var.t -> Code.instr list -> Var.t * Code.instr list) option array
  ; constants : Code.constant array
  ; primitives : string array
  }

let make_globals size constants primitives =
  { vars = Array.make size None
  ; is_const = Array.make size false
  ; is_exported = Array.make size false
  ; named_value = Array.make size None
  ; override = Array.make size None
  ; constants
  ; primitives
  }

let resize_array a len def =
  let b = Array.make len def in
  Array.blit ~src:a ~src_pos:0 ~dst:b ~dst_pos:0 ~len:(Array.length a);
  b

let resize_globals g size =
  g.vars <- resize_array g.vars size None;
  g.is_const <- resize_array g.is_const size false;
  g.is_exported <- resize_array g.is_exported size true;
  g.named_value <- resize_array g.named_value size None;
  g.override <- resize_array g.override size None

(* State of the VM *)
module State = struct
  type elt =
    | Var of Var.t
    | Dummy

  let elt_to_var e =
    match e with
    | Var x -> x
    | _ -> assert false

  let print_elt f v =
    match v with
    | Var x -> Format.fprintf f "%a" Var.print x
    | Dummy -> Format.fprintf f "???"

  type handler =
    { var : Var.t
    ; addr : Addr.t
    ; stack_len : int
    ; block_pc : Addr.t
    }

  type t =
    { accu : elt
    ; stack : elt list
    ; env : elt array
    ; env_offset : int
    ; handlers : handler list
    ; globals : globals
    ; current_pc : Addr.t
    }

  let fresh_var state =
    let x = Var.fresh () in
    x, { state with accu = Var x }

  let globals st = st.globals

  let size_globals st size =
    if size > Array.length st.globals.vars then resize_globals st.globals size

  let rec list_start n l =
    if n = 0
    then []
    else
      match l with
      | [] -> assert false
      | v :: r -> v :: list_start (n - 1) r

  let rec st_pop n st =
    if n = 0
    then st
    else
      match st with
      | [] -> assert false
      | _ :: r -> st_pop (n - 1) r

  let push st = { st with stack = st.accu :: st.stack }

  let pop n st = { st with stack = st_pop n st.stack }

  let acc n st = { st with accu = List.nth st.stack n }

  let env_acc n st = { st with accu = st.env.(st.env_offset + n) }

  let accu st = elt_to_var st.accu

  let stack_vars st =
    List.fold_left (st.accu :: st.stack) ~init:[] ~f:(fun l e ->
        match e with
        | Var x -> x :: l
        | Dummy -> l)

  let set_accu st x = { st with accu = Var x }

  let clear_accu st = { st with accu = Dummy }

  let peek n st = elt_to_var (List.nth st.stack n)

  let grab n st = List.map (list_start n st.stack) ~f:elt_to_var, pop n st

  let rec st_assign s n x =
    match s with
    | [] -> assert false
    | y :: rem -> if n = 0 then x :: rem else y :: st_assign rem (n - 1) x

  let assign st n = { st with stack = st_assign st.stack n st.accu }

  let start_function state env offset =
    { state with accu = Dummy; stack = []; env; env_offset = offset; handlers = [] }

  let start_block current_pc state =
    let stack =
      List.fold_right state.stack ~init:[] ~f:(fun e stack ->
          match e with
          | Dummy -> Dummy :: stack
          | Var x ->
              let y = Var.fork x in
              Var y :: stack)
    in
    let state = { state with stack; current_pc } in
    match state.accu with
    | Dummy -> state
    | Var x ->
        let y, state = fresh_var state in
        Var.propagate_name x y;
        state

  let push_handler state x addr =
    { state with
      handlers =
        { block_pc = state.current_pc
        ; var = x
        ; addr
        ; stack_len = List.length state.stack
        }
        :: state.handlers
    }

  let pop_handler state = { state with handlers = List.tl state.handlers }

  let addr_of_current_handler state =
    match state.handlers with
    | [] -> assert false
    | x :: _ -> x.block_pc

  let current_handler state =
    match state.handlers with
    | [] -> None
    | { var; addr; stack_len; _ } :: _ ->
        let state =
          { state with
            accu = Var var
          ; stack = st_pop (List.length state.stack - stack_len) state.stack
          }
        in
        Some (var, (addr, stack_vars state))

  let initial g =
    { accu = Dummy
    ; stack = []
    ; env = [||]
    ; env_offset = 0
    ; handlers = []
    ; globals = g
    ; current_pc = -1
    }

  let rec print_stack f l =
    match l with
    | [] -> ()
    | v :: r -> Format.fprintf f "%a %a" print_elt v print_stack r

  let print_env f e =
    Array.iteri e ~f:(fun i v ->
        if i > 0 then Format.fprintf f " ";
        Format.fprintf f "%a" print_elt v)

  let print st =
    Format.eprintf
      "{ %a | %a | (%d) %a }@."
      print_elt
      st.accu
      print_stack
      st.stack
      st.env_offset
      print_env
      st.env

  let pi_of_loc debug location =
    let pos = location.Location.loc_start in
    let src = Debug.find_source debug pos.Lexing.pos_fname in
    Parse_info.t_of_position ~src pos

  let rec name_rec debug i l s summary =
    match l, s with
    | [], _ -> ()
    | (j, nm, ident) :: lrem, Var v :: srem when i = j ->
        (match Ocaml_compiler.find_loc_in_summary ident summary with
        | None -> ()
        | Some loc -> Var.loc v (pi_of_loc debug loc));
        Var.name v nm;
        name_rec debug (i + 1) lrem srem summary
    | (j, _, _) :: _, _ :: srem when i < j -> name_rec debug (i + 1) l srem summary
    | _ -> assert false

  let name_vars st debug pc =
    if Debug.names debug
    then
      let l, summary = Debug.find debug pc in
      name_rec debug 0 l st.stack summary

  let rec make_stack i state =
    if i = 0
    then [], state
    else
      let x, state = fresh_var state in
      let params, state = make_stack (pred i) (push state) in
      if debug_parser () then if i > 1 then Format.printf ", ";
      if debug_parser () then Format.printf "%a" Var.print x;
      x :: params, state
end

let primitive_name state i =
  let g = State.globals state in
  assert (i >= 0 && i <= Array.length g.primitives);
  let prim = g.primitives.(i) in
  Primitive.add_external prim;
  prim

let access_global g i =
  match g.vars.(i) with
  | Some x -> x
  | None ->
      g.is_const.(i) <- true;
      let x = Var.fresh () in
      g.vars.(i) <- Some x;
      x

let register_global ?(force = false) g i rem =
  if force || g.is_exported.(i)
  then
    let args =
      match g.named_value.(i) with
      | None -> []
      | Some name ->
          Code.Var.name (access_global g i) name;
          [ Pc (IString name) ]
    in
    Let
      ( Var.fresh ()
      , Prim
          ( Extern "caml_register_global"
          , Pc (Int (Int32.of_int i)) :: Pv (access_global g i) :: args ) )
    :: rem
  else rem

let get_global state instrs i =
  State.size_globals state (i + 1);
  let g = State.globals state in
  match g.vars.(i) with
  | Some x ->
      if debug_parser () then Format.printf "(global access %a)@." Var.print x;
      x, State.set_accu state x, instrs
  | None ->
      if i < Array.length g.constants && Constants.inlined g.constants.(i)
      then
        let x, state = State.fresh_var state in
        let cst = g.constants.(i) in
        x, state, Let (x, Constant cst) :: instrs
      else (
        g.is_const.(i) <- true;
        let x, state = State.fresh_var state in
        if debug_parser () then Format.printf "%a = CONST(%d)@." Var.print x i;
        g.vars.(i) <- Some x;
        x, state, instrs)

let tagged_blocks = ref Addr.Set.empty

let compiled_blocks = ref Addr.Map.empty

let method_cache_id = ref 1

let new_closure_repr =
  match Ocaml_version.v with
  | `V4_02 | `V4_03 | `V4_04 | `V4_06 | `V4_07 | `V4_08 | `V4_09 | `V4_10 | `V4_11 ->
      false
  | `V4_12 | `V4_13 -> true

let clo_offset_3 = if new_closure_repr then 3 else 2

type compile_info =
  { blocks : Blocks.u
  ; code : string
  ; limit : int
  ; debug : Debug.t
  }

let rec compile_block blocks debug_data code pc state =
  if not (Addr.Set.mem pc !tagged_blocks)
  then (
    let limit = Blocks.next blocks pc in
    assert (limit > pc);
    let string_of_addr addr =
      match Debug.find_loc debug_data addr with
      | None -> string_of_int addr
      | Some loc -> (
          match loc.Parse_info.src with
          | None -> string_of_int addr
          | Some file ->
              Printf.sprintf
                "%s:%d:%d-%d"
                file
                loc.Parse_info.line
                loc.Parse_info.col
                (addr + 2))
    in
    if debug_parser ()
    then Format.eprintf "Compiling from %s to %d@." (string_of_addr pc) (limit - 1);
    let state = State.start_block pc state in
    tagged_blocks := Addr.Set.add pc !tagged_blocks;
    let instr, last, state' =
      compile { blocks; code; limit; debug = debug_data } pc state []
    in
    assert (not (Addr.Map.mem pc !compiled_blocks));
    compiled_blocks := Addr.Map.add pc (state, List.rev instr, last) !compiled_blocks;
    match last with
    | Branch (pc', _) | Poptrap ((pc', _), _) ->
        compile_block blocks debug_data code pc' state'
    | Cond (_, (pc1, _), (pc2, _)) ->
        compile_block blocks debug_data code pc1 state';
        compile_block blocks debug_data code pc2 state'
    | Switch (_, l1, l2) ->
        Array.iter l1 ~f:(fun (pc', _) -> compile_block blocks debug_data code pc' state');
        Array.iter l2 ~f:(fun (pc', _) -> compile_block blocks debug_data code pc' state')
    | Pushtrap _ | Raise _ | Return _ | Stop -> ())

and compile infos pc state instrs =
  if debug_parser () then State.print state;
  assert (pc <= infos.limit);
  if pc = infos.limit
  then
    if (* stop if we reach end_of_code (ie when compiling cmo) *)
       pc = String.length infos.code / 4
    then (
      if debug_parser () then Format.eprintf "Stop@.";
      instrs, Stop, state)
    else (
      State.name_vars state infos.debug pc;
      let stack = State.stack_vars state in
      if debug_parser () then Format.eprintf "Branch %d (%a) @." pc Print.var_list stack;
      instrs, Branch (pc, stack), state)
  else (
    if debug_parser () then Format.eprintf "%4d " pc;
    State.name_vars state infos.debug pc;
    let code = infos.code in
    let instr = get_instr_exn code pc in
    if debug_parser () then Format.eprintf "%08x %s@." instr.opcode instr.name;
    match instr.Instr.code with
    | ACC0 -> compile infos (pc + 1) (State.acc 0 state) instrs
    | ACC1 -> compile infos (pc + 1) (State.acc 1 state) instrs
    | ACC2 -> compile infos (pc + 1) (State.acc 2 state) instrs
    | ACC3 -> compile infos (pc + 1) (State.acc 3 state) instrs
    | ACC4 -> compile infos (pc + 1) (State.acc 4 state) instrs
    | ACC5 -> compile infos (pc + 1) (State.acc 5 state) instrs
    | ACC6 -> compile infos (pc + 1) (State.acc 6 state) instrs
    | ACC7 -> compile infos (pc + 1) (State.acc 7 state) instrs
    | ACC ->
        let n = getu code (pc + 1) in
        compile infos (pc + 2) (State.acc n state) instrs
    | PUSH -> compile infos (pc + 1) (State.push state) instrs
    | PUSHACC0 -> compile infos (pc + 1) (State.acc 0 (State.push state)) instrs
    | PUSHACC1 -> compile infos (pc + 1) (State.acc 1 (State.push state)) instrs
    | PUSHACC2 -> compile infos (pc + 1) (State.acc 2 (State.push state)) instrs
    | PUSHACC3 -> compile infos (pc + 1) (State.acc 3 (State.push state)) instrs
    | PUSHACC4 -> compile infos (pc + 1) (State.acc 4 (State.push state)) instrs
    | PUSHACC5 -> compile infos (pc + 1) (State.acc 5 (State.push state)) instrs
    | PUSHACC6 -> compile infos (pc + 1) (State.acc 6 (State.push state)) instrs
    | PUSHACC7 -> compile infos (pc + 1) (State.acc 7 (State.push state)) instrs
    | PUSHACC ->
        let n = getu code (pc + 1) in
        compile infos (pc + 2) (State.acc n (State.push state)) instrs
    | POP ->
        let n = getu code (pc + 1) in
        compile infos (pc + 2) (State.pop n state) instrs
    | ASSIGN ->
        let n = getu code (pc + 1) in
        let state = State.assign state n in
        let x, state = State.fresh_var state in
        if debug_parser () then Format.printf "%a = 0@." Var.print x;
        (* We switch to a different block as this may have
           changed the exception handler continuation *)
        compile_block infos.blocks infos.debug code (pc + 2) state;
        Let (x, const 0l) :: instrs, Branch (pc + 2, State.stack_vars state), state
    | ENVACC1 -> compile infos (pc + 1) (State.env_acc 1 state) instrs
    | ENVACC2 -> compile infos (pc + 1) (State.env_acc 2 state) instrs
    | ENVACC3 -> compile infos (pc + 1) (State.env_acc 3 state) instrs
    | ENVACC4 -> compile infos (pc + 1) (State.env_acc 4 state) instrs
    | ENVACC ->
        let n = getu code (pc + 1) in
        compile infos (pc + 2) (State.env_acc n state) instrs
    | PUSHENVACC1 -> compile infos (pc + 1) (State.env_acc 1 (State.push state)) instrs
    | PUSHENVACC2 -> compile infos (pc + 1) (State.env_acc 2 (State.push state)) instrs
    | PUSHENVACC3 -> compile infos (pc + 1) (State.env_acc 3 (State.push state)) instrs
    | PUSHENVACC4 -> compile infos (pc + 1) (State.env_acc 4 (State.push state)) instrs
    | PUSHENVACC ->
        let n = getu code (pc + 1) in
        compile infos (pc + 2) (State.env_acc n (State.push state)) instrs
    | PUSH_RETADDR ->
        compile
          infos
          (pc + 2)
          { state with
            State.stack =
              (* See interp.c *)
              State.Dummy :: State.Dummy :: State.Dummy :: state.State.stack
          }
          instrs
    | APPLY ->
        let n = getu code (pc + 1) in
        let f = State.accu state in
        let x, state = State.fresh_var state in
        let args, state = State.grab n state in
        if debug_parser ()
        then (
          Format.printf "%a = %a(" Var.print x Var.print f;
          for i = 0 to n - 1 do
            if i > 0 then Format.printf ", ";
            Format.printf "%a" Var.print (List.nth args i)
          done;
          Format.printf ")@.");
        compile
          infos
          (pc + 2)
          (State.pop 3 state)
          (Let (x, Apply (f, args, false)) :: instrs)
    | APPLY1 ->
        let f = State.accu state in
        let x, state = State.fresh_var state in
        let y = State.peek 0 state in
        if debug_parser ()
        then Format.printf "%a = %a(%a)@." Var.print x Var.print f Var.print y;
        compile
          infos
          (pc + 1)
          (State.pop 1 state)
          (Let (x, Apply (f, [ y ], false)) :: instrs)
    | APPLY2 ->
        let f = State.accu state in
        let x, state = State.fresh_var state in
        let y = State.peek 0 state in
        let z = State.peek 1 state in
        if debug_parser ()
        then
          Format.printf
            "%a = %a(%a, %a)@."
            Var.print
            x
            Var.print
            f
            Var.print
            y
            Var.print
            z;
        compile
          infos
          (pc + 1)
          (State.pop 2 state)
          (Let (x, Apply (f, [ y; z ], false)) :: instrs)
    | APPLY3 ->
        let f = State.accu state in
        let x, state = State.fresh_var state in
        let y = State.peek 0 state in
        let z = State.peek 1 state in
        let t = State.peek 2 state in
        if debug_parser ()
        then
          Format.printf
            "%a = %a(%a, %a, %a)@."
            Var.print
            x
            Var.print
            f
            Var.print
            y
            Var.print
            z
            Var.print
            t;
        compile
          infos
          (pc + 1)
          (State.pop 3 state)
          (Let (x, Apply (f, [ y; z; t ], false)) :: instrs)
    | APPTERM ->
        let n = getu code (pc + 1) in
        let f = State.accu state in
        let l, state = State.grab n state in
        if debug_parser ()
        then (
          Format.printf "return %a(" Var.print f;
          for i = 0 to n - 1 do
            if i > 0 then Format.printf ", ";
            Format.printf "%a" Var.print (List.nth l i)
          done;
          Format.printf ")@.");
        let x, state = State.fresh_var state in
        Let (x, Apply (f, l, false)) :: instrs, Return x, state
    | APPTERM1 ->
        let f = State.accu state in
        let x = State.peek 0 state in
        if debug_parser () then Format.printf "return %a(%a)@." Var.print f Var.print x;
        let y, state = State.fresh_var state in
        Let (y, Apply (f, [ x ], false)) :: instrs, Return y, state
    | APPTERM2 ->
        let f = State.accu state in
        let x = State.peek 0 state in
        let y = State.peek 1 state in
        if debug_parser ()
        then Format.printf "return %a(%a, %a)@." Var.print f Var.print x Var.print y;
        let z, state = State.fresh_var state in
        Let (z, Apply (f, [ x; y ], false)) :: instrs, Return z, state
    | APPTERM3 ->
        let f = State.accu state in
        let x = State.peek 0 state in
        let y = State.peek 1 state in
        let z = State.peek 2 state in
        if debug_parser ()
        then
          Format.printf
            "return %a(%a, %a, %a)@."
            Var.print
            f
            Var.print
            x
            Var.print
            y
            Var.print
            z;
        let t, state = State.fresh_var state in
        Let (t, Apply (f, [ x; y; z ], false)) :: instrs, Return t, state
    | RETURN ->
        let x = State.accu state in
        if debug_parser () then Format.printf "return %a@." Var.print x;
        instrs, Return x, state
    | RESTART -> assert false
    | GRAB -> compile infos (pc + 2) state instrs
    | CLOSURE ->
        let nvars = getu code (pc + 1) in
        let addr = pc + gets code (pc + 2) + 2 in
        let state = if nvars > 0 then State.push state else state in
        let vals, state = State.grab nvars state in
        let x, state = State.fresh_var state in
        let env = List.map vals ~f:(fun x -> State.Var x) in
        let env =
          let code = State.Dummy in
          let closure_info = State.Dummy in
          if new_closure_repr then code :: closure_info :: env else code :: env
        in
        let env = Array.of_list env in
        if debug_parser () then Format.printf "fun %a (" Var.print x;
        let nparams =
          match (get_instr_exn code addr).Instr.code with
          | GRAB -> getu code (addr + 1) + 1
          | _ -> 1
        in
        let state' = State.start_function state env 0 in
        let params, state' = State.make_stack nparams state' in
        if debug_parser () then Format.printf ") {@.";
        let state' = State.clear_accu state' in
        compile_block infos.blocks infos.debug code addr state';
        if debug_parser () then Format.printf "}@.";
        let args = State.stack_vars state' in
        let state'', _, _ = Addr.Map.find addr !compiled_blocks in
        Debug.propagate (State.stack_vars state'') args;
        compile
          infos
          (pc + 3)
          state
          (Let (x, Closure (List.rev params, (addr, args))) :: instrs)
    | CLOSUREREC ->
        let nfuncs = getu code (pc + 1) in
        let nvars = getu code (pc + 2) in
        let state = if nvars > 0 then State.push state else state in
        let vals, state = State.grab nvars state in
        let state = ref state in
        let vars = ref [] in
        for i = 0 to nfuncs - 1 do
          let x, st = State.fresh_var !state in
          vars := (i, x) :: !vars;
          state := State.push st
        done;
        let env = ref (List.map vals ~f:(fun x -> State.Var x)) in
        List.iter !vars ~f:(fun (i, x) ->
            let code = State.Var x in
            let closure_info = State.Dummy in
            if new_closure_repr
            then env := code :: closure_info :: !env
            else env := code :: !env;
            if i > 0
            then
              let infix_tag = State.Dummy in
              env := infix_tag :: !env);
        let env = Array.of_list !env in
        let state = !state in
        let instrs =
          List.fold_left (List.rev !vars) ~init:instrs ~f:(fun instr (i, x) ->
              let addr = pc + 3 + gets code (pc + 3 + i) in
              if debug_parser () then Format.printf "fun %a (" Var.print x;
              let nparams =
                match (get_instr_exn code addr).Instr.code with
                | GRAB -> getu code (addr + 1) + 1
                | _ -> 1
              in
              let offset = i * clo_offset_3 in
              let state' = State.start_function state env offset in
              let params, state' = State.make_stack nparams state' in
              if debug_parser () then Format.printf ") {@.";
              let state' = State.clear_accu state' in
              compile_block infos.blocks infos.debug code addr state';
              if debug_parser () then Format.printf "}@.";
              let args = State.stack_vars state' in
              let state'', _, _ = Addr.Map.find addr !compiled_blocks in
              Debug.propagate (State.stack_vars state'') args;
              Let (x, Closure (List.rev params, (addr, args))) :: instr)
        in
        compile infos (pc + 3 + nfuncs) (State.acc (nfuncs - 1) state) instrs
    | OFFSETCLOSUREM3 ->
        compile infos (pc + 1) (State.env_acc (-clo_offset_3) state) instrs
    | OFFSETCLOSURE0 -> compile infos (pc + 1) (State.env_acc 0 state) instrs
    | OFFSETCLOSURE3 -> compile infos (pc + 1) (State.env_acc clo_offset_3 state) instrs
    | OFFSETCLOSURE ->
        let n = gets code (pc + 1) in
        compile infos (pc + 2) (State.env_acc n state) instrs
    | PUSHOFFSETCLOSUREM3 ->
        let state = State.push state in
        compile infos (pc + 1) (State.env_acc (-clo_offset_3) state) instrs
    | PUSHOFFSETCLOSURE0 ->
        let state = State.push state in
        compile infos (pc + 1) (State.env_acc 0 state) instrs
    | PUSHOFFSETCLOSURE3 ->
        let state = State.push state in
        compile infos (pc + 1) (State.env_acc clo_offset_3 state) instrs
    | PUSHOFFSETCLOSURE ->
        let state = State.push state in
        let n = gets code (pc + 1) in
        compile infos (pc + 2) (State.env_acc n state) instrs
    | GETGLOBAL ->
        let i = getu code (pc + 1) in
        let _, state, instrs = get_global state instrs i in
        compile infos (pc + 2) state instrs
    | PUSHGETGLOBAL ->
        let state = State.push state in
        let i = getu code (pc + 1) in
        let _, state, instrs = get_global state instrs i in
        compile infos (pc + 2) state instrs
    | GETGLOBALFIELD ->
        let i = getu code (pc + 1) in
        let x, state, instrs = get_global state instrs i in
        let j = getu code (pc + 2) in
        let y, state = State.fresh_var state in
        if debug_parser () then Format.printf "%a = %a[%d]@." Var.print y Var.print x j;
        compile infos (pc + 3) state (Let (y, Field (x, j)) :: instrs)
    | PUSHGETGLOBALFIELD ->
        let state = State.push state in
        let i = getu code (pc + 1) in
        let x, state, instrs = get_global state instrs i in
        let j = getu code (pc + 2) in
        let y, state = State.fresh_var state in
        if debug_parser () then Format.printf "%a = %a[%d]@." Var.print y Var.print x j;
        compile infos (pc + 3) state (Let (y, Field (x, j)) :: instrs)
    | SETGLOBAL ->
        let i = getu code (pc + 1) in
        State.size_globals state (i + 1);
        let y = State.accu state in
        let g = State.globals state in
        assert (Option.is_none g.vars.(i));
        if debug_parser () then Format.printf "(global %d) = %a@." i Var.print y;
        let instrs =
          match g.override.(i) with
          | Some f ->
              let v, instrs = f y instrs in
              g.vars.(i) <- Some v;
              instrs
          | None ->
              g.vars.(i) <- Some y;
              instrs
        in
        let x, state = State.fresh_var state in
        if debug_parser () then Format.printf "%a = 0@." Var.print x;
        let instrs = register_global g i instrs in
        compile infos (pc + 2) state (Let (x, const 0l) :: instrs)
    | ATOM0 ->
        let x, state = State.fresh_var state in
        if debug_parser () then Format.printf "%a = ATOM(0)@." Var.print x;
        compile infos (pc + 1) state (Let (x, Block (0, [||], Unknown)) :: instrs)
    | ATOM ->
        let i = getu code (pc + 1) in
        let x, state = State.fresh_var state in
        if debug_parser () then Format.printf "%a = ATOM(%d)@." Var.print x i;
        compile infos (pc + 2) state (Let (x, Block (i, [||], NotArray)) :: instrs)
    | PUSHATOM0 ->
        let state = State.push state in
        let x, state = State.fresh_var state in
        if debug_parser () then Format.printf "%a = ATOM(0)@." Var.print x;
        compile infos (pc + 1) state (Let (x, Block (0, [||], Unknown)) :: instrs)
    | PUSHATOM ->
        let state = State.push state in
        let i = getu code (pc + 1) in
        let x, state = State.fresh_var state in
        if debug_parser () then Format.printf "%a = ATOM(%d)@." Var.print x i;
        compile infos (pc + 2) state (Let (x, Block (i, [||], NotArray)) :: instrs)
    | MAKEBLOCK ->
        let size = getu code (pc + 1) in
        let tag = getu code (pc + 2) in
        let state = State.push state in
        let x, state = State.fresh_var state in
        let contents, state = State.grab size state in
        if debug_parser ()
        then (
          Format.printf "%a = { " Var.print x;
          for i = 0 to size - 1 do
            Format.printf "%d = %a; " i Var.print (List.nth contents i)
          done;
          Format.printf "}@.");
        compile
          infos
          (pc + 3)
          state
          (Let (x, Block (tag, Array.of_list contents, Unknown)) :: instrs)
    | MAKEBLOCK1 ->
        let tag = getu code (pc + 1) in
        let y = State.accu state in
        let x, state = State.fresh_var state in
        if debug_parser () then Format.printf "%a = { 0 = %a; }@." Var.print x Var.print y;
        compile infos (pc + 2) state (Let (x, Block (tag, [| y |], NotArray)) :: instrs)
    | MAKEBLOCK2 ->
        let tag = getu code (pc + 1) in
        let y = State.accu state in
        let z = State.peek 0 state in
        let x, state = State.fresh_var state in
        if debug_parser ()
        then
          Format.printf "%a = { 0 = %a; 1 = %a; }@." Var.print x Var.print y Var.print z;
        compile
          infos
          (pc + 2)
          (State.pop 1 state)
          (Let (x, Block (tag, [| y; z |], NotArray)) :: instrs)
    | MAKEBLOCK3 ->
        let tag = getu code (pc + 1) in
        let y = State.accu state in
        let z = State.peek 0 state in
        let t = State.peek 1 state in
        let x, state = State.fresh_var state in
        if debug_parser ()
        then
          Format.printf
            "%a = { 0 = %a; 1 = %a; 2 = %a }@."
            Var.print
            x
            Var.print
            y
            Var.print
            z
            Var.print
            t;
        compile
          infos
          (pc + 2)
          (State.pop 2 state)
          (Let (x, Block (tag, [| y; z; t |], NotArray)) :: instrs)
    | MAKEFLOATBLOCK ->
        let size = getu code (pc + 1) in
        let state = State.push state in
        let x, state = State.fresh_var state in
        let contents, state = State.grab size state in
        if debug_parser ()
        then (
          Format.printf "%a = { " Var.print x;
          for i = 0 to size - 1 do
            Format.printf "%d = %a; " i Var.print (List.nth contents i)
          done;
          Format.printf "}@.");
        compile
          infos
          (pc + 2)
          state
          (Let (x, Block (254, Array.of_list contents, Unknown)) :: instrs)
    | GETFIELD0 ->
        let y = State.accu state in
        let x, state = State.fresh_var state in
        if debug_parser () then Format.printf "%a = %a[0]@." Var.print x Var.print y;
        compile infos (pc + 1) state (Let (x, Field (y, 0)) :: instrs)
    | GETFIELD1 ->
        let y = State.accu state in
        let x, state = State.fresh_var state in
        if debug_parser () then Format.printf "%a = %a[1]@." Var.print x Var.print y;
        compile infos (pc + 1) state (Let (x, Field (y, 1)) :: instrs)
    | GETFIELD2 ->
        let y = State.accu state in
        let x, state = State.fresh_var state in
        if debug_parser () then Format.printf "%a = %a[2]@." Var.print x Var.print y;
        compile infos (pc + 1) state (Let (x, Field (y, 2)) :: instrs)
    | GETFIELD3 ->
        let y = State.accu state in
        let x, state = State.fresh_var state in
        if debug_parser () then Format.printf "%a = %a[3]@." Var.print x Var.print y;
        compile infos (pc + 1) state (Let (x, Field (y, 3)) :: instrs)
    | GETFIELD ->
        let y = State.accu state in
        let n = getu code (pc + 1) in
        let x, state = State.fresh_var state in
        if debug_parser () then Format.printf "%a = %a[%d]@." Var.print x Var.print y n;
        compile infos (pc + 2) state (Let (x, Field (y, n)) :: instrs)
    | GETFLOATFIELD ->
        let y = State.accu state in
        let n = getu code (pc + 1) in
        let x, state = State.fresh_var state in
        if debug_parser () then Format.printf "%a = %a[%d]@." Var.print x Var.print y n;
        compile infos (pc + 2) state (Let (x, Field (y, n)) :: instrs)
    | SETFIELD0 ->
        let y = State.accu state in
        let z = State.peek 0 state in
        if debug_parser () then Format.printf "%a[0] = %a@." Var.print y Var.print z;
        let x, state = State.fresh_var state in
        if debug_parser () then Format.printf "%a = 0@." Var.print x;
        compile
          infos
          (pc + 1)
          (State.pop 1 state)
          (Let (x, const 0l) :: Set_field (y, 0, z) :: instrs)
    | SETFIELD1 ->
        let y = State.accu state in
        let z = State.peek 0 state in
        if debug_parser () then Format.printf "%a[1] = %a@." Var.print y Var.print z;
        let x, state = State.fresh_var state in
        if debug_parser () then Format.printf "%a = 0@." Var.print x;
        compile
          infos
          (pc + 1)
          (State.pop 1 state)
          (Let (x, const 0l) :: Set_field (y, 1, z) :: instrs)
    | SETFIELD2 ->
        let y = State.accu state in
        let z = State.peek 0 state in
        if debug_parser () then Format.printf "%a[2] = %a@." Var.print y Var.print z;
        let x, state = State.fresh_var state in
        if debug_parser () then Format.printf "%a = 0@." Var.print x;
        compile
          infos
          (pc + 1)
          (State.pop 1 state)
          (Let (x, const 0l) :: Set_field (y, 2, z) :: instrs)
    | SETFIELD3 ->
        let y = State.accu state in
        let z = State.peek 0 state in
        if debug_parser () then Format.printf "%a[3] = %a@." Var.print y Var.print z;
        let x, state = State.fresh_var state in
        if debug_parser () then Format.printf "%a = 0@." Var.print x;
        compile
          infos
          (pc + 1)
          (State.pop 1 state)
          (Let (x, const 0l) :: Set_field (y, 3, z) :: instrs)
    | SETFIELD ->
        let y = State.accu state in
        let z = State.peek 0 state in
        let n = getu code (pc + 1) in
        if debug_parser () then Format.printf "%a[%d] = %a@." Var.print y n Var.print z;
        let x, state = State.fresh_var state in
        if debug_parser () then Format.printf "%a = 0@." Var.print x;
        compile
          infos
          (pc + 2)
          (State.pop 1 state)
          (Let (x, const 0l) :: Set_field (y, n, z) :: instrs)
    | SETFLOATFIELD ->
        let y = State.accu state in
        let z = State.peek 0 state in
        let n = getu code (pc + 1) in
        if debug_parser () then Format.printf "%a[%d] = %a@." Var.print y n Var.print z;
        let x, state = State.fresh_var state in
        if debug_parser () then Format.printf "%a = 0@." Var.print x;
        compile
          infos
          (pc + 2)
          (State.pop 1 state)
          (Let (x, const 0l) :: Set_field (y, n, z) :: instrs)
    | VECTLENGTH ->
        let y = State.accu state in
        let x, state = State.fresh_var state in
        if debug_parser () then Format.printf "%a = %a.length@." Var.print x Var.print y;
        compile infos (pc + 1) state (Let (x, Prim (Vectlength, [ Pv y ])) :: instrs)
    | GETVECTITEM ->
        let y = State.accu state in
        let z = State.peek 0 state in
        let x, state = State.fresh_var state in
        if debug_parser ()
        then Format.printf "%a = %a[%a]@." Var.print x Var.print y Var.print z;
        compile
          infos
          (pc + 1)
          (State.pop 1 state)
          (Let (x, Prim (Array_get, [ Pv y; Pv z ])) :: instrs)
    | SETVECTITEM ->
        if debug_parser ()
        then
          Format.printf
            "%a[%a] = %a@."
            Var.print
            (State.accu state)
            Var.print
            (State.peek 0 state)
            Var.print
            (State.peek 1 state);
        let instrs =
          Array_set (State.accu state, State.peek 0 state, State.peek 1 state) :: instrs
        in
        let x, state = State.fresh_var state in
        if debug_parser () then Format.printf "%a = 0@." Var.print x;
        compile infos (pc + 1) (State.pop 2 state) (Let (x, const 0l) :: instrs)
    | GETSTRINGCHAR ->
        let y = State.accu state in
        let z = State.peek 0 state in
        let x, state = State.fresh_var state in
        if debug_parser ()
        then Format.printf "%a = %a[%a]@." Var.print x Var.print y Var.print z;
        compile
          infos
          (pc + 1)
          (State.pop 1 state)
          (Let (x, Prim (Extern "caml_string_unsafe_get", [ Pv y; Pv z ])) :: instrs)
    | GETBYTESCHAR ->
        let y = State.accu state in
        let z = State.peek 0 state in
        let x, state = State.fresh_var state in
        if debug_parser ()
        then Format.printf "%a = %a[%a]@." Var.print x Var.print y Var.print z;
        compile
          infos
          (pc + 1)
          (State.pop 1 state)
          (Let (x, Prim (Extern "caml_bytes_unsafe_get", [ Pv y; Pv z ])) :: instrs)
    | SETBYTESCHAR ->
        if debug_parser ()
        then
          Format.printf
            "%a[%a] = %a@."
            Var.print
            (State.accu state)
            Var.print
            (State.peek 0 state)
            Var.print
            (State.peek 1 state);
        let x = State.accu state in
        let y = State.peek 0 state in
        let z = State.peek 1 state in
        let t, state = State.fresh_var state in
        let instrs =
          Let (t, Prim (Extern "caml_bytes_unsafe_set", [ Pv x; Pv y; Pv z ])) :: instrs
        in
        let x, state = State.fresh_var state in
        if debug_parser () then Format.printf "%a = 0@." Var.print x;
        compile infos (pc + 1) (State.pop 2 state) (Let (x, const 0l) :: instrs)
    | BRANCH ->
        let offset = gets code (pc + 1) in
        if debug_parser () then Format.printf "... (branch)@.";
        instrs, Branch (pc + offset + 1, State.stack_vars state), state
    | BRANCHIF ->
        let offset = gets code (pc + 1) in
        let x = State.accu state in
        let args = State.stack_vars state in
        instrs, Cond (x, (pc + offset + 1, args), (pc + 2, args)), state
    | BRANCHIFNOT ->
        let offset = gets code (pc + 1) in
        let x = State.accu state in
        let args = State.stack_vars state in
        instrs, Cond (x, (pc + 2, args), (pc + offset + 1, args)), state
    | SWITCH ->
        if debug_parser () then Format.printf "switch ...@.";
        let sz = getu code (pc + 1) in
        let x = State.accu state in
        let args = State.stack_vars state in
        let l = sz land 0xFFFF in
        let it =
          Array.init (sz land 0XFFFF) ~f:(fun i -> pc + 2 + gets code (pc + 2 + i), args)
        in
        let bt =
          Array.init (sz lsr 16) ~f:(fun i -> pc + 2 + gets code (pc + 2 + l + i), args)
        in
        instrs, Switch (x, it, bt), state
    | BOOLNOT ->
        let y = State.accu state in
        let x, state = State.fresh_var state in
        if debug_parser () then Format.printf "%a = !%a@." Var.print x Var.print y;
        compile infos (pc + 1) state (Let (x, Prim (Not, [ Pv y ])) :: instrs)
    | PUSHTRAP ->
        let addr = pc + 1 + gets code (pc + 1) in
        let x, state' = State.fresh_var state in
        compile_block infos.blocks infos.debug code addr state';
        compile_block
          infos.blocks
          infos.debug
          code
          (pc + 2)
          { (State.push_handler state x addr) with
            State.stack =
              (* See interp.c *)
              State.Dummy
              :: State.Dummy :: State.Dummy :: State.Dummy :: state.State.stack
          };
        ( instrs
        , Pushtrap
            ( (pc + 2, State.stack_vars state)
            , x
            , (addr, State.stack_vars state')
            , Addr.Set.empty )
        , state )
    | POPTRAP ->
        let addr = pc + 1 in
        let handler_addr = State.addr_of_current_handler state in
        compile_block
          infos.blocks
          infos.debug
          code
          addr
          (State.pop 4 (State.pop_handler state));
        instrs, Poptrap ((addr, State.stack_vars state), handler_addr), state
    | RERAISE | RAISE_NOTRACE | RAISE ->
        let kind =
          match instr.Instr.code with
          | RERAISE -> `Reraise
          | RAISE_NOTRACE -> `Notrace
          | RAISE -> `Normal
          | _ -> assert false
        in
        if debug_parser () then Format.printf "throw(%a)@." Var.print (State.accu state);
        instrs, Raise (State.accu state, kind), state
    | CHECK_SIGNALS -> compile infos (pc + 1) state instrs
    | C_CALL1 ->
        let prim = primitive_name state (getu code (pc + 1)) in
        if String.equal (Primitive.resolve prim) "%identity"
        then (* This is a no-op *)
          compile infos (pc + 2) state instrs
        else
          let y = State.accu state in
          let x, state = State.fresh_var state in
          if debug_parser ()
          then Format.printf "%a = ccall \"%s\" (%a)@." Var.print x prim Var.print y;
          compile infos (pc + 2) state (Let (x, Prim (Extern prim, [ Pv y ])) :: instrs)
    | C_CALL2 ->
        let prim = primitive_name state (getu code (pc + 1)) in
        let y = State.accu state in
        let z = State.peek 0 state in
        let x, state = State.fresh_var state in
        if debug_parser ()
        then
          Format.printf
            "%a = ccall \"%s\" (%a, %a)@."
            Var.print
            x
            prim
            Var.print
            y
            Var.print
            z;
        compile
          infos
          (pc + 2)
          (State.pop 1 state)
          (Let (x, Prim (Extern prim, [ Pv y; Pv z ])) :: instrs)
    | C_CALL3 ->
        let prim = primitive_name state (getu code (pc + 1)) in
        let y = State.accu state in
        let z = State.peek 0 state in
        let t = State.peek 1 state in
        let x, state = State.fresh_var state in
        if debug_parser ()
        then
          Format.printf
            "%a = ccall \"%s\" (%a, %a, %a)@."
            Var.print
            x
            prim
            Var.print
            y
            Var.print
            z
            Var.print
            t;
        compile
          infos
          (pc + 2)
          (State.pop 2 state)
          (Let (x, Prim (Extern prim, [ Pv y; Pv z; Pv t ])) :: instrs)
    | C_CALL4 ->
        let nargs = 4 in
        let prim = primitive_name state (getu code (pc + 1)) in
        let state = State.push state in
        let x, state = State.fresh_var state in
        let args, state = State.grab nargs state in
        if debug_parser ()
        then (
          Format.printf "%a = ccal \"%s\" (" Var.print x prim;
          for i = 0 to nargs - 1 do
            if i > 0 then Format.printf ", ";
            Format.printf "%a" Var.print (List.nth args i)
          done;
          Format.printf ")@.");
        compile
          infos
          (pc + 2)
          state
          (Let (x, Prim (Extern prim, List.map args ~f:(fun x -> Pv x))) :: instrs)
    | C_CALL5 ->
        let nargs = 5 in
        let prim = primitive_name state (getu code (pc + 1)) in
        let state = State.push state in
        let x, state = State.fresh_var state in
        let args, state = State.grab nargs state in
        if debug_parser ()
        then (
          Format.printf "%a = ccal \"%s\" (" Var.print x prim;
          for i = 0 to nargs - 1 do
            if i > 0 then Format.printf ", ";
            Format.printf "%a" Var.print (List.nth args i)
          done;
          Format.printf ")@.");
        compile
          infos
          (pc + 2)
          state
          (Let (x, Prim (Extern prim, List.map args ~f:(fun x -> Pv x))) :: instrs)
    | C_CALLN ->
        let nargs = getu code (pc + 1) in
        let prim = primitive_name state (getu code (pc + 2)) in
        let state = State.push state in
        let x, state = State.fresh_var state in
        let args, state = State.grab nargs state in
        if debug_parser ()
        then (
          Format.printf "%a = ccal \"%s\" (" Var.print x prim;
          for i = 0 to nargs - 1 do
            if i > 0 then Format.printf ", ";
            Format.printf "%a" Var.print (List.nth args i)
          done;
          Format.printf ")@.");
        compile
          infos
          (pc + 3)
          state
          (Let (x, Prim (Extern prim, List.map args ~f:(fun x -> Pv x))) :: instrs)
    | (CONST0 | CONST1 | CONST2 | CONST3) as cc ->
        let x, state = State.fresh_var state in
        let n =
          match cc with
          | CONST0 -> 0l
          | CONST1 -> 1l
          | CONST2 -> 2l
          | CONST3 -> 3l
          | _ -> assert false
        in
        if debug_parser () then Format.printf "%a = %ld@." Var.print x n;
        compile infos (pc + 1) state (Let (x, const n) :: instrs)
    | CONSTINT ->
        let n = gets32 code (pc + 1) in
        let x, state = State.fresh_var state in
        if debug_parser () then Format.printf "%a = %ld@." Var.print x n;
        compile infos (pc + 2) state (Let (x, const n) :: instrs)
    | (PUSHCONST0 | PUSHCONST1 | PUSHCONST2 | PUSHCONST3) as cc ->
        let state = State.push state in
        let x, state = State.fresh_var state in
        let n =
          match cc with
          | PUSHCONST0 -> 0l
          | PUSHCONST1 -> 1l
          | PUSHCONST2 -> 2l
          | PUSHCONST3 -> 3l
          | _ -> assert false
        in
        if debug_parser () then Format.printf "%a = %ld@." Var.print x n;
        compile infos (pc + 1) state (Let (x, const n) :: instrs)
    | PUSHCONSTINT ->
        let state = State.push state in
        let n = gets32 code (pc + 1) in
        let x, state = State.fresh_var state in
        if debug_parser () then Format.printf "%a = %ld@." Var.print x n;
        compile infos (pc + 2) state (Let (x, const n) :: instrs)
    | NEGINT ->
        let y = State.accu state in
        let x, state = State.fresh_var state in
        if debug_parser () then Format.printf "%a = -%a@." Var.print x Var.print y;
        compile
          infos
          (pc + 1)
          state
          (Let (x, Prim (Extern "%int_neg", [ Pv y ])) :: instrs)
    | ADDINT ->
        let y = State.accu state in
        let z = State.peek 0 state in
        let x, state = State.fresh_var state in
        if debug_parser ()
        then Format.printf "%a = %a + %a@." Var.print x Var.print y Var.print z;
        compile
          infos
          (pc + 1)
          (State.pop 1 state)
          (Let (x, Prim (Extern "%int_add", [ Pv y; Pv z ])) :: instrs)
    | SUBINT ->
        let y = State.accu state in
        let z = State.peek 0 state in
        let x, state = State.fresh_var state in
        if debug_parser ()
        then Format.printf "%a = %a - %a@." Var.print x Var.print y Var.print z;
        compile
          infos
          (pc + 1)
          (State.pop 1 state)
          (Let (x, Prim (Extern "%int_sub", [ Pv y; Pv z ])) :: instrs)
    | MULINT ->
        let y = State.accu state in
        let z = State.peek 0 state in
        let x, state = State.fresh_var state in
        if debug_parser ()
        then Format.printf "%a = %a * %a@." Var.print x Var.print y Var.print z;
        compile
          infos
          (pc + 1)
          (State.pop 1 state)
          (Let (x, Prim (Extern "%int_mul", [ Pv y; Pv z ])) :: instrs)
    | DIVINT ->
        let y = State.accu state in
        let z = State.peek 0 state in
        let x, state = State.fresh_var state in
        if debug_parser ()
        then Format.printf "%a = %a / %a@." Var.print x Var.print y Var.print z;
        compile
          infos
          (pc + 1)
          (State.pop 1 state)
          (Let (x, Prim (Extern "%int_div", [ Pv y; Pv z ])) :: instrs)
    | MODINT ->
        let y = State.accu state in
        let z = State.peek 0 state in
        let x, state = State.fresh_var state in
        if debug_parser ()
        then Format.printf "%a = %a %% %a@." Var.print x Var.print y Var.print z;
        compile
          infos
          (pc + 1)
          (State.pop 1 state)
          (Let (x, Prim (Extern "%int_mod", [ Pv y; Pv z ])) :: instrs)
    | ANDINT ->
        let y = State.accu state in
        let z = State.peek 0 state in
        let x, state = State.fresh_var state in
        if debug_parser ()
        then Format.printf "%a = %a & %a@." Var.print x Var.print y Var.print z;
        compile
          infos
          (pc + 1)
          (State.pop 1 state)
          (Let (x, Prim (Extern "%int_and", [ Pv y; Pv z ])) :: instrs)
    | ORINT ->
        let y = State.accu state in
        let z = State.peek 0 state in
        let x, state = State.fresh_var state in
        if debug_parser ()
        then Format.printf "%a = %a | %a@." Var.print x Var.print y Var.print z;
        compile
          infos
          (pc + 1)
          (State.pop 1 state)
          (Let (x, Prim (Extern "%int_or", [ Pv y; Pv z ])) :: instrs)
    | XORINT ->
        let y = State.accu state in
        let z = State.peek 0 state in
        let x, state = State.fresh_var state in
        if debug_parser ()
        then Format.printf "%a = %a ^ %a@." Var.print x Var.print y Var.print z;
        compile
          infos
          (pc + 1)
          (State.pop 1 state)
          (Let (x, Prim (Extern "%int_xor", [ Pv y; Pv z ])) :: instrs)
    | LSLINT ->
        let y = State.accu state in
        let z = State.peek 0 state in
        let x, state = State.fresh_var state in
        if debug_parser ()
        then Format.printf "%a = %a << %a@." Var.print x Var.print y Var.print z;
        compile
          infos
          (pc + 1)
          (State.pop 1 state)
          (Let (x, Prim (Extern "%int_lsl", [ Pv y; Pv z ])) :: instrs)
    | LSRINT ->
        let y = State.accu state in
        let z = State.peek 0 state in
        let x, state = State.fresh_var state in
        if debug_parser ()
        then Format.printf "%a = %a >>> %a@." Var.print x Var.print y Var.print z;
        compile
          infos
          (pc + 1)
          (State.pop 1 state)
          (Let (x, Prim (Extern "%int_lsr", [ Pv y; Pv z ])) :: instrs)
    | ASRINT ->
        let y = State.accu state in
        let z = State.peek 0 state in
        let x, state = State.fresh_var state in
        if debug_parser ()
        then Format.printf "%a = %a >> %a@." Var.print x Var.print y Var.print z;
        compile
          infos
          (pc + 1)
          (State.pop 1 state)
          (Let (x, Prim (Extern "%int_asr", [ Pv y; Pv z ])) :: instrs)
    | EQ ->
        let y = State.accu state in
        let z = State.peek 0 state in
        let x, state = State.fresh_var state in
        if debug_parser ()
        then Format.printf "%a = mk_bool(%a == %a)@." Var.print x Var.print y Var.print z;
        compile
          infos
          (pc + 1)
          (State.pop 1 state)
          (Let (x, Prim (Eq, [ Pv y; Pv z ])) :: instrs)
    | NEQ ->
        let y = State.accu state in
        let z = State.peek 0 state in
        let x, state = State.fresh_var state in
        if debug_parser ()
        then Format.printf "%a = mk_bool(%a != %a)@." Var.print x Var.print y Var.print z;
        compile
          infos
          (pc + 1)
          (State.pop 1 state)
          (Let (x, Prim (Neq, [ Pv y; Pv z ])) :: instrs)
    | LTINT ->
        let y = State.accu state in
        let z = State.peek 0 state in
        let x, state = State.fresh_var state in
        if debug_parser ()
        then
          Format.printf
            "%a = mk_bool(%a < %a)@."
            Var.print
            x
            Var.print
            y
            Var.print
            (State.peek 0 state);
        compile
          infos
          (pc + 1)
          (State.pop 1 state)
          (Let (x, Prim (Lt, [ Pv y; Pv z ])) :: instrs)
    | LEINT ->
        let y = State.accu state in
        let z = State.peek 0 state in
        let x, state = State.fresh_var state in
        if debug_parser ()
        then Format.printf "%a = mk_bool(%a <= %a)@." Var.print x Var.print y Var.print z;
        compile
          infos
          (pc + 1)
          (State.pop 1 state)
          (Let (x, Prim (Le, [ Pv y; Pv z ])) :: instrs)
    | GTINT ->
        let y = State.accu state in
        let z = State.peek 0 state in
        let x, state = State.fresh_var state in
        if debug_parser ()
        then Format.printf "%a = mk_bool(%a > %a)@." Var.print x Var.print y Var.print z;
        compile
          infos
          (pc + 1)
          (State.pop 1 state)
          (Let (x, Prim (Lt, [ Pv z; Pv y ])) :: instrs)
    | GEINT ->
        let y = State.accu state in
        let z = State.peek 0 state in
        let x, state = State.fresh_var state in
        if debug_parser ()
        then Format.printf "%a = mk_bool(%a >= %a)@." Var.print x Var.print y Var.print z;
        compile
          infos
          (pc + 1)
          (State.pop 1 state)
          (Let (x, Prim (Le, [ Pv z; Pv y ])) :: instrs)
    | OFFSETINT ->
        let n = gets32 code (pc + 1) in
        let y = State.accu state in
        let z, state = State.fresh_var state in
        let x, state = State.fresh_var state in
        if debug_parser () then Format.printf "%a = %a + %ld@." Var.print x Var.print y n;
        compile
          infos
          (pc + 2)
          state
          (Let (x, Prim (Extern "%int_add", [ Pv y; Pv z ])) :: Let (z, const n) :: instrs)
    | OFFSETREF ->
        let n = gets code (pc + 1) in
        let x = State.accu state in
        if debug_parser () then Format.printf "%a += %d@." Var.print x n;
        let instrs = Offset_ref (x, n) :: instrs in
        let x, state = State.fresh_var state in
        if debug_parser () then Format.printf "x = 0@.";
        compile infos (pc + 2) state (Let (x, const 0l) :: instrs)
    | ISINT ->
        let y = State.accu state in
        let x, state = State.fresh_var state in
        if debug_parser () then Format.printf "%a = !%a@." Var.print x Var.print y;
        compile infos (pc + 1) state (Let (x, Prim (IsInt, [ Pv y ])) :: instrs)
    | BEQ ->
        let n = gets32 code (pc + 1) in
        let offset = gets code (pc + 2) in
        let x = State.accu state in
        let args = State.stack_vars state in
        let y = Var.fresh () in
        ( Let (y, Prim (Eq, [ Pc (Int n); Pv x ])) :: instrs
        , Cond (y, (pc + offset + 2, args), (pc + 3, args))
        , state )
    | BNEQ ->
        let n = gets32 code (pc + 1) in
        let offset = gets code (pc + 2) in
        let x = State.accu state in
        let args = State.stack_vars state in
        let y = Var.fresh () in
        ( Let (y, Prim (Eq, [ Pc (Int n); Pv x ])) :: instrs
        , Cond (y, (pc + 3, args), (pc + offset + 2, args))
        , state )
    | BLTINT ->
        let n = gets32 code (pc + 1) in
        let offset = gets code (pc + 2) in
        let x = State.accu state in
        let args = State.stack_vars state in
        let y = Var.fresh () in
        ( Let (y, Prim (Lt, [ Pc (Int n); Pv x ])) :: instrs
        , Cond (y, (pc + offset + 2, args), (pc + 3, args))
        , state )
    | BLEINT ->
        let n = gets32 code (pc + 1) in
        let offset = gets code (pc + 2) in
        let x = State.accu state in
        let args = State.stack_vars state in
        let y = Var.fresh () in
        ( Let (y, Prim (Le, [ Pc (Int n); Pv x ])) :: instrs
        , Cond (y, (pc + offset + 2, args), (pc + 3, args))
        , state )
    | BGTINT ->
        let n = gets32 code (pc + 1) in
        let offset = gets code (pc + 2) in
        let x = State.accu state in
        let args = State.stack_vars state in
        let y = Var.fresh () in
        ( Let (y, Prim (Le, [ Pc (Int n); Pv x ])) :: instrs
        , Cond (y, (pc + 3, args), (pc + offset + 2, args))
        , state )
    | BGEINT ->
        let n = gets32 code (pc + 1) in
        let offset = gets code (pc + 2) in
        let x = State.accu state in
        let args = State.stack_vars state in
        let y = Var.fresh () in
        ( Let (y, Prim (Lt, [ Pc (Int n); Pv x ])) :: instrs
        , Cond (y, (pc + 3, args), (pc + offset + 2, args))
        , state )
    | BULTINT ->
        let n = getu32 code (pc + 1) in
        let offset = gets code (pc + 2) in
        let x = State.accu state in
        let args = State.stack_vars state in
        let y = Var.fresh () in
        ( Let (y, Prim (Ult, [ Pc (Int n); Pv x ])) :: instrs
        , Cond (y, (pc + offset + 2, args), (pc + 3, args))
        , state )
    | BUGEINT ->
        let n = getu32 code (pc + 1) in
        let offset = gets code (pc + 2) in
        let x = State.accu state in
        let args = State.stack_vars state in
        let y = Var.fresh () in
        ( Let (y, Prim (Ult, [ Pc (Int n); Pv x ])) :: instrs
        , Cond (y, (pc + 3, args), (pc + offset + 2, args))
        , state )
    | ULTINT ->
        let y = State.accu state in
        let z = State.peek 0 state in
        let x, state = State.fresh_var state in
        if debug_parser ()
        then
          Format.printf
            "%a = mk_bool(%a <= %a) (unsigned)@."
            Var.print
            x
            Var.print
            y
            Var.print
            z;
        compile
          infos
          (pc + 1)
          (State.pop 1 state)
          (Let (x, Prim (Ult, [ Pv y; Pv z ])) :: instrs)
    | UGEINT ->
        let y = State.accu state in
        let z = State.peek 0 state in
        let x, state = State.fresh_var state in
        if debug_parser ()
        then Format.printf "%a = mk_bool(%a >= %a)@." Var.print x Var.print y Var.print z;
        compile
          infos
          (pc + 1)
          (State.pop 1 state)
          (Let (x, Prim (Ult, [ Pv z; Pv y ])) :: instrs)
    | GETPUBMET ->
        let n = gets32 code (pc + 1) in
        let cache = !method_cache_id in
        incr method_cache_id;
        let obj = State.accu state in
        let state = State.push state in
        let tag, state = State.fresh_var state in
        let m, state = State.fresh_var state in
        if debug_parser () then Format.printf "%a = %ld@." Var.print tag n;
        if debug_parser ()
        then
          Format.printf
            "%a = caml_get_public_method(%a, %a)@."
            Var.print
            m
            Var.print
            obj
            Var.print
            tag;
        compile
          infos
          (pc + 3)
          state
          (Let
             ( m
             , Prim
                 ( Extern "caml_get_public_method"
                 , [ Pv obj; Pv tag; Pc (Int (Int32.of_int cache)) ] ) )
           :: Let (tag, const n) :: instrs)
    | GETDYNMET ->
        let tag = State.accu state in
        let obj = State.peek 0 state in
        let m, state = State.fresh_var state in
        if debug_parser ()
        then
          Format.printf
            "%a = caml_get_public_method(%a, %a)@."
            Var.print
            m
            Var.print
            obj
            Var.print
            tag;
        compile
          infos
          (pc + 1)
          state
          (Let (m, Prim (Extern "caml_get_public_method", [ Pv obj; Pv tag; Pc (Int 0l) ]))
           :: instrs)
    | GETMETHOD ->
        let lab = State.accu state in
        let obj = State.peek 0 state in
        let meths, state = State.fresh_var state in
        let m, state = State.fresh_var state in
        if debug_parser ()
        then Format.printf "%a = lookup(%a, %a)@." Var.print m Var.print obj Var.print lab;
        compile
          infos
          (pc + 1)
          state
          (Let (m, Prim (Array_get, [ Pv meths; Pv lab ]))
           :: Let (meths, Field (obj, 0)) :: instrs)
    | STOP -> instrs, Stop, state
    | EVENT | BREAK | FIRST_UNIMPLEMENTED_OP -> assert false)

(****)

let match_exn_traps (blocks : 'a Addr.Map.t) =
  let map =
    Addr.Map.fold
      (fun _ block map ->
        match block.branch with
        | Poptrap ((cont, _), addr_push) ->
            let set =
              try Addr.Set.add cont (Addr.Map.find addr_push map)
              with Not_found -> Addr.Set.singleton cont
            in
            Addr.Map.add addr_push set map
        | _ -> map)
      blocks
      Addr.Map.empty
  in
  Addr.Map.fold
    (fun pc conts' blocks ->
      match Addr.Map.find pc blocks with
      | { branch = Pushtrap (cont1, x, cont2, conts); _ } as block ->
          assert (Addr.Set.is_empty conts);
          let branch = Pushtrap (cont1, x, cont2, conts') in
          Addr.Map.add pc { block with branch } blocks
      | _ -> assert false)
    map
    blocks

(****)

type one =
  { code : Code.program
  ; cmis : StringSet.t
  ; debug : Debug.t
  }

let parse_bytecode code globals debug_data =
  let state = State.initial globals in
  Code.Var.reset ();
  let blocks = Blocks.analyse debug_data code in
  let blocks =
    (* Disabled. [pc] might not be an appropriate place to split blocks *)
    if false && Debug.enabled debug_data
    then Debug.fold debug_data (fun pc _ blocks -> Blocks.add blocks pc) blocks
    else blocks
  in
  let blocks' = Blocks.finish_analysis blocks in
  if not (Blocks.is_empty blocks') then compile_block blocks' debug_data code 0 state;
  let blocks =
    Addr.Map.mapi
      (fun _ (state, instr, last) ->
        { params = State.stack_vars state
        ; handler = State.current_handler state
        ; body = instr
        ; branch = last
        })
      !compiled_blocks
  in
  compiled_blocks := Addr.Map.empty;
  tagged_blocks := Addr.Set.empty;
  let free_pc = String.length code / 4 in
  let blocks = match_exn_traps blocks in
  { start = 0; blocks; free_pc }

(* HACK - override module *)

let override_global =
  match Ocaml_version.v with
  | `V4_13 -> []
  | `V4_02 | `V4_03 | `V4_04 | `V4_06 | `V4_07 | `V4_08 | `V4_09 | `V4_10 | `V4_11
  | `V4_12 ->
      let jsmodule name func =
        Prim (Extern "%overrideMod", [ Pc (String name); Pc (String func) ])
      in
      [ ( "CamlinternalMod"
        , fun _orig instrs ->
            let x = Var.fresh_n "internalMod" in
            let init_mod = Var.fresh_n "init_mod" in
            let update_mod = Var.fresh_n "update_mod" in
            ( x
            , Let (x, Block (0, [| init_mod; update_mod |], NotArray))
              ::
              Let (init_mod, jsmodule "CamlinternalMod" "init_mod")
              :: Let (update_mod, jsmodule "CamlinternalMod" "update_mod") :: instrs ) )
      ]

(* HACK END *)

let seek_section toc ic name =
  let rec seek_sec curr_ofs = function
    | [] -> raise Not_found
    | (n, len) :: rem ->
        if String.equal n name
        then (
          seek_in ic (curr_ofs - len);
          len)
        else seek_sec (curr_ofs - len) rem
  in
  seek_sec (in_channel_length ic - 16 - (8 * List.length toc)) toc

let read_toc ic =
  let pos_trailer = in_channel_length ic - 16 in
  seek_in ic pos_trailer;
  let num_sections = input_binary_int ic in
  seek_in ic (pos_trailer - (8 * num_sections));
  let section_table = ref [] in
  for _i = 1 to num_sections do
    let name = really_input_string ic 4 in
    let len = input_binary_int ic in
    section_table := (name, len) :: !section_table
  done;
  !section_table

let from_exe
    ?(includes = [])
    ?(toplevel = false)
    ?exported_unit
    ?(dynlink = false)
    ?(debug = false)
    ic =
  let debug_data = Debug.create ~toplevel debug in
  let toc = read_toc ic in
  let prim_size = seek_section toc ic "PRIM" in
  let prim = really_input_string ic prim_size in
  let primitive_table = Array.of_list (String.split_char ~sep:'\000' prim) in
  let code_size = seek_section toc ic "CODE" in
  let code = really_input_string ic code_size in
  ignore (seek_section toc ic "DATA");
  let init_data : Obj.t array = input_value ic in
  let init_data = Array.map ~f:Constants.parse init_data in
  ignore (seek_section toc ic "SYMB");
  let orig_symbols : Ocaml_compiler.Symtable.GlobalMap.t = input_value ic in
  ignore (seek_section toc ic "CRCS");
  let orig_crcs : (string * Digest.t option) list = input_value ic in
  let keeps =
    let t = Hashtbl.create 17 in
    List.iter ~f:(fun (_, s) -> Hashtbl.add t s ()) predefined_exceptions;
    List.iter ~f:(fun s -> Hashtbl.add t s ()) [ "Outcometree"; "Topdirs"; "Toploop" ];
    t
  in
  let keep s =
    try
      Hashtbl.find keeps s;
      true
    with Not_found -> (
      match exported_unit with
      | Some l -> List.mem s ~set:l
      | None -> true)
  in
  let crcs = List.filter ~f:(fun (unit, _crc) -> keep unit) orig_crcs in
  let symbols =
    Ocaml_compiler.Symtable.GlobalMap.filter_global_map
      (fun id -> keep (Ident.name id))
      orig_symbols
  in
  (if not (Debug.dbg_section_needed debug_data)
  then ()
  else
    try
      ignore (seek_section toc ic "DBUG");
      Debug.read debug_data ~crcs ~includes ic
    with Not_found ->
      if Debug.enabled debug_data || Debug.toplevel debug_data
      then
        warn
          "Warning: Program not linked with -g, original variable names and locations \
           not available.@.");
  let globals = make_globals (Array.length init_data) init_data primitive_table in
  (* Initialize module override mechanism *)
  List.iter override_global ~f:(fun (name, v) ->
      try
        let nn = Ident.create_persistent name in
        let i = Ocaml_compiler.Symtable.GlobalMap.find nn orig_symbols in
        globals.override.(i) <- Some v;
        if debug_parser () then Format.eprintf "overriding global %s@." name
      with Not_found -> ());
  if toplevel || dynlink
  then
    (* export globals *)
    Ocaml_compiler.Symtable.GlobalMap.iter
      (fun id n ->
        globals.named_value.(n) <- Some (Ident.name id);
        globals.is_exported.(n) <- true)
      symbols
    (* @vouillon: *)
    (* we should then use the -linkall option to build the toplevel. *)
    (* The OCaml compiler can generate code using this primitive but *)
    (* does not use it itself. This is the only primitive in this case. *)
    (* Ideally, Js_of_ocaml should parse the .mli files for primitives as *)
    (* well as marking this primitive as potentially used. But *)
    (* the -linkall option is probably good enough. *)
    (* Primitive.mark_used "caml_string_greaterthan" *);
  let p = parse_bytecode code globals debug_data in
  (* register predefined exception *)
  let body =
    List.fold_left predefined_exceptions ~init:[] ~f:(fun body (i, name) ->
        globals.named_value.(i) <- Some name;
        let body = register_global ~force:true globals i body in
        globals.is_exported.(i) <- false;
        body)
  in
  let body =
    Array.fold_right_i globals.constants ~init:body ~f:(fun i _ l ->
        match globals.vars.(i) with
        | Some x when globals.is_const.(i) ->
            let l = register_global globals i l in
            Let (x, Constant globals.constants.(i)) :: l
        | _ -> l)
  in
  let body =
    if toplevel
    then
      (* Include linking information *)
      let toc =
        [ "SYMB", Obj.repr symbols; "CRCS", Obj.repr crcs; "PRIM", Obj.repr prim ]
      in
      let gdata = Var.fresh () in
      let infos =
        [ "toc", Constants.parse (Obj.repr toc)
        ; "prim_count", Int (Int32.of_int (Array.length globals.primitives))
        ]
      in
      let body =
        List.fold_left infos ~init:body ~f:(fun rem (name, const) ->
            let c = Var.fresh () in
            Let (c, Constant const)
            ::
            Let
              ( Var.fresh ()
              , Prim (Extern "caml_js_set", [ Pv gdata; Pc (String name); Pv c ]) )
            :: rem)
      in
      Let (gdata, Prim (Extern "caml_get_global_data", [])) :: body
    else body
  in
  (* List interface files *)
  let is_module =
    let is_ident_char = function
      | 'A' .. 'Z' | 'a' .. 'z' | '_' | '\'' | '0' .. '9' -> true
      | _ -> false
    in
    let is_uppercase = function
      | 'A' .. 'Z' -> true
      | _ -> false
    in
    fun name ->
      try
        if String.length name = 0 then raise Exit;
        if not (is_uppercase name.[0]) then raise Exit;
        for i = 1 to String.length name - 1 do
          if not (is_ident_char name.[i]) then raise Exit
        done;
        true
      with Exit -> false
  in
  let cmis =
    let exception_ids =
      List.fold_left predefined_exceptions ~init:(-1) ~f:(fun acc (i, _) -> max acc i)
    in
    if toplevel && Config.Flag.include_cmis ()
    then
      Ocaml_compiler.Symtable.GlobalMap.fold
        (fun id num acc ->
          if num > exception_ids && Ident.global id && is_module (Ident.name id)
          then StringSet.add (Ident.name id) acc
          else acc)
        symbols
        StringSet.empty
    else StringSet.empty
  in
  let cmis =
    match exported_unit with
    | None -> cmis
    | Some l ->
        if toplevel && Config.Flag.include_cmis ()
        then List.fold_left l ~init:cmis ~f:(fun acc s -> StringSet.add s acc)
        else cmis
  in
  let code = prepend p body in
  Code.invariant code;
  { code; cmis; debug = debug_data }

(* As input: list of primitives + size of global table *)
let from_bytes primitives (code : bytecode) =
  let debug_data = Debug.create ~toplevel:false false in
  let globals = make_globals 0 [||] primitives in
  let p = parse_bytecode code globals debug_data in
  let gdata = Var.fresh () in
  let body =
    Array.fold_right_i globals.vars ~init:[] ~f:(fun i var l ->
        match var with
        | Some x when globals.is_const.(i) -> Let (x, Field (gdata, i)) :: l
        | _ -> l)
  in
  let body = Let (gdata, Prim (Extern "caml_get_global_data", [])) :: body in
  prepend p body, debug_data

let from_string primitives (code : string) = from_bytes primitives code

module Reloc = struct
  let gen_patch_int buff pos n =
    Bytes.set buff (pos + 0) (Char.unsafe_chr n);
    Bytes.set buff (pos + 1) (Char.unsafe_chr (n asr 8));
    Bytes.set buff (pos + 2) (Char.unsafe_chr (n asr 16));
    Bytes.set buff (pos + 3) (Char.unsafe_chr (n asr 24))

  type t =
    { mutable pos : int
    ; mutable constants : Code.constant list
    ; mutable step2_started : bool
    ; names : (string, int) Hashtbl.t
    ; primitives : (string, int) Hashtbl.t
    }

  let create () =
    let constants = [] in
    { pos = List.length constants
    ; constants
    ; step2_started = false
    ; names = Hashtbl.create 17
    ; primitives = Hashtbl.create 17
    }

  (* We currently rely on constants to be relocated before globals. *)
  let step1 t compunit code =
    if t.step2_started then assert false;
    let open Cmo_format in
    List.iter compunit.cu_primitives ~f:(fun name ->
        Hashtbl.add t.primitives name (Hashtbl.length t.primitives));
    let slot_for_literal sc =
      t.constants <- Ocaml_compiler.constant_of_const sc :: t.constants;
      let pos = t.pos in
      t.pos <- succ t.pos;
      pos
    in
    let num_of_prim name =
      try Hashtbl.find t.primitives name
      with Not_found ->
        let i = Hashtbl.length t.primitives in
        Hashtbl.add t.primitives name i;
        i
    in
    List.iter compunit.cu_reloc ~f:(function
        | Reloc_literal sc, pos -> gen_patch_int code pos (slot_for_literal sc)
        | Reloc_primitive name, pos -> gen_patch_int code pos (num_of_prim name)
        | _ -> ())

  let step2 t compunit code =
    t.step2_started <- true;
    let open Cmo_format in
    let next id =
      let name = Ident.name id in
      try Hashtbl.find t.names name
      with Not_found ->
        let pos = t.pos in
        t.pos <- succ t.pos;
        Hashtbl.add t.names name pos;
        pos
    in
    let slot_for_getglobal id = next id in
    let slot_for_setglobal id = next id in
    List.iter compunit.cu_reloc ~f:(function
        | Reloc_getglobal id, pos -> gen_patch_int code pos (slot_for_getglobal id)
        | Reloc_setglobal id, pos -> gen_patch_int code pos (slot_for_setglobal id)
        | _ -> ())

  let primitives t =
    let l = Hashtbl.length t.primitives in
    let a = Array.make l "" in
    Hashtbl.iter (fun name i -> a.(i) <- name) t.primitives;
    a

  let constants t = Array.of_list (List.rev t.constants)

  let make_globals t =
    let primitives = primitives t in
    let constants = constants t in
    let globals = make_globals (Array.length constants) constants primitives in
    resize_globals globals t.pos;
    Hashtbl.iter (fun name i -> globals.named_value.(i) <- Some name) t.names;
    (* Initialize module override mechanism *)
    List.iter override_global ~f:(fun (name, v) ->
        try
          let i = Hashtbl.find t.names name in
          globals.override.(i) <- Some v;
          if debug_parser () then Format.eprintf "overriding global %s@." name
        with Not_found -> ());
    globals
end

let from_compilation_units ~includes:_ ~toplevel ~debug_data l =
  let reloc = Reloc.create () in
  List.iter l ~f:(fun (compunit, code) -> Reloc.step1 reloc compunit code);
  List.iter l ~f:(fun (compunit, code) -> Reloc.step2 reloc compunit code);
  let globals = Reloc.make_globals reloc in
  let code =
    let l = List.map l ~f:(fun (_, c) -> Bytes.to_string c) in
    String.concat ~sep:"" l
  in
  let prog = parse_bytecode code globals debug_data in
  let gdata = Var.fresh_n "global_data" in
  let body =
    Array.fold_right_i globals.vars ~init:[] ~f:(fun i var l ->
        match var with
        | Some x when globals.is_const.(i) -> (
            match globals.named_value.(i) with
            | None ->
                let l = register_global globals i l in
                let cst = globals.constants.(i) in
                (match cst, Code.Var.get_name x with
                | (String str | IString str), None ->
                    Code.Var.name x (Printf.sprintf "cst_%s" str)
                | _ -> ());
                Let (x, Constant cst) :: l
            | Some name ->
                Var.name x name;
                Let (x, Prim (Extern "caml_js_get", [ Pv gdata; Pc (IString name) ])) :: l
            )
        | _ -> l)
  in
  let body = Let (gdata, Prim (Extern "caml_get_global_data", [])) :: body in
  let cmis =
    if toplevel && Config.Flag.include_cmis ()
    then
      List.fold_left l ~init:StringSet.empty ~f:(fun acc (compunit, _) ->
          StringSet.add compunit.Cmo_format.cu_name acc)
    else StringSet.empty
  in
  { code = prepend prog body; cmis; debug = debug_data }

let from_cmo ?(includes = []) ?(toplevel = false) ?(debug = false) compunit ic =
  let debug_data = Debug.create ~toplevel debug in
  seek_in ic compunit.Cmo_format.cu_pos;
  let code = Bytes.create compunit.Cmo_format.cu_codesize in
  really_input ic code 0 compunit.Cmo_format.cu_codesize;
  if (not (Debug.dbg_section_needed debug_data)) || compunit.Cmo_format.cu_debug = 0
  then ()
  else (
    seek_in ic compunit.Cmo_format.cu_debug;
    Debug.read_event_list debug_data ~crcs:[] ~includes ~orig:0 ic);
  let p = from_compilation_units ~toplevel ~includes ~debug_data [ compunit, code ] in
  Code.invariant p.code;
  p

let from_cma ?(includes = []) ?(toplevel = false) ?(debug = false) lib ic =
  let debug_data = Debug.create ~toplevel debug in
  let orig = ref 0 in
  let units =
    List.map lib.Cmo_format.lib_units ~f:(fun compunit ->
        seek_in ic compunit.Cmo_format.cu_pos;
        let code = Bytes.create compunit.Cmo_format.cu_codesize in
        really_input ic code 0 compunit.Cmo_format.cu_codesize;
        if (not (Debug.dbg_section_needed debug_data)) || compunit.Cmo_format.cu_debug = 0
        then ()
        else (
          seek_in ic compunit.Cmo_format.cu_debug;
          Debug.read_event_list debug_data ~crcs:[] ~includes ~orig:!orig ic);
        orig := !orig + compunit.Cmo_format.cu_codesize;
        compunit, code)
  in
  let p = from_compilation_units ~toplevel ~includes ~debug_data units in
  Code.invariant p.code;
  p

let from_channel ic =
  let format =
    try
      let header = really_input_string ic Magic_number.size in
      `Pre (Magic_number.of_string header)
    with _ ->
      let pos_magic = in_channel_length ic - Magic_number.size in
      seek_in ic pos_magic;
      let header = really_input_string ic Magic_number.size in
      `Post (Magic_number.of_string header)
  in
  match format with
  | `Pre magic -> (
      match Magic_number.kind magic with
      | `Cmo ->
          if Config.Flag.check_magic ()
             && not (Magic_number.equal magic Magic_number.current_cmo)
          then raise Magic_number.(Bad_magic_version magic);
          let compunit_pos = input_binary_int ic in
          seek_in ic compunit_pos;
          let compunit : Cmo_format.compilation_unit = input_value ic in
          `Cmo compunit
      | `Cma ->
          if Config.Flag.check_magic ()
             && not (Magic_number.equal magic Magic_number.current_cma)
          then raise Magic_number.(Bad_magic_version magic);
          let pos_toc = input_binary_int ic in
          (* Go to table of contents *)
          seek_in ic pos_toc;
          let lib : Cmo_format.library = input_value ic in
          `Cma lib
      | _ -> raise Magic_number.(Bad_magic_number (to_string magic)))
  | `Post magic -> (
      match Magic_number.kind magic with
      | `Exe ->
          if Config.Flag.check_magic ()
             && not (Magic_number.equal magic Magic_number.current_exe)
          then raise Magic_number.(Bad_magic_version magic);
          `Exe
      | _ -> raise Magic_number.(Bad_magic_number (to_string magic)))

let predefined_exceptions () =
  let body =
    let open Code in
    List.map predefined_exceptions ~f:(fun (index, name) ->
        let exn = Var.fresh () in
        let v_name = Var.fresh () in
        let v_name_js = Var.fresh () in
        let v_index = Var.fresh () in
        [ Let (v_name, Constant (String name))
        ; Let (v_name_js, Constant (IString name))
        ; Let (v_index, Constant (Int (Int32.of_int (-index))))
        ; Let (exn, Block (248, [| v_name; v_index |], NotArray))
        ; Let
            ( Var.fresh ()
            , Prim
                ( Extern "caml_register_global"
                , [ Pc (Int (Int32.of_int index)); Pv exn; Pv v_name_js ] ) )
        ])
    |> List.concat
  in
  let block = { params = []; handler = None; body; branch = Stop } in
  { start = 0; blocks = Addr.Map.singleton 0 block; free_pc = 1 }
