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

let times = Debug.find "times"

type bytecode = string

let predefined_exceptions =
  Runtimedef.builtin_exceptions |> Array.to_list |> List.mapi ~f:(fun i name -> i, name)

let new_closure_repr = Ocaml_version.compare Ocaml_version.current [ 4; 12 ] >= 0

(* Read and manipulate debug section *)
module Debug : sig
  type t

  type position =
    | Before
    | After

  val names : t -> bool

  val enabled : t -> bool

  val is_empty : t -> bool

  val dbg_section_needed : t -> bool

  val propagate : Code.Var.t list -> Code.Var.t list -> unit

  val find : t -> Code.Addr.t -> (int * Ident.t) list * Env.summary

  val find_rec : t -> Code.Addr.t -> (int * Ident.t) list

  val find_loc : t -> position:position -> Code.Addr.t -> Parse_info.t option

  val find_locs : t -> int -> (string option * Instruct.debug_event) list

  val event_location :
       position:position
    -> source:string option
    -> event:Instruct.debug_event
    -> Parse_info.t

  val read_event :
       paths:string list
    -> crcs:(string, string option) Hashtbl.t
    -> orig:int
    -> t
    -> Instruct.debug_event
    -> unit

  val read :
    t -> crcs:(string * string option) list -> includes:string list -> in_channel -> unit

  val read_event_list :
       t
    -> crcs:(string * string option) list
    -> includes:string list
    -> orig:int
    -> in_channel
    -> unit

  val create : include_cmis:bool -> bool -> t

  val paths : t -> units:StringSet.t -> StringSet.t
end = struct
  open Instruct

  type path = string

  type ml_unit =
    { module_name : string
    ; crc : string option
    ; paths : string list
    ; source : path option
    }
  [@@ocaml.warning "-unused-field"]

  type event_and_source =
    { event : debug_event
    ; source : path option
    }

  module Int_table = Hashtbl.Make (Int)

  type t =
    { events_by_pc : event_and_source Int_table.t
    ; units : (string * string option, ml_unit) Hashtbl.t
    ; names : bool
    ; enabled : bool
    ; include_cmis : bool
    }

  type position =
    | Before
    | After

  let names t = t.names

  let enabled t = t.enabled

  let dbg_section_needed t = t.names || t.enabled || t.include_cmis

  let relocate_event orig ev = ev.ev_pos <- (orig + ev.ev_pos) / 4

  let create ~include_cmis enabled =
    let names = enabled || Config.Flag.pretty () in
    { events_by_pc = Int_table.create 17
    ; units = Hashtbl.create 17
    ; names
    ; enabled
    ; include_cmis
    }

  let is_empty t = Int_table.length t.events_by_pc = 0

  let find_ml_in_paths paths name =
    let uname = String.uncapitalize_ascii name in
    match Fs.find_in_path paths (uname ^ ".ml") with
    | Some _ as x -> x
    | None -> Fs.find_in_path paths (name ^ ".ml")

  let read_event
      ~paths
      ~crcs
      ~orig
      { events_by_pc; units; names; enabled; include_cmis = _ }
      ev =
    let pos_fname =
      match ev.ev_loc.Location.loc_start.Lexing.pos_fname with
      | "_none_" -> None
      | x -> Some x
    in
    let ev_module = ev.ev_module in
    let unit =
      try Hashtbl.find units (ev_module, pos_fname)
      with Not_found ->
        let crc = try Hashtbl.find crcs ev_module with Not_found -> None in
        let source : path option =
          (* First search the source based on [pos_fname] because the
             filename of the source might be unreleased to the
             module name. (e.g. pos_fname = list.ml, module = Stdlib__list) *)
          let from_pos_fname =
            match pos_fname with
            | None -> None
            | Some pos_fname -> (
                match Fs.find_in_path paths pos_fname with
                | Some _ as x -> x
                | None -> Fs.find_in_path paths (Filename.basename pos_fname))
          in
          match from_pos_fname with
          | None -> find_ml_in_paths paths ev_module
          | Some _ as x -> x
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
            (match pos_fname with
            | None -> "NONE"
            | Some x -> x);
        let u = { module_name = ev_module; crc; source; paths } in
        Hashtbl.add units (ev_module, pos_fname) u;
        u
    in
    relocate_event orig ev;
    if enabled || names
    then Int_table.add events_by_pc ev.ev_pos { event = ev; source = unit.source };
    ()

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
    fun debug ~crcs ~includes ~orig ic ->
      let crcs =
        let t = Hashtbl.create 17 in
        List.iter crcs ~f:(fun (m, crc) -> Hashtbl.add t m crc);
        t
      in
      let evl : debug_event list = input_value ic in
      let paths = read_paths ic @ includes in
      List.iter evl ~f:(read_event ~paths ~crcs ~orig debug)

  let read t ~crcs ~includes ic =
    let len = input_binary_int ic in
    for _i = 0 to len - 1 do
      let orig = input_binary_int ic in
      read_event_list t ~crcs ~includes ~orig ic
    done

  let find { events_by_pc; _ } pc =
    try
      let { event; _ } = Int_table.find events_by_pc pc in
      let l =
        Ident.fold_name
          (fun ident i acc -> (event.ev_stacksize - i, ident) :: acc)
          event.ev_compenv.ce_stack
          []
        |> List.sort ~cmp:(fun (i, _) (j, _) -> compare i j)
      in

      l, event.ev_typenv
    with Not_found -> [], Env.Env_empty

  let find_rec { events_by_pc; _ } pc =
    try
      let { event; _ } = Int_table.find events_by_pc pc in
      let env = event.ev_compenv in
      let names =
        Ident.fold_name
          (fun ident i acc -> ((if new_closure_repr then i / 3 else i / 2), ident) :: acc)
          env.ce_rec
          []
      in
      List.sort names ~cmp:(fun (i, _) (j, _) -> compare i j)
    with Not_found -> []
  [@@if ocaml_version < (5, 2, 0)]

  let find_rec { events_by_pc; _ } pc =
    try
      let { event; _ } = Int_table.find events_by_pc pc in
      let env = event.ev_compenv in
      let names =
        match env.ce_closure with
        | Not_in_closure -> raise Not_found
        | In_closure { entries; _ } ->
            Ident.fold_name
              (fun ident ent acc ->
                match ent with
                | Function i -> (i / 3, ident) :: acc
                | Free_variable _ -> acc)
              entries
              []
      in
      List.sort names ~cmp:(fun (i, _) (j, _) -> compare i j)
    with Not_found -> []
  [@@if ocaml_version >= (5, 2, 0)]

  let dummy_location (loc : Location.t) =
    loc.loc_start.pos_cnum = -1 || loc.loc_end.pos_cnum = -1

  (* We can have several events at the same location when a function
     application is followed by a branch target, typically due to some
     code like [if ... then f(); ...] : the event after the function
     application, and the event at the beginning of the continuation.
     Both events are interesting. They are returned by this function
     in the expected order: first after the function call, then before
     the continuation. *)
  let find_locs { events_by_pc; _ } pc =
    List.filter_map (Int_table.find_all events_by_pc pc) ~f:(fun { event; source } ->
        if dummy_location event.ev_loc then None else Some (source, event))

  let event_location ~position ~source ~event =
    let pos =
      match position with
      | After -> event.ev_loc.Location.loc_end
      | Before -> event.ev_loc.Location.loc_start
    in
    Parse_info.t_of_position ~src:source pos

  let find_loc t ~position pc =
    match find_locs t pc with
    | [] -> None
    | (source, event) :: _ -> Some (event_location ~position ~source ~event)

  let rec propagate l1 l2 =
    match l1, l2 with
    | v1 :: r1, v2 :: r2 ->
        Var.propagate_name v1 v2;
        propagate r1 r2
    | [], [] -> ()
    | _ -> assert false

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

  val analyse : bytecode -> t

  val next : t -> int -> int

  val is_empty : t -> bool
end = struct
  type t = int array

  let add blocks pc = Addr.Set.add pc blocks

  let rec scan blocks code pc len =
    if pc < len
    then
      match (get_instr_exn code pc).kind with
      | KNullary -> scan blocks code (pc + 1) len
      | KUnary -> scan blocks code (pc + 2) len
      | KBinary -> scan blocks code (pc + 3) len
      | KNullaryCall -> scan blocks code (pc + 1) len
      | KUnaryCall -> scan blocks code (pc + 2) len
      | KBinaryCall -> scan blocks code (pc + 3) len
      | KJump ->
          let offset = gets code (pc + 1) in
          let blocks = Addr.Set.add (pc + offset + 1) blocks in
          scan blocks code (pc + 2) len
      | KCond_jump ->
          let offset = gets code (pc + 1) in
          let blocks = Addr.Set.add (pc + offset + 1) blocks in
          scan blocks code (pc + 2) len
      | KCmp_jump ->
          let offset = gets code (pc + 2) in
          let blocks = Addr.Set.add (pc + offset + 2) blocks in
          scan blocks code (pc + 3) len
      | KSwitch ->
          let sz = getu code (pc + 1) in
          let blocks = ref blocks in
          for i = 0 to (sz land 0xffff) + (sz lsr 16) - 1 do
            let offset = gets code (pc + 2 + i) in
            blocks := Addr.Set.add (pc + offset + 2) !blocks
          done;
          scan !blocks code (pc + 2 + (sz land 0xffff) + (sz lsr 16)) len
      | KClosurerec ->
          let nfuncs = getu code (pc + 1) in
          scan blocks code (pc + nfuncs + 3) len
      | KClosure -> scan blocks code (pc + 3) len
      | KStop n -> scan blocks code (pc + n + 1) len
      | K_will_not_happen -> assert false
    else (
      assert (pc = len);
      blocks)

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

  let analyse code =
    let blocks = Addr.Set.empty in
    let len = String.length code / 4 in
    let blocks = add blocks 0 in
    let blocks = add blocks len in
    let blocks = scan blocks code 0 len in
    Array.of_list (Addr.Set.elements blocks)
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
        | Some name when same_ident name ident_32 -> (
            let i : int32 = Obj.magic x in
            match Config.target () with
            | `JavaScript -> Int (Targetint.of_int32_warning_on_overflow i)
            | `Wasm -> Int32 i)
        | Some name when same_ident name ident_native -> (
            let i : nativeint = Obj.magic x in
            match Config.target () with
            | `JavaScript -> Int (Targetint.of_nativeint_warning_on_overflow i)
            | `Wasm -> NativeInt (Int32.of_nativeint_warning_on_overflow i))
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
      Int (Targetint.of_int_warning_on_overflow i)

  let inlined = function
    | String _ | NativeString _ -> false
    | Float _ -> true
    | Float_array _ -> false
    | Int64 _ -> false
    | Tuple _ -> false
    | Int _ -> true
    | Int32 _ | NativeInt _ -> false
end

let const32 i = Constant (Int (Targetint.of_int32_exn i))

let const i = Constant (Int (Targetint.of_int_exn i))

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
    | Dummy of string
    | Unset

  let elt_to_var e =
    match e with
    | Var x -> x
    | _ -> assert false

  let print_elt f v =
    match v with
    | Var x -> Format.fprintf f "%a" Var.print x
    | Dummy _ -> Format.fprintf f "٭"
    | Unset -> Format.fprintf f "∅"

  type handler = { stack : elt list }

  type t =
    { accu : elt
    ; stack : elt list
    ; env : elt array
    ; env_offset : int
    ; handlers : handler list
    ; globals : globals
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
        | Dummy _ | Unset -> l)

  let set_accu st x = { st with accu = Var x }

  let clear_accu st = { st with accu = Unset }

  let peek n st = elt_to_var (List.nth st.stack n)

  let grab n st = List.map (list_start n st.stack) ~f:elt_to_var, pop n st

  let rec st_assign s n x =
    match s with
    | [] -> assert false
    | y :: rem -> if n = 0 then x :: rem else y :: st_assign rem (n - 1) x

  let assign st n = { st with stack = st_assign st.stack n st.accu }

  let start_function state env offset =
    { state with accu = Unset; stack = []; env; env_offset = offset; handlers = [] }

  let start_block _current_pc state =
    let stack =
      List.fold_right state.stack ~init:[] ~f:(fun e stack ->
          match e with
          | Dummy x -> Dummy x :: stack
          | Unset -> Unset :: stack
          | Var x ->
              let y = Var.fork x in
              Var y :: stack)
    in
    let state = { state with stack } in
    match state.accu with
    | Dummy _ | Unset -> state
    | Var x ->
        let y, state = fresh_var state in
        Var.propagate_name x y;
        state

  let push_handler state =
    { state with handlers = { stack = state.stack } :: state.handlers }

  let pop_handler state = { state with handlers = List.tl state.handlers }

  let initial g =
    { accu = Unset; stack = []; env = [||]; env_offset = 0; handlers = []; globals = g }

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

  let rec name_rec debug i l s summary =
    match l, s with
    | [], _ -> ()
    | (j, ident) :: lrem, Var v :: srem when i = j ->
        Var.name v (Ident.name ident);
        name_rec debug (i + 1) lrem srem summary
    | (j, _) :: _, _ :: srem when i < j -> name_rec debug (i + 1) l srem summary
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
  match g.is_exported.(i), force, Config.target () with
  | true, _, `Wasm ->
      (* Register a compilation unit (Wasm) *)
      assert (not force);
      let name =
        match g.named_value.(i) with
        | None -> assert false
        | Some name -> name
      in
      Code.Var.name (access_global g i) name;
      Let
        ( Var.fresh ()
        , Prim (Extern "caml_set_global", [ Pc (String name); Pv (access_global g i) ]) )
      :: rem
  | true, _, (`JavaScript as target) | false, true, ((`Wasm | `JavaScript) as target) ->
      (* Register an exception (if force = true), or a compilation unit
         (Javascript) *)
      let args =
        match g.named_value.(i) with
        | None -> []
        | Some name ->
            Code.Var.name (access_global g i) name;
            [ Pc
                (match target with
                | `JavaScript -> NativeString (Native_string.of_string name)
                | `Wasm -> String name)
            ]
      in
      Let
        ( Var.fresh ()
        , Prim
            ( Extern "caml_register_global"
            , Pc (Int (Targetint.of_int_exn i)) :: Pv (access_global g i) :: args ) )
      :: rem
  | false, false, (`JavaScript | `Wasm) -> rem

let get_global state instrs i =
  State.size_globals state (i + 1);
  let g = State.globals state in
  match g.vars.(i) with
  | Some x ->
      (* Registered global *)
      if debug_parser () then Format.printf "(global access %a)@." Var.print x;
      x, State.set_accu state x, instrs
  | None -> (
      if i < Array.length g.constants && Constants.inlined g.constants.(i)
      then
        (* Inlined constant *)
        let x, state = State.fresh_var state in
        let cst = g.constants.(i) in
        x, state, Let (x, Constant cst) :: instrs
      else
        match i < Array.length g.constants, Config.target () with
        | true, _ | false, `JavaScript ->
            (* Non-inlined constant, and reference to another compilation
               units in case of separate compilation (JavaScript).
               Some code is generated in a prelude to store the relevant
               module in variable [x]. *)
            g.is_const.(i) <- true;
            let x, state = State.fresh_var state in
            if debug_parser () then Format.printf "%a = CONST(%d)@." Var.print x i;
            g.vars.(i) <- Some x;
            x, state, instrs
        | false, `Wasm -> (
            (* Reference to another compilation units in case of separate
               compilation (Wasm).
               The toplevel module is available in an imported global
               variables. *)
            match g.named_value.(i) with
            | None -> assert false
            | Some name ->
                let x, state = State.fresh_var state in
                if debug_parser ()
                then Format.printf "%a = get_global(%s)@." Var.print x name;
                ( x
                , state
                , Let (x, Prim (Extern "caml_get_global", [ Pc (String name) ])) :: instrs
                )))

let tagged_blocks = ref Addr.Map.empty

let compiled_blocks : (_ * instr list * last) Addr.Map.t ref = ref Addr.Map.empty

let method_cache_id = ref 1

let clo_offset_3 = if new_closure_repr then 3 else 2

type compile_info =
  { blocks : Blocks.t
  ; code : string
  ; limit : int
  ; debug : Debug.t
  }

let string_of_addr debug_data addr =
  List.map
    (Debug.find_locs debug_data addr)
    ~f:(fun (src, { Instruct.ev_loc = loc; ev_kind = kind; _ }) ->
      let pos (p : Lexing.position) =
        Printf.sprintf "%d:%d" p.pos_lnum (p.pos_cnum - p.pos_bol)
      in
      let file =
        match src with
        | None -> "<unknown>"
        | Some file -> file
      in
      let kind =
        match kind with
        | Event_before -> "(before)"
        | Event_after _ -> "(after)"
        | Event_pseudo -> "(pseudo)"
      in
      Printf.sprintf "%s:%s-%s %s" file (pos loc.loc_start) (pos loc.loc_end) kind)

let rec compile_block blocks debug_data code pc state : unit =
  match Addr.Map.find_opt pc !tagged_blocks with
  | Some old_state -> (
      (* Check that the shape of the stack is compatible with the one used to compile the block *)
      let rec check (xs : State.elt list) (ys : State.elt list) =
        match xs, ys with
        | Var _ :: xs, Var _ :: ys -> check xs ys
        | Dummy _ :: xs, Dummy _ :: ys -> check xs ys
        | Unset :: _, _ -> assert false
        | _, Unset :: _ -> assert false
        | [], [] -> ()
        | Var _ :: _, Dummy _ :: _ -> assert false
        | Dummy _ :: _, Var _ :: _ -> assert false
        | _ :: _, [] -> assert false
        | [], _ :: _ -> assert false
      in
      check old_state.State.stack state.State.stack;
      match old_state.State.accu, state.State.accu with
      | Dummy _, Dummy _ -> ()
      | Var _, Var _ -> ()
      | Unset, Unset -> ()
      | Var _, Dummy _ -> assert false
      | Dummy _, Var _ -> assert false
      | Unset, _ -> assert false
      | _, Unset -> assert false)
  | None -> (
      let limit = Blocks.next blocks pc in
      assert (limit > pc);
      if debug_parser () then Format.eprintf "Compiling from %d to %d@." pc (limit - 1);
      let state = State.start_block pc state in
      tagged_blocks := Addr.Map.add pc state !tagged_blocks;
      let instr, last, state' =
        compile { blocks; code; limit; debug = debug_data } pc state []
      in
      assert (not (Addr.Map.mem pc !compiled_blocks));
      (* When jumping to a block that was already visited and the
         [accu] was [Unset] for that block, we make the current accu
         [Unset] *)
      let adjust_state pc =
        match state', Addr.Map.find_opt pc !tagged_blocks with
        | _, None -> state'
        | { State.accu = Var _; _ }, Some { State.accu = Unset; _ } ->
            State.clear_accu state'
        | _, _ -> state'
      in
      let mk_cont pc =
        let state = adjust_state pc in
        pc, State.stack_vars state
      in
      let last =
        match last with
        | Branch (pc, _) -> Branch (mk_cont pc)
        | Cond (x, (pc1, _), (pc2, _)) -> Cond (x, mk_cont pc1, mk_cont pc2)
        | Poptrap (pc, _) -> Poptrap (mk_cont pc)
        | Switch (x, a) -> Switch (x, Array.map a ~f:(fun (pc, _) -> mk_cont pc))
        | Raise _ | Return _ | Stop -> last
        | Pushtrap _ -> assert false
      in
      compiled_blocks := Addr.Map.add pc (state, List.rev instr, last) !compiled_blocks;
      match last with
      | Branch (pc', _) -> compile_block blocks debug_data code pc' (adjust_state pc')
      | Cond (_, (pc1, _), (pc2, _)) ->
          compile_block blocks debug_data code pc1 (adjust_state pc1);
          compile_block blocks debug_data code pc2 (adjust_state pc2)
      | Poptrap (_, _) -> ()
      | Switch (_, _) -> ()
      | Raise _ | Return _ | Stop -> ()
      | Pushtrap _ -> assert false)

and compile infos pc state (instrs : instr list) =
  if debug_parser () then State.print state;
  assert (pc <= infos.limit);
  if debug_parser ()
  then
    List.iter (string_of_addr infos.debug pc) ~f:(fun s ->
        Format.eprintf "@@@@ %s @@@@@." s);

  let instrs =
    let push_event position source event instrs =
      match instrs with
      | Event _ :: instrs | instrs ->
          Event (Debug.event_location ~position ~source ~event) :: instrs
    in
    List.fold_left
      (Debug.find_locs infos.debug pc)
      ~init:instrs
      ~f:(fun instrs (source, event) ->
        match event, instrs with
        | { Instruct.ev_kind = Event_pseudo; ev_info = Event_other; _ }, _ ->
            (* Ignore allocation events (not very interesting) *)
            if debug_parser () then Format.eprintf "Ignored allocation event@.";
            instrs
        | ( { ev_kind = Event_pseudo | Event_after _; ev_info = Event_return _; _ }
          , (Let (_, (Apply _ | Prim _)) as i) :: rem ) ->
            (* Event after a call. If it is followed by another event,
               it may have been weaken to a pseudo-event but was kept
               for stack traces *)
            if debug_parser () then Format.eprintf "Added event across call@.";
            push_event After source event (i :: push_event Before source event rem)
        | { ev_kind = Event_pseudo; ev_info = Event_function; _ }, [] ->
            (* At beginning of function *)
            if debug_parser () then Format.eprintf "Added event at function start@.";
            push_event Before source event instrs
        | { ev_kind = Event_after _ | Event_pseudo; ev_info = Event_return _; _ }, _ ->
            if debug_parser ()
            then
              Format.eprintf "Ignored useless event (beginning of a block after a call)@.";
            instrs
        | { ev_kind = Event_after _; ev_info = Event_other; _ }, _ ->
            if debug_parser ()
            then Format.eprintf "Ignored useless event (before a raise)@.";
            (* We already have an event for the exception. The
               compiler add these events for stack traces. *)
            instrs
        | { ev_kind = Event_before; ev_info = Event_other; _ }, _
        | { ev_kind = Event_before | Event_pseudo; ev_info = Event_function; _ }, _ ->
            if debug_parser () then Format.eprintf "added event@.";
            push_event Before source event instrs
        | { ev_kind = Event_after _; ev_info = Event_function; _ }, _
        | { ev_kind = Event_before; ev_info = Event_return _; _ }, _ ->
            (* Nonsensical events *)
            assert false)
  in

  if pc = infos.limit
  then
    if
      (* stop if we reach end_of_code (ie when compiling cmo) *)
      pc = String.length infos.code / 4
    then (
      if debug_parser () then Format.eprintf "Stop@.";
      instrs, Stop, state)
    else (
      State.name_vars state infos.debug pc;
      if debug_parser ()
      then Format.eprintf "Branch %d (%a) @." pc Print.var_list (State.stack_vars state);
      instrs, Branch (pc, []), state)
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
        let accu = State.accu state in
        let state = State.assign state n in
        let stack_size = List.length state.stack in
        let x, state = State.fresh_var state in
        let instrs =
          (* If the assigned variable is used in an exception handler,
             we register that from now on the parameter [dest] should
             be bound to the value of [accu] when entering the
             exception handler.
             For the optimization phases, [Assign (dest, acccu)] is
             interpreted as: [accu] is a possible value for parameter
             [dest]. For code generation, this is implemented as an
             assignment.
             For this to make sense, the intermediate code generated
             for [PUSHTRAP] is such that [dest] is in scope at this
             point but is only used in the exception handler.
          *)
          List.fold_left
            state.handlers
            ~init:(Let (x, const 0) :: instrs)
            ~f:(fun acc (handler : State.handler) ->
              let handler_stack_size = List.length handler.stack in
              let diff = stack_size - handler_stack_size in
              if n >= diff
              then
                let dest = State.elt_to_var (List.nth handler.stack (n - diff)) in
                Assign (dest, accu) :: acc
              else acc)
        in
        if debug_parser () then Format.printf "%a = 0@." Var.print x;
        compile infos (pc + 2) state instrs
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
              State.Dummy "push_retaddr(retaddr)"
              :: State.Dummy "push_retaddr(env)"
              :: State.Dummy "push_retaddr(extra_args)"
              :: state.State.stack
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
          (Let (x, Apply { f; args; exact = false }) :: instrs)
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
          (Let (x, Apply { f; args = [ y ]; exact = false }) :: instrs)
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
          (Let (x, Apply { f; args = [ y; z ]; exact = false }) :: instrs)
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
          (Let (x, Apply { f; args = [ y; z; t ]; exact = false }) :: instrs)
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
        Let (x, Apply { f; args = l; exact = false }) :: instrs, Return x, state
    | APPTERM1 ->
        let f = State.accu state in
        let x = State.peek 0 state in
        if debug_parser () then Format.printf "return %a(%a)@." Var.print f Var.print x;
        let y, state = State.fresh_var state in
        Let (y, Apply { f; args = [ x ]; exact = false }) :: instrs, Return y, state
    | APPTERM2 ->
        let f = State.accu state in
        let x = State.peek 0 state in
        let y = State.peek 1 state in
        if debug_parser ()
        then Format.printf "return %a(%a, %a)@." Var.print f Var.print x Var.print y;
        let z, state = State.fresh_var state in
        Let (z, Apply { f; args = [ x; y ]; exact = false }) :: instrs, Return z, state
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
        Let (t, Apply { f; args = [ x; y; z ]; exact = false }) :: instrs, Return t, state
    | RETURN ->
        let x = State.accu state in

        if debug_parser () then Format.printf "return %a@." Var.print x;
        instrs, Return x, state
    | RESTART -> assert false
    | GRAB -> assert false
    | CLOSURE ->
        let nvars = getu code (pc + 1) in
        let addr = pc + gets code (pc + 2) + 2 in
        let state = if nvars > 0 then State.push state else state in

        let vals, state = State.grab nvars state in
        let x, state = State.fresh_var state in
        let env = List.map vals ~f:(fun x -> State.Var x) in
        let env =
          let code = State.Dummy "closure(code)" in
          let closure_info = State.Dummy "closure(info)" in
          if new_closure_repr then code :: closure_info :: env else code :: env
        in
        let env = Array.of_list env in
        if debug_parser () then Format.printf "fun %a (" Var.print x;
        let nparams, addr =
          match (get_instr_exn code addr).Instr.code with
          | GRAB -> getu code (addr + 1) + 1, addr + 2
          | _ -> 1, addr
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
        let rec_names = ref (Debug.find_rec infos.debug (pc + 3 + gets code (pc + 3))) in
        for i = 0 to nfuncs - 1 do
          let x, st = State.fresh_var !state in
          (match !rec_names with
          | (j, ident) :: rest ->
              assert (j = i);
              Var.name x (Ident.name ident);
              rec_names := rest
          | [] -> ());
          vars := (i, x) :: !vars;
          state := State.push st
        done;
        let env = ref (List.map vals ~f:(fun x -> State.Var x)) in
        List.iter !vars ~f:(fun (i, x) ->
            let code = State.Var x in
            let closure_info = State.Dummy "closurerec(info)" in
            if new_closure_repr
            then env := code :: closure_info :: !env
            else env := code :: !env;
            if i > 0
            then
              let infix_tag = State.Dummy "closurerec(infix_tag)" in
              env := infix_tag :: !env);
        let env = Array.of_list !env in
        let state = !state in
        let instrs =
          List.fold_left (List.rev !vars) ~init:instrs ~f:(fun instr (i, x) ->
              let addr = pc + 3 + gets code (pc + 3 + i) in
              if debug_parser () then Format.printf "fun %a (" Var.print x;
              let nparams, addr =
                match (get_instr_exn code addr).Instr.code with
                | GRAB -> getu code (addr + 1) + 1, addr + 2
                | _ -> 1, addr
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
        compile infos (pc + 3) state (Let (y, Field (x, j, Non_float)) :: instrs)
    | PUSHGETGLOBALFIELD ->
        let state = State.push state in

        let i = getu code (pc + 1) in
        let x, state, instrs = get_global state instrs i in
        let j = getu code (pc + 2) in
        let y, state = State.fresh_var state in
        if debug_parser () then Format.printf "%a = %a[%d]@." Var.print y Var.print x j;
        compile infos (pc + 3) state (Let (y, Field (x, j, Non_float)) :: instrs)
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
        compile infos (pc + 2) state (Let (x, const 0) :: instrs)
    | ATOM0 ->
        let x, state = State.fresh_var state in

        if debug_parser () then Format.printf "%a = ATOM(0)@." Var.print x;
        compile
          infos
          (pc + 1)
          state
          (Let (x, Block (0, [||], Unknown, Maybe_mutable)) :: instrs)
    | ATOM ->
        let i = getu code (pc + 1) in
        let x, state = State.fresh_var state in

        if debug_parser () then Format.printf "%a = ATOM(%d)@." Var.print x i;
        compile
          infos
          (pc + 2)
          state
          (Let (x, Block (i, [||], Unknown, Maybe_mutable)) :: instrs)
    | PUSHATOM0 ->
        let state = State.push state in
        let x, state = State.fresh_var state in

        if debug_parser () then Format.printf "%a = ATOM(0)@." Var.print x;
        compile
          infos
          (pc + 1)
          state
          (Let (x, Block (0, [||], Unknown, Maybe_mutable)) :: instrs)
    | PUSHATOM ->
        let state = State.push state in

        let i = getu code (pc + 1) in
        let x, state = State.fresh_var state in
        if debug_parser () then Format.printf "%a = ATOM(%d)@." Var.print x i;
        compile
          infos
          (pc + 2)
          state
          (Let (x, Block (i, [||], Unknown, Maybe_mutable)) :: instrs)
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
          (Let (x, Block (tag, Array.of_list contents, Unknown, Maybe_mutable)) :: instrs)
    | MAKEBLOCK1 ->
        let tag = getu code (pc + 1) in
        let y = State.accu state in
        let x, state = State.fresh_var state in

        if debug_parser () then Format.printf "%a = { 0 = %a; }@." Var.print x Var.print y;
        compile
          infos
          (pc + 2)
          state
          (Let (x, Block (tag, [| y |], Unknown, Maybe_mutable)) :: instrs)
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
          (Let (x, Block (tag, [| y; z |], Unknown, Maybe_mutable)) :: instrs)
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
          (Let (x, Block (tag, [| y; z; t |], Unknown, Maybe_mutable)) :: instrs)
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
          (Let (x, Block (254, Array.of_list contents, Unknown, Maybe_mutable)) :: instrs)
    | GETFIELD0 ->
        let y = State.accu state in
        let x, state = State.fresh_var state in

        if debug_parser () then Format.printf "%a = %a[0]@." Var.print x Var.print y;
        compile infos (pc + 1) state (Let (x, Field (y, 0, Non_float)) :: instrs)
    | GETFIELD1 ->
        let y = State.accu state in
        let x, state = State.fresh_var state in

        if debug_parser () then Format.printf "%a = %a[1]@." Var.print x Var.print y;
        compile infos (pc + 1) state (Let (x, Field (y, 1, Non_float)) :: instrs)
    | GETFIELD2 ->
        let y = State.accu state in
        let x, state = State.fresh_var state in

        if debug_parser () then Format.printf "%a = %a[2]@." Var.print x Var.print y;
        compile infos (pc + 1) state (Let (x, Field (y, 2, Non_float)) :: instrs)
    | GETFIELD3 ->
        let y = State.accu state in
        let x, state = State.fresh_var state in

        if debug_parser () then Format.printf "%a = %a[3]@." Var.print x Var.print y;
        compile infos (pc + 1) state (Let (x, Field (y, 3, Non_float)) :: instrs)
    | GETFIELD ->
        let y = State.accu state in
        let n = getu code (pc + 1) in
        let x, state = State.fresh_var state in

        if debug_parser () then Format.printf "%a = %a[%d]@." Var.print x Var.print y n;
        compile infos (pc + 2) state (Let (x, Field (y, n, Non_float)) :: instrs)
    | GETFLOATFIELD ->
        let y = State.accu state in
        let n = getu code (pc + 1) in
        let x, state = State.fresh_var state in

        if debug_parser ()
        then Format.printf "%a = FLOAT{%a[%d]}@." Var.print x Var.print y n;
        compile infos (pc + 2) state (Let (x, Field (y, n, Float)) :: instrs)
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
          (Let (x, const 0) :: Set_field (y, 0, Non_float, z) :: instrs)
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
          (Let (x, const 0) :: Set_field (y, 1, Non_float, z) :: instrs)
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
          (Let (x, const 0) :: Set_field (y, 2, Non_float, z) :: instrs)
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
          (Let (x, const 0) :: Set_field (y, 3, Non_float, z) :: instrs)
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
          (Let (x, const 0) :: Set_field (y, n, Non_float, z) :: instrs)
    | SETFLOATFIELD ->
        let y = State.accu state in
        let z = State.peek 0 state in
        let n = getu code (pc + 1) in

        if debug_parser ()
        then Format.printf "FLOAT{%a[%d]} = %a@." Var.print y n Var.print z;
        let x, state = State.fresh_var state in
        if debug_parser () then Format.printf "%a = 0@." Var.print x;
        compile
          infos
          (pc + 2)
          (State.pop 1 state)
          (Let (x, const 0) :: Set_field (y, n, Float, z) :: instrs)
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
        compile infos (pc + 1) (State.pop 2 state) (Let (x, const 0) :: instrs)
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
        compile infos (pc + 1) (State.pop 2 state) (Let (x, const 0) :: instrs)
    | BRANCH ->
        let offset = gets code (pc + 1) in
        if debug_parser () then Format.printf "... (branch)@.";
        instrs, Branch (pc + offset + 1, []), state
    | BRANCHIF ->
        let offset = gets code (pc + 1) in
        let x = State.accu state in
        instrs, Cond (x, (pc + offset + 1, []), (pc + 2, [])), state
    | BRANCHIFNOT ->
        let offset = gets code (pc + 1) in
        let x = State.accu state in
        instrs, Cond (x, (pc + 2, []), (pc + offset + 1, [])), state
    | SWITCH -> (
        if debug_parser () then Format.printf "switch ...@.";
        let sz = getu code (pc + 1) in
        let x = State.accu state in
        let isize = sz land 0XFFFF in
        let bsize = sz lsr 16 in
        let base = pc + 2 in
        let it = Array.init isize ~f:(fun i -> base + gets code (base + i)) in
        let bt = Array.init bsize ~f:(fun i -> base + gets code (base + isize + i)) in
        Array.iter it ~f:(fun pc' ->
            compile_block infos.blocks infos.debug code pc' state);
        Array.iter bt ~f:(fun pc' ->
            compile_block infos.blocks infos.debug code pc' state);
        match isize, bsize with
        | _, 0 -> instrs, Switch (x, Array.map it ~f:(fun pc -> pc, [])), state
        | 0, _ ->
            let x_tag = Var.fresh () in
            let instrs =
              Let (x_tag, Prim (Extern "%direct_obj_tag", [ Pv x ])) :: instrs
            in
            instrs, Switch (x_tag, Array.map bt ~f:(fun pc -> pc, [])), state
        | _, _ ->
            let isint_branch = pc + 1 in
            let isblock_branch = pc + 2 in
            let () =
              tagged_blocks := Addr.Map.add isint_branch state !tagged_blocks;
              let i_state = State.start_block isint_branch state in
              let i_args = State.stack_vars i_state in
              compiled_blocks :=
                Addr.Map.add
                  isint_branch
                  (i_state, [], Switch (x, Array.map it ~f:(fun pc -> pc, i_args)))
                  !compiled_blocks
            in
            let () =
              tagged_blocks := Addr.Map.add isblock_branch state !tagged_blocks;
              let x_tag = Var.fresh () in
              let b_state = State.start_block isblock_branch state in
              let b_args = State.stack_vars b_state in
              let instrs = [ Let (x_tag, Prim (Extern "%direct_obj_tag", [ Pv x ])) ] in
              compiled_blocks :=
                Addr.Map.add
                  isblock_branch
                  (b_state, instrs, Switch (x_tag, Array.map bt ~f:(fun pc -> pc, b_args)))
                  !compiled_blocks
            in
            let isint_var = Var.fresh () in
            let instrs = Let (isint_var, Prim (IsInt, [ Pv x ])) :: instrs in
            instrs, Cond (isint_var, (isint_branch, []), (isblock_branch, [])), state)
    | BOOLNOT ->
        let y = State.accu state in
        let x, state = State.fresh_var state in
        if debug_parser () then Format.printf "%a = !%a@." Var.print x Var.print y;
        compile infos (pc + 1) state (Let (x, Prim (Not, [ Pv y ])) :: instrs)
    | PUSHTRAP ->
        (* We insert an intermediate block that binds the handler's
           context, so that it is also in the scope of the body. Then,
           when a mutable variable is assigned in the body, we can
           update this context. *)
        let interm_addr = pc + 1 in
        let handler_ctx_state = State.start_block interm_addr state in
        let body_addr = pc + 2 in
        let handler_addr = pc + 1 + gets code (pc + 1) in
        let x, handler_state = State.fresh_var handler_ctx_state in

        tagged_blocks := Addr.Map.add interm_addr state !tagged_blocks;
        compiled_blocks :=
          Addr.Map.add
            interm_addr
            ( handler_ctx_state
            , []
            , Pushtrap
                ( (body_addr, State.stack_vars state)
                , x
                , (handler_addr, State.stack_vars handler_state) ) )
            !compiled_blocks;
        compile_block infos.blocks infos.debug code handler_addr handler_state;
        compile_block
          infos.blocks
          infos.debug
          code
          body_addr
          { (State.push_handler handler_ctx_state) with
            State.stack =
              (* See interp.c *)
              State.Dummy "pushtrap(pc)"
              :: State.Dummy "pushtrap(sp_off)"
              :: State.Dummy "pushtrap(env)"
              :: State.Dummy "pushtrap(extra_args)"
              :: state.State.stack
          };
        instrs, Branch (interm_addr, []), state
    | POPTRAP ->
        let addr = pc + 1 in
        compile_block
          infos.blocks
          infos.debug
          code
          addr
          (State.pop 4 (State.pop_handler state));
        instrs, Poptrap (addr, []), state
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
          Format.printf "%a = ccall \"%s\" (" Var.print x prim;
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
          Format.printf "%a = ccall \"%s\" (" Var.print x prim;
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
          Format.printf "%a = ccall \"%s\" (" Var.print x prim;
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
          | CONST0 -> 0
          | CONST1 -> 1
          | CONST2 -> 2
          | CONST3 -> 3
          | _ -> assert false
        in

        if debug_parser () then Format.printf "%a = %d@." Var.print x n;
        compile infos (pc + 1) state (Let (x, const n) :: instrs)
    | CONSTINT ->
        let n = gets32 code (pc + 1) in
        let x, state = State.fresh_var state in

        if debug_parser () then Format.printf "%a = %ld@." Var.print x n;
        compile infos (pc + 2) state (Let (x, const32 n) :: instrs)
    | (PUSHCONST0 | PUSHCONST1 | PUSHCONST2 | PUSHCONST3) as cc ->
        let state = State.push state in
        let x, state = State.fresh_var state in
        let n =
          match cc with
          | PUSHCONST0 -> 0
          | PUSHCONST1 -> 1
          | PUSHCONST2 -> 2
          | PUSHCONST3 -> 3
          | _ -> assert false
        in

        if debug_parser () then Format.printf "%a = %d@." Var.print x n;
        compile infos (pc + 1) state (Let (x, const n) :: instrs)
    | PUSHCONSTINT ->
        let state = State.push state in
        let n = gets32 code (pc + 1) in
        let x, state = State.fresh_var state in

        if debug_parser () then Format.printf "%a = %ld@." Var.print x n;
        compile infos (pc + 2) state (Let (x, const32 n) :: instrs)
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
          (Let (x, Prim (Extern "%int_add", [ Pv y; Pv z ]))
          :: Let (z, const32 n)
          :: instrs)
    | OFFSETREF ->
        let n = gets code (pc + 1) in
        let x = State.accu state in

        if debug_parser () then Format.printf "%a += %d@." Var.print x n;
        let instrs = Offset_ref (x, n) :: instrs in
        let x, state = State.fresh_var state in
        if debug_parser () then Format.printf "x = 0@.";
        compile infos (pc + 2) state (Let (x, const 0) :: instrs)
    | ISINT ->
        let y = State.accu state in
        let x, state = State.fresh_var state in

        if debug_parser () then Format.printf "%a = !%a@." Var.print x Var.print y;
        compile infos (pc + 1) state (Let (x, Prim (IsInt, [ Pv y ])) :: instrs)
    | BEQ ->
        let n = gets32 code (pc + 1) in
        let offset = gets code (pc + 2) in
        let x = State.accu state in
        let y = Var.fresh () in

        ( Let (y, Prim (Eq, [ Pc (Int (Targetint.of_int32_exn n)); Pv x ])) :: instrs
        , Cond (y, (pc + offset + 2, []), (pc + 3, []))
        , state )
    | BNEQ ->
        let n = gets32 code (pc + 1) in
        let offset = gets code (pc + 2) in
        let x = State.accu state in
        let y = Var.fresh () in

        ( Let (y, Prim (Eq, [ Pc (Int (Targetint.of_int32_exn n)); Pv x ])) :: instrs
        , Cond (y, (pc + 3, []), (pc + offset + 2, []))
        , state )
    | BLTINT ->
        let n = gets32 code (pc + 1) in
        let offset = gets code (pc + 2) in
        let x = State.accu state in
        let y = Var.fresh () in

        ( Let (y, Prim (Lt, [ Pc (Int (Targetint.of_int32_exn n)); Pv x ])) :: instrs
        , Cond (y, (pc + offset + 2, []), (pc + 3, []))
        , state )
    | BLEINT ->
        let n = gets32 code (pc + 1) in
        let offset = gets code (pc + 2) in
        let x = State.accu state in
        let y = Var.fresh () in

        ( Let (y, Prim (Le, [ Pc (Int (Targetint.of_int32_exn n)); Pv x ])) :: instrs
        , Cond (y, (pc + offset + 2, []), (pc + 3, []))
        , state )
    | BGTINT ->
        let n = gets32 code (pc + 1) in
        let offset = gets code (pc + 2) in
        let x = State.accu state in
        let y = Var.fresh () in

        ( Let (y, Prim (Le, [ Pc (Int (Targetint.of_int32_exn n)); Pv x ])) :: instrs
        , Cond (y, (pc + 3, []), (pc + offset + 2, []))
        , state )
    | BGEINT ->
        let n = gets32 code (pc + 1) in
        let offset = gets code (pc + 2) in
        let x = State.accu state in
        let y = Var.fresh () in

        ( Let (y, Prim (Lt, [ Pc (Int (Targetint.of_int32_exn n)); Pv x ])) :: instrs
        , Cond (y, (pc + 3, []), (pc + offset + 2, []))
        , state )
    | BULTINT ->
        let n = getu32 code (pc + 1) in
        let offset = gets code (pc + 2) in
        let x = State.accu state in
        let y = Var.fresh () in

        ( Let (y, Prim (Ult, [ Pc (Int (Targetint.of_int32_exn n)); Pv x ])) :: instrs
        , Cond (y, (pc + offset + 2, []), (pc + 3, []))
        , state )
    | BUGEINT ->
        let n = getu32 code (pc + 1) in
        let offset = gets code (pc + 2) in
        let x = State.accu state in
        let y = Var.fresh () in
        ( Let (y, Prim (Ult, [ Pc (Int (Targetint.of_int32_exn n)); Pv x ])) :: instrs
        , Cond (y, (pc + 3, []), (pc + offset + 2, []))
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
                 , [ Pv obj; Pv tag; Pc (Int (Targetint.of_int_exn cache)) ] ) )
          :: Let (tag, const32 n)
          :: instrs)
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
          (Let
             ( m
             , Prim
                 ( Extern "caml_get_public_method"
                 , [ Pv obj; Pv tag; Pc (Int Targetint.zero) ] ) )
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
          :: Let (meths, Field (obj, 0, Non_float))
          :: instrs)
    | STOP -> instrs, Stop, state
    | RESUME ->
        let stack = State.accu state in
        let func = State.peek 0 state in
        let arg = State.peek 1 state in
        let x, state = State.fresh_var state in

        if debug_parser ()
        then
          Format.printf
            "%a = resume(%a, %a, %a)@."
            Var.print
            x
            Var.print
            stack
            Var.print
            func
            Var.print
            arg;
        let state =
          match Ocaml_version.compare Ocaml_version.current [ 5; 2 ] < 0 with
          | true -> State.pop 2 state
          | false -> State.pop 3 state
        in

        compile
          infos
          (pc + 1)
          state
          (Let (x, Prim (Extern "%resume", [ Pv stack; Pv func; Pv arg ])) :: instrs)
    | RESUMETERM ->
        let stack = State.accu state in
        let func = State.peek 0 state in
        let arg = State.peek 1 state in
        let x, state = State.fresh_var state in

        if debug_parser ()
        then
          Format.printf
            "return resume(%a, %a, %a)@."
            Var.print
            stack
            Var.print
            func
            Var.print
            arg;
        ( Let (x, Prim (Extern "%resume", [ Pv stack; Pv func; Pv arg ])) :: instrs
        , Return x
        , state )
    | PERFORM ->
        let eff = State.accu state in
        let x, state = State.fresh_var state in

        if debug_parser ()
        then Format.printf "%a = perform(%a)@." Var.print x Var.print eff;
        compile
          infos
          (pc + 1)
          state
          (Let (x, Prim (Extern "%perform", [ Pv eff ])) :: instrs)
    | REPERFORMTERM ->
        let eff = State.accu state in
        let stack = State.peek 0 state in
        (* We don't need [State.peek 1 state] *)
        let state = State.pop 2 state in
        let x, state = State.fresh_var state in

        if debug_parser ()
        then Format.printf "return reperform(%a, %a)@." Var.print eff Var.print stack;
        ( Let (x, Prim (Extern "%reperform", [ Pv eff; Pv stack ])) :: instrs
        , Return x
        , state )
    | EVENT | BREAK | FIRST_UNIMPLEMENTED_OP -> assert false)

(****)

type one =
  { code : Code.program
  ; cmis : StringSet.t
  ; debug : Debug.t
  }

let parse_bytecode code globals debug_data =
  let state = State.initial globals in
  Code.Var.reset ();
  let blocks' = Blocks.analyse code in
  let p =
    if not (Blocks.is_empty blocks')
    then (
      let start = 0 in
      compile_block blocks' debug_data code start state;
      let blocks =
        Addr.Map.mapi
          (fun _ (state, instr, last) ->
            { params = State.stack_vars state; body = instr; branch = last })
          !compiled_blocks
      in
      let free_pc = String.length code / 4 in
      { start; blocks; free_pc })
    else Code.empty
  in
  compiled_blocks := Addr.Map.empty;
  tagged_blocks := Addr.Map.empty;
  p

(* HACK - override module *)

let override_global =
  match Ocaml_version.compare Ocaml_version.current [ 4; 13 ] >= 0 with
  | true -> []
  | false ->
      [ ( "CamlinternalMod"
        , fun _orig instrs ->
            let x = Var.fresh_n "internalMod" in
            let init_mod = Var.fresh_n "init_mod" in
            let update_mod = Var.fresh_n "update_mod" in
            ( x
            , Let (x, Block (0, [| init_mod; update_mod |], NotArray, Immutable))
              :: Let (init_mod, Special (Alias_prim "caml_CamlinternalMod_init_mod"))
              :: Let (update_mod, Special (Alias_prim "caml_CamlinternalMod_update_mod"))
              :: instrs ) )
      ]

(* HACK END *)

module Toc : sig
  type t

  val read : in_channel -> t

  val seek_section : t -> in_channel -> string -> int

  val read_code : t -> in_channel -> string

  val read_data : t -> in_channel -> Obj.t array

  val read_crcs : t -> in_channel -> (string * Digest.t option) list

  val read_prim : t -> in_channel -> string

  val read_symb : t -> in_channel -> Ocaml_compiler.Symtable.GlobalMap.t
end = struct
  type t = (string * int) list

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

  let read ic =
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

  let read_code toc ic =
    let code_size = seek_section toc ic "CODE" in
    really_input_string ic code_size

  let read_data toc ic =
    ignore (seek_section toc ic "DATA");
    let init_data : Obj.t array = input_value ic in
    init_data

  let read_symb toc ic =
    ignore (seek_section toc ic "SYMB");
    let orig_symbols : Ocaml_compiler.Symtable.GlobalMap.t = input_value ic in
    orig_symbols

  let read_crcs toc ic =
    ignore (seek_section toc ic "CRCS");
    let orig_crcs : (string * Digest.t option) list = input_value ic in
    orig_crcs

  let read_prim toc ic =
    let prim_size = seek_section toc ic "PRIM" in
    let prim = really_input_string ic prim_size in
    prim
end

let read_primitives toc ic =
  let prim = Toc.read_prim toc ic in
  assert (Char.equal (String.get prim (String.length prim - 1)) '\000');
  String.split_char ~sep:'\000' (String.sub prim ~pos:0 ~len:(String.length prim - 1))

type bytesections =
  { symb : Ocaml_compiler.Symtable.GlobalMap.t
  ; crcs : (string * Digest.t option) list
  ; prim : string list
  ; dlpt : string list
  }
[@@ocaml.warning "-unused-field"]

let from_exe
    ?(includes = [])
    ~linkall
    ~link_info
    ~include_cmis
    ?exported_unit
    ?(debug = false)
    ic =
  let debug_data = Debug.create ~include_cmis debug in
  let toc = Toc.read ic in
  let primitives = read_primitives toc ic in
  let primitive_table = Array.of_list primitives in
  let code = Toc.read_code toc ic in
  let init_data = Toc.read_data toc ic in
  let init_data = Array.map ~f:Constants.parse init_data in
  let orig_symbols = Toc.read_symb toc ic in
  let orig_crcs = Toc.read_crcs toc ic in
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
    Ocaml_compiler.Symtable.GlobalMap.filter
      (function
        | Glob_predef _ -> true
        | Glob_compunit name -> keep name)
      orig_symbols
  in
  let t = Timer.make () in
  (if Debug.dbg_section_needed debug_data
   then
     try
       ignore (Toc.seek_section toc ic "DBUG");
       Debug.read debug_data ~crcs ~includes ic
     with Not_found ->
       if Debug.enabled debug_data || include_cmis
       then
         warn
           "Warning: Program not linked with -g, original variable names and locations \
            not available.@.");
  if times () then Format.eprintf "    read debug events: %a@." Timer.print t;

  let globals = make_globals (Array.length init_data) init_data primitive_table in
  (* Initialize module override mechanism *)
  List.iter override_global ~f:(fun (name, v) ->
      try
        let nn = Ocaml_compiler.Symtable.Global.Glob_compunit name in
        let i = Ocaml_compiler.Symtable.GlobalMap.find nn orig_symbols in
        globals.override.(i) <- Some v;
        if debug_parser () then Format.eprintf "overriding global %s@." name
      with Not_found -> ());
  if linkall
  then
    (* export globals *)
    Ocaml_compiler.Symtable.GlobalMap.iter symbols ~f:(fun id n ->
        globals.named_value.(n) <- Some (Ocaml_compiler.Symtable.Global.name id);
        globals.is_exported.(n) <- true);
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
    if link_info
    then
      let symbols_array =
        Ocaml_compiler.Symtable.GlobalMap.fold
          (fun i p acc -> (Ocaml_compiler.Symtable.Global.name i, p) :: acc)
          symbols
          []
        |> Array.of_list
      in
      (* Include linking information *)
      let sections = { symb = symbols; crcs; prim = primitives; dlpt = [] } in
      let gdata = Var.fresh () in
      let need_gdata = ref false in
      let infos =
        [ "sections", Constants.parse (Obj.repr sections)
        ; "symbols", Constants.parse (Obj.repr symbols_array)
        ; "prim_count", Int (Targetint.of_int_exn (Array.length globals.primitives))
        ]
      in
      let body =
        List.fold_left infos ~init:body ~f:(fun rem (name, const) ->
            assert (String.is_valid_utf_8 name);
            need_gdata := true;
            let c = Var.fresh () in
            Let (c, Constant const)
            :: Let
                 ( Var.fresh ()
                 , Prim
                     ( Extern "caml_js_set"
                     , [ Pv gdata
                       ; Pc (NativeString (Code.Native_string.of_string name))
                       ; Pv c
                       ] ) )
            :: rem)
      in
      if !need_gdata
      then Let (gdata, Prim (Extern "caml_get_global_data", [])) :: body
      else body
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
    if include_cmis
    then
      Ocaml_compiler.Symtable.GlobalMap.fold
        (fun id _num acc ->
          match id with
          | Ocaml_compiler.Symtable.Global.Glob_compunit name ->
              if is_module name then StringSet.add name acc else acc
          | Glob_predef _ -> acc)
        symbols
        StringSet.empty
    else StringSet.empty
  in
  let cmis =
    match exported_unit with
    | None -> cmis
    | Some l ->
        if include_cmis
        then List.fold_left l ~init:cmis ~f:(fun acc s -> StringSet.add s acc)
        else cmis
  in
  let code = prepend p body in
  Code.invariant code;
  { code; cmis; debug = debug_data }

(* As input: list of primitives + size of global table *)
let from_bytes ~prims ~debug (code : bytecode) =
  let debug_data = Debug.create ~include_cmis:false true in
  let t = Timer.make () in
  if Debug.names debug_data
  then
    Array.iter debug ~f:(fun l ->
        List.iter l ~f:(fun ev ->
            Debug.read_event ~paths:[] ~crcs:(Hashtbl.create 17) ~orig:0 debug_data ev));
  if times () then Format.eprintf "    read debug events: %a@." Timer.print t;
  let ident_table =
    let t = Hashtbl.create 17 in
    if Debug.names debug_data
    then
      Ocaml_compiler.Symtable.GlobalMap.iter
        (Ocaml_compiler.Symtable.current_state ())
        ~f:(fun id pos' -> Hashtbl.add t pos' id);
    t
  in
  let globals = make_globals 0 [||] prims in
  let p = parse_bytecode code globals debug_data in
  let gdata = Var.fresh_n "global_data" in
  let need_gdata = ref false in
  let find_name i =
    let value = (Meta.global_data ()).(i) in
    let tag = Obj.tag value in
    if tag = Obj.string_tag
    then Some ("cst_" ^ Obj.magic value : string)
    else
      match Hashtbl.find ident_table i with
      | exception Not_found -> None
      | glob -> Some (Ocaml_compiler.Symtable.Global.name glob)
  in
  let body =
    Array.fold_right_i globals.vars ~init:[] ~f:(fun i var l ->
        match var with
        | Some x when globals.is_const.(i) ->
            (if Debug.names debug_data
             then
               match find_name i with
               | None -> ()
               | Some name -> Code.Var.name x name);
            need_gdata := true;
            Let (x, Field (gdata, i, Non_float)) :: l
        | _ -> l)
  in
  let body =
    if !need_gdata
    then Let (gdata, Prim (Extern "caml_get_global_data", [])) :: body
    else body
  in
  prepend p body, debug_data

let from_string ~prims ~debug (code : string) = from_bytes ~prims ~debug code

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

  let constant_of_const x = Ocaml_compiler.constant_of_const x
  [@@if ocaml_version < (5, 1, 0)]

  let constant_of_const x = Constants.parse x [@@if ocaml_version >= (5, 1, 0)]

  (* We currently rely on constants to be relocated before globals. *)
  let step1 t compunit code =
    if t.step2_started then assert false;
    let open Cmo_format in
    List.iter compunit.cu_primitives ~f:(fun name ->
        Hashtbl.add t.primitives name (Hashtbl.length t.primitives));
    let slot_for_literal sc =
      t.constants <- constant_of_const sc :: t.constants;
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
    let next name =
      try Hashtbl.find t.names name
      with Not_found ->
        let pos = t.pos in
        t.pos <- succ t.pos;
        Hashtbl.add t.names name pos;
        pos
    in
    let slot_for_global id = next id in
    List.iter compunit.cu_reloc ~f:(fun (reloc, pos) ->
        let patch name = gen_patch_int code pos name in
        match reloc with
        | ((Reloc_getglobal id) [@if ocaml_version < (5, 2, 0)]) ->
            patch (slot_for_global (Ident.name id))
        | ((Reloc_setglobal id) [@if ocaml_version < (5, 2, 0)]) ->
            patch (slot_for_global (Ident.name id))
        | ((Reloc_getcompunit (Compunit id)) [@if ocaml_version >= (5, 2, 0)]) ->
            patch (slot_for_global id)
        | ((Reloc_getpredef (Predef_exn id)) [@if ocaml_version >= (5, 2, 0)]) ->
            patch (slot_for_global id)
        | ((Reloc_setcompunit (Compunit id)) [@if ocaml_version >= (5, 2, 0)]) ->
            patch (slot_for_global id)
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

let from_compilation_units ~includes:_ ~include_cmis ~debug_data l =
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
  let need_gdata = ref false in
  let body =
    Array.fold_right_i globals.vars ~init:[] ~f:(fun i var l ->
        match var with
        | Some x when globals.is_const.(i) -> (
            match globals.named_value.(i) with
            | None ->
                let l = register_global globals i l in
                let cst = globals.constants.(i) in
                (match cst, Code.Var.get_name x with
                | String str, None -> Code.Var.name x (Printf.sprintf "cst_%s" str)
                | _ -> ());
                Let (x, Constant cst) :: l
            | Some name ->
                Var.name x name;
                need_gdata := true;
                Let
                  ( x
                  , Prim
                      ( Extern "caml_js_get"
                      , [ Pv gdata; Pc (NativeString (Native_string.of_string name)) ] )
                  )
                :: l)
        | _ -> l)
  in
  let body =
    if !need_gdata
    then Let (gdata, Prim (Extern "caml_get_global_data", [])) :: body
    else body
  in
  let cmis =
    if include_cmis
    then
      List.fold_left l ~init:StringSet.empty ~f:(fun acc (compunit, _) ->
          StringSet.add (Ocaml_compiler.Cmo_format.name compunit) acc)
    else StringSet.empty
  in
  { code = prepend prog body; cmis; debug = debug_data }

let from_cmo ?(includes = []) ?(include_cmis = false) ?(debug = false) compunit ic =
  let debug_data = Debug.create ~include_cmis debug in
  seek_in ic compunit.Cmo_format.cu_pos;
  let code = Bytes.create compunit.Cmo_format.cu_codesize in
  really_input ic code 0 compunit.Cmo_format.cu_codesize;
  let t = Timer.make () in
  if Debug.dbg_section_needed debug_data && compunit.Cmo_format.cu_debug <> 0
  then (
    seek_in ic compunit.Cmo_format.cu_debug;
    Debug.read_event_list debug_data ~crcs:[] ~includes ~orig:0 ic);
  if times () then Format.eprintf "    read debug events: %a@." Timer.print t;
  let p = from_compilation_units ~includes ~include_cmis ~debug_data [ compunit, code ] in
  Code.invariant p.code;
  p

let from_cma ?(includes = []) ?(include_cmis = false) ?(debug = false) lib ic =
  let debug_data = Debug.create ~include_cmis debug in
  let orig = ref 0 in
  let t = ref 0. in
  let units =
    List.map lib.Cmo_format.lib_units ~f:(fun compunit ->
        seek_in ic compunit.Cmo_format.cu_pos;
        let code = Bytes.create compunit.Cmo_format.cu_codesize in
        really_input ic code 0 compunit.Cmo_format.cu_codesize;
        let t0 = Timer.make () in
        if Debug.dbg_section_needed debug_data && compunit.Cmo_format.cu_debug <> 0
        then (
          seek_in ic compunit.Cmo_format.cu_debug;
          Debug.read_event_list debug_data ~crcs:[] ~includes ~orig:!orig ic);
        t := !t +. Timer.get t0;
        orig := !orig + compunit.Cmo_format.cu_codesize;
        compunit, code)
  in
  if times () then Format.eprintf "    read debug events: %.2f@." !t;
  let p = from_compilation_units ~includes ~include_cmis ~debug_data units in
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
          if
            Config.Flag.check_magic ()
            && not (Magic_number.equal magic Magic_number.current_cmo)
          then raise Magic_number.(Bad_magic_version magic);
          let compunit_pos = input_binary_int ic in
          seek_in ic compunit_pos;
          let compunit : Cmo_format.compilation_unit = input_value ic in
          `Cmo compunit
      | `Cma ->
          if
            Config.Flag.check_magic ()
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
          if
            Config.Flag.check_magic ()
            && not (Magic_number.equal magic Magic_number.current_exe)
          then raise Magic_number.(Bad_magic_version magic);
          `Exe
      | _ -> raise Magic_number.(Bad_magic_number (to_string magic)))

let predefined_exceptions () =
  (* Register predefined exceptions in case of separate compilation *)
  let body =
    let open Code in
    List.map predefined_exceptions ~f:(fun (index, name) ->
        assert (String.is_valid_utf_8 name);
        let exn = Var.fresh () in
        let v_name = Var.fresh () in
        let v_index = Var.fresh () in
        [ Let (v_name, Constant (String name))
        ; Let
            ( v_index
            , Constant
                (Int
                   ((* Predefined exceptions are registered in
                       Symtable.init with [-index - 1] *)
                    Targetint.of_int_exn
                      (-index - 1))) )
        ; Let (exn, Block (248, [| v_name; v_index |], NotArray, Immutable))
        ]
        @
        match Config.target () with
        | `JavaScript ->
            let v_name_js = Var.fresh () in
            [ Let (v_name_js, Constant (NativeString (Native_string.of_string name)))
            ; Let
                ( Var.fresh ()
                , Prim
                    ( Extern "caml_register_global"
                    , [ Pc (Int (Targetint.of_int_exn index)); Pv exn; Pv v_name_js ] ) )
            ]
        | `Wasm ->
            [ Let
                ( Var.fresh ()
                , Prim
                    ( Extern "caml_register_global"
                    , [ Pc (Int (Targetint.of_int_exn index)); Pv exn; Pv v_name ] ) )
              (* Also make the exception available to the generated code *)
            ; Let
                ( Var.fresh ()
                , Prim (Extern "caml_set_global", [ Pc (String name); Pv exn ]) )
            ])
    |> List.concat
  in
  let block = { params = []; body; branch = Stop } in
  let unit_info =
    { Unit_info.provides = StringSet.of_list (List.map ~f:snd predefined_exceptions)
    ; requires = StringSet.empty
    ; crcs = StringMap.empty
    ; force_link = true
    ; effects_without_cps = false
    ; primitives = []
    }
  in
  { start = 0; blocks = Addr.Map.singleton 0 block; free_pc = 1 }, unit_info

let link_info ~symbols ~primitives ~crcs =
  let gdata = Code.Var.fresh_n "global_data" in
  let symbols_array =
    Ocaml_compiler.Symtable.GlobalMap.fold
      (fun i p acc -> (Ocaml_compiler.Symtable.Global.name i, p) :: acc)
      symbols
      []
    |> Array.of_list
  in
  let primitives =
    (* Add the externals translated by jsoo directly (in generate.ml) *)
    StringSet.union (Primitive.get_external ()) primitives |> StringSet.elements
  in
  let body = [] in
  let body =
    (* Include linking information *)
    let sections = { symb = symbols; crcs; prim = primitives; dlpt = [] } in
    let infos =
      [ "sections", Constants.parse (Obj.repr sections)
      ; "symbols", Constants.parse (Obj.repr symbols_array)
      ; "prim_count", Int (Targetint.of_int_exn (List.length primitives))
      ]
    in
    let body =
      List.fold_left infos ~init:body ~f:(fun rem (name, const) ->
          let c = Var.fresh () in
          Let (c, Constant const)
          :: Let
               ( Var.fresh ()
               , Prim
                   ( Extern "caml_js_set"
                   , [ Pv gdata; Pc (NativeString (Native_string.of_string name)); Pv c ]
                   ) )
          :: rem)
    in
    Let (gdata, Prim (Extern "caml_get_global_data", [])) :: body
  in
  let block = { params = []; body; branch = Stop } in
  { start = 0; blocks = Addr.Map.singleton 0 block; free_pc = 1 }
