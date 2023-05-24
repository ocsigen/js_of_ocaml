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

let new_closure_repr =
  match Ocaml_version.v with
  | `V4_08 | `V4_09 | `V4_10 | `V4_11 -> false
  | `V4_12 | `V4_13 | `V4_14 | `V5_00 | `V5_01 | `V5_02 -> true

(* Read and manipulate debug section *)
module Debug : sig
  type t

  type force =
    | Before
    | After
    | No

  val names : t -> bool

  val enabled : t -> bool

  val is_empty : t -> bool

  val dbg_section_needed : t -> bool

  val propagate : Code.Var.t list -> Code.Var.t list -> unit

  val find : t -> Code.Addr.t -> (int * Ident.t) list * Env.summary

  val find_rec : t -> Code.Addr.t -> (int * Ident.t) list

  val find_loc : t -> ?force:force -> Code.loc -> Parse_info.t option

  val find_loc' :
    t -> int -> (string option * Location.t * Instruct.debug_event_kind) option

  val find_source : t -> string -> string option

  val mem : t -> int -> bool

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

  val fold : t -> (Code.Addr.t -> Instruct.debug_event -> 'a -> 'a) -> 'a -> 'a

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

  module String_table = Hashtbl.Make (String)
  module Int_table = Hashtbl.Make (Int)

  type t =
    { events_by_pc : event_and_source Int_table.t
    ; units : (string * string option, ml_unit) Hashtbl.t
    ; pos_fname_to_source : string String_table.t
    ; names : bool
    ; enabled : bool
    ; include_cmis : bool
    }

  type force =
    | Before
    | After
    | No

  let names t = t.names

  let enabled t = t.enabled

  let dbg_section_needed t = t.names || t.enabled || t.include_cmis

  let relocate_event orig ev = ev.ev_pos <- (orig + ev.ev_pos) / 4

  let create ~include_cmis enabled =
    let names = enabled || Config.Flag.pretty () in
    { events_by_pc = Int_table.create 17
    ; units = Hashtbl.create 17
    ; pos_fname_to_source = String_table.create 17
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
      { events_by_pc; units; pos_fname_to_source; names; enabled; include_cmis = _ }
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
        (match pos_fname, source with
        | None, _ | _, None -> ()
        | Some pos_fname, Some source ->
            String_table.add pos_fname_to_source pos_fname source);
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

  let find_source { pos_fname_to_source; _ } pos_fname =
    match pos_fname with
    | "_none_" -> None
    | _ -> (
        match String_table.find_all pos_fname_to_source pos_fname with
        | [ x ] -> Some x
        | _ :: _ :: _ -> None
        | [] -> None)

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
        Ocaml_compiler.Ident.table_contents event.ev_compenv.ce_stack
        |> List.map ~f:(fun (i, ident) -> event.ev_stacksize - i, ident)
        |> List.sort ~cmp:(fun (i, _) (j, _) -> compare i j)
      in

      l, event.ev_typenv
    with Not_found -> [], Env.Env_empty

  let find_rec { events_by_pc; _ } pc =
    try
      let { event; _ } = Int_table.find events_by_pc pc in
      Ocaml_compiler.Ident.table_contents event.ev_compenv.ce_rec
      |> List.map ~f:(fun (i, ident) ->
             (if new_closure_repr then i / 3 else i / 2), ident)
      |> List.sort ~cmp:(fun (i, _) (j, _) -> compare i j)
    with Not_found -> []

  let mem { events_by_pc; _ } pc = Int_table.mem events_by_pc pc

  let find_loc' { events_by_pc; _ } pc =
    try
      let { event; source } = Int_table.find events_by_pc pc in
      let loc = event.ev_loc in
      Some (source, loc, event.ev_kind)
    with Not_found -> None

  let find_loc { events_by_pc; _ } ?(force = No) x =
    match x with
    | Code.No -> None
    | Code.Before pc | Code.After pc -> (
        try
          let { event; source } = Int_table.find events_by_pc pc in
          let loc = event.ev_loc in
          if loc.Location.loc_ghost
          then None
          else
            let pos =
              match force with
              | After -> loc.Location.loc_end
              | Before -> loc.Location.loc_start
              | No -> (
                  match x with
                  | Code.Before _ -> loc.Location.loc_start
                  | Code.After _ -> loc.Location.loc_end
                  | _ -> assert false)
            in
            Some (Parse_info.t_of_position ~src:source pos)
        with Not_found -> None)

  let rec propagate l1 l2 =
    match l1, l2 with
    | v1 :: r1, v2 :: r2 ->
        Var.propagate_name v1 v2;
        propagate r1 r2
    | _ -> ()

  let fold t f acc =
    Int_table.fold (fun k { event; _ } acc -> f k event acc) t.events_by_pc acc

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
      | KNullaryCall -> scan debug blocks code (pc + 1) len
      | KUnaryCall -> scan debug blocks code (pc + 2) len
      | KBinaryCall -> scan debug blocks code (pc + 3) len
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
      if Debug.enabled debug_data
      then debug_data
      else Debug.create ~include_cmis:false false
    in
    let blocks = Addr.Set.empty in
    let len = String.length code / 4 in
    let blocks = add blocks 0 in
    let blocks = add blocks len in
    scan debug_data blocks code 0 len
end

(* Parse constants *)
module Constants : sig
  val parse : target:[ `JavaScript | `Wasm ] -> Obj.t -> Code.constant

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

  let rec parse ~target x =
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
        | Some name when same_ident name ident_32 -> Int (Int32, (Obj.magic x : int32))
        | Some name when same_ident name ident_native ->
            let i : nativeint = Obj.magic x in
            Int
              ( Native
              , match target with
                | `JavaScript -> Int32.of_nativeint_warning_on_overflow i
                | `Wasm -> Int31.of_nativeint_warning_on_overflow i )
        | Some name when same_ident name ident_64 -> Int64 (Obj.magic x : int64)
        | Some name ->
            failwith
              (Printf.sprintf
                 "parse_bytecode: Don't know what to do with custom block (%s)"
                 name)
        | None -> assert false
      else if tag < Obj.no_scan_tag
      then
        Tuple
          ( tag
          , Array.init (Obj.size x) ~f:(fun i -> parse ~target (Obj.field x i))
          , Unknown )
      else assert false
    else
      let i : int = Obj.magic x in
      Int
        ( Regular
        , match target with
          | `JavaScript -> Int32.of_int_warning_on_overflow i
          | `Wasm -> Int31.of_int_warning_on_overflow i )

  let inlined = function
    | String _ | NativeString _ -> false
    | Float _ -> true
    | Float_array _ -> false
    | Int64 _ -> false
    | Tuple _ -> false
    | Int _ -> true
end

let const i = Constant (Int (Regular, i))

(* Globals *)
type globals =
  { mutable vars : Var.t option array
  ; mutable is_const : bool array
  ; mutable is_exported : bool array
  ; mutable named_value : string option array
  ; mutable override :
      (Var.t -> (Code.instr * Code.loc) list -> Var.t * (Code.instr * Code.loc) list)
      option
      array
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
    | Var of Var.t * loc
    | Dummy

  let elt_to_var e =
    match e with
    | Var (x, loc) -> x, loc
    | _ -> assert false

  let print_elt f v =
    match v with
    | Var (x, _) -> Format.fprintf f "%a" Var.print x
    | Dummy -> Format.fprintf f "???"

  type handler =
    { block_pc : Addr.t
    ; stack : elt list
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

  let fresh_var state loc =
    let x = Var.fresh () in
    x, { state with accu = Var (x, loc) }

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

  let push st loc =
    match loc with
    | No -> { st with stack = st.accu :: st.stack }
    | _ ->
        { st with
          stack =
            (match st.accu with
            | Dummy -> Dummy
            | Var (x, _) -> Var (x, loc))
            :: st.stack
        }

  let pop n st = { st with stack = st_pop n st.stack }

  let acc n st loc =
    match loc with
    | No -> { st with accu = List.nth st.stack n }
    | _ ->
        { st with
          accu =
            (match List.nth st.stack n with
            | Dummy -> Dummy
            | Var (x, _) -> Var (x, loc))
        }

  let env_acc n st = { st with accu = st.env.(st.env_offset + n) }

  let accu st = elt_to_var st.accu

  let stack_vars st =
    List.fold_left (st.accu :: st.stack) ~init:[] ~f:(fun l e ->
        match e with
        | Var (x, _) -> x :: l
        | Dummy -> l)

  let set_accu st x loc = { st with accu = Var (x, loc) }

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
          | Var (x, l) ->
              let y = Var.fork x in
              Var (y, l) :: stack)
    in
    let state = { state with stack; current_pc } in
    match state.accu with
    | Dummy -> state
    | Var (x, loc) ->
        let y, state = fresh_var state loc in
        Var.propagate_name x y;
        state

  let push_handler state =
    { state with
      handlers = { block_pc = state.current_pc; stack = state.stack } :: state.handlers
    }

  let pop_handler state = { state with handlers = List.tl state.handlers }

  let addr_of_current_handler state =
    match state.handlers with
    | [] -> assert false
    | x :: _ -> x.block_pc

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
    | (j, ident) :: lrem, Var (v, _) :: srem when i = j ->
        (match Ocaml_compiler.find_loc_in_summary ident summary with
        | None -> ()
        | Some loc -> Var.loc v (pi_of_loc debug loc));
        Var.name v (Ident.name ident);
        name_rec debug (i + 1) lrem srem summary
    | (j, _) :: _, _ :: srem when i < j -> name_rec debug (i + 1) l srem summary
    | _ -> assert false

  let name_vars st debug pc =
    if Debug.names debug
    then
      let l, summary = Debug.find debug pc in
      name_rec debug 0 l st.stack summary

  let rec make_stack i state loc =
    if i = 0
    then [], state
    else
      let x, state = fresh_var state loc in
      let params, state = make_stack (pred i) (push state loc) loc in
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

let register_global ?(force = false) g i loc rem =
  if force || g.is_exported.(i)
  then
    let args =
      match g.named_value.(i) with
      | None -> []
      | Some name ->
          Code.Var.name (access_global g i) name;
          [ Pc (NativeString (Native_string.of_string name)) ]
    in
    ( Let
        ( Var.fresh ()
        , Prim
            ( Extern "caml_register_global"
            , Pc (Int (Regular, Int32.of_int i)) :: Pv (access_global g i) :: args ) )
    , loc )
    :: rem
  else rem

let get_global state instrs i loc =
  State.size_globals state (i + 1);
  let g = State.globals state in
  match g.vars.(i) with
  | Some x ->
      if debug_parser () then Format.printf "(global access %a)@." Var.print x;
      x, State.set_accu state x loc, instrs
  | None ->
      if i < Array.length g.constants && Constants.inlined g.constants.(i)
      then
        let x, state = State.fresh_var state loc in
        let cst = g.constants.(i) in
        x, state, (Let (x, Constant cst), loc) :: instrs
      else (
        g.is_const.(i) <- true;
        let x, state = State.fresh_var state loc in
        if debug_parser () then Format.printf "%a = CONST(%d)@." Var.print x i;
        g.vars.(i) <- Some x;
        x, state, instrs)

let tagged_blocks = ref Addr.Set.empty

let compiled_blocks = ref Addr.Map.empty

let pushpop = ref Addr.Map.empty

let method_cache_id = ref 1

let clo_offset_3 = if new_closure_repr then 3 else 2

type compile_info =
  { blocks : Blocks.u
  ; code : string
  ; limit : int
  ; debug : Debug.t
  }

let string_of_addr debug_data addr =
  match Debug.find_loc' debug_data addr with
  | None -> None
  | Some (src, loc, kind) ->
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
      Some (Printf.sprintf "%s:%s-%s %s" file (pos loc.loc_start) (pos loc.loc_end) kind)

let ( ||| ) x y =
  match x with
  | No -> y
  | _ -> x

let rec compile_block blocks debug_data code pc state =
  if not (Addr.Set.mem pc !tagged_blocks)
  then (
    let limit = Blocks.next blocks pc in
    assert (limit > pc);
    if debug_parser () then Format.eprintf "Compiling from %d to %d@." pc (limit - 1);
    let state = State.start_block pc state in
    tagged_blocks := Addr.Set.add pc !tagged_blocks;
    let instr, last, state' =
      compile { blocks; code; limit; debug = debug_data } pc state []
    in
    assert (not (Addr.Map.mem pc !compiled_blocks));
    compiled_blocks := Addr.Map.add pc (state, List.rev instr, last) !compiled_blocks;
    match fst last with
    | Branch (pc', _) | Poptrap (pc', _) ->
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
  (if debug_parser ()
   then
     match string_of_addr infos.debug pc with
     | None -> ()
     | Some s -> Format.eprintf "@@@@ %s @@@@@." s);
  if pc = infos.limit
  then
    if (* stop if we reach end_of_code (ie when compiling cmo) *)
       pc = String.length infos.code / 4
    then (
      if debug_parser () then Format.eprintf "Stop@.";
      instrs, (Stop, noloc), state)
    else (
      State.name_vars state infos.debug pc;
      let stack = State.stack_vars state in
      if debug_parser () then Format.eprintf "Branch %d (%a) @." pc Print.var_list stack;
      instrs, (Branch (pc, stack), Code.noloc), state)
  else (
    if debug_parser () then Format.eprintf "%4d " pc;
    State.name_vars state infos.debug pc;
    let code = infos.code in
    let instr = get_instr_exn code pc in
    if debug_parser () then Format.eprintf "%08x %s@." instr.opcode instr.name;
    let loc =
      match instr.Instr.code with
      | APPLY
      | APPLY1
      | APPLY2
      | APPLY3
      | C_CALL1
      | C_CALL2
      | C_CALL3
      | C_CALL4
      | C_CALL5
      | C_CALLN
      | PERFORM
      | RESUME -> (
          let offset =
            match instr.Instr.kind with
            | KNullaryCall -> 1
            | KUnaryCall -> 2
            | KBinaryCall -> 3
            | _ -> assert false
          in
          match Debug.find_loc' infos.debug (pc + offset) with
          | Some (_, _, (Event_pseudo | Event_after _)) -> Code.Before (pc + offset)
          | Some _ | None -> if Debug.mem infos.debug pc then Code.Before pc else noloc)
      (* bytegen.ml insert a pseudo event after the following instruction *)
      | MAKEBLOCK | MAKEBLOCK1 | MAKEBLOCK2 | MAKEBLOCK3 | MAKEFLOATBLOCK | GETFLOATFIELD
        -> (
          let offset =
            match instr.Instr.kind with
            | KUnary -> 2
            | KBinary -> 3
            | _ -> assert false
          in
          match Debug.find_loc' infos.debug (pc + offset) with
          | Some (_, _, Event_pseudo) -> Code.Before (pc + offset)
          | Some _ | _ -> if Debug.mem infos.debug pc then Code.Before pc else noloc)
      | RAISE | RAISE_NOTRACE | RERAISE -> (
          match Debug.find_loc' infos.debug pc with
          | Some (_, _, _) -> Code.Before pc
          | None -> noloc)
      | _ -> (
          match Debug.find_loc' infos.debug pc with
          | Some (_, _, Event_after _) -> Code.Before pc
          | Some (_, _, (Event_pseudo | Event_before)) -> Code.Before pc
          | None -> noloc)
    in

    match instr.Instr.code with
    | ACC0 -> compile infos (pc + 1) (State.acc 0 state loc) instrs
    | ACC1 -> compile infos (pc + 1) (State.acc 1 state loc) instrs
    | ACC2 -> compile infos (pc + 1) (State.acc 2 state loc) instrs
    | ACC3 -> compile infos (pc + 1) (State.acc 3 state loc) instrs
    | ACC4 -> compile infos (pc + 1) (State.acc 4 state loc) instrs
    | ACC5 -> compile infos (pc + 1) (State.acc 5 state loc) instrs
    | ACC6 -> compile infos (pc + 1) (State.acc 6 state loc) instrs
    | ACC7 -> compile infos (pc + 1) (State.acc 7 state loc) instrs
    | ACC ->
        let n = getu code (pc + 1) in
        compile infos (pc + 2) (State.acc n state loc) instrs
    | PUSH -> compile infos (pc + 1) (State.push state loc) instrs
    | PUSHACC0 -> compile infos (pc + 1) (State.acc 0 (State.push state loc) loc) instrs
    | PUSHACC1 -> compile infos (pc + 1) (State.acc 1 (State.push state loc) loc) instrs
    | PUSHACC2 -> compile infos (pc + 1) (State.acc 2 (State.push state loc) loc) instrs
    | PUSHACC3 -> compile infos (pc + 1) (State.acc 3 (State.push state loc) loc) instrs
    | PUSHACC4 -> compile infos (pc + 1) (State.acc 4 (State.push state loc) loc) instrs
    | PUSHACC5 -> compile infos (pc + 1) (State.acc 5 (State.push state loc) loc) instrs
    | PUSHACC6 -> compile infos (pc + 1) (State.acc 6 (State.push state loc) loc) instrs
    | PUSHACC7 -> compile infos (pc + 1) (State.acc 7 (State.push state loc) loc) instrs
    | PUSHACC ->
        let n = getu code (pc + 1) in
        compile infos (pc + 2) (State.acc n (State.push state loc) loc) instrs
    | POP ->
        let n = getu code (pc + 1) in
        compile infos (pc + 2) (State.pop n state) instrs
    | ASSIGN ->
        let n = getu code (pc + 1) in
        let accu, _ = State.accu state in
        let state = State.assign state n in
        let stack_size = List.length state.stack in
        let x, state = State.fresh_var state loc in
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
            ~init:((Let (x, const 0l), loc) :: instrs)
            ~f:(fun acc (handler : State.handler) ->
              let handler_stack_size = List.length handler.stack in
              let diff = stack_size - handler_stack_size in
              if n >= diff
              then
                let dest, _ = State.elt_to_var (List.nth handler.stack (n - diff)) in
                (Assign (dest, accu), loc) :: acc
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
    | PUSHENVACC1 ->
        compile infos (pc + 1) (State.env_acc 1 (State.push state loc)) instrs
    | PUSHENVACC2 ->
        compile infos (pc + 1) (State.env_acc 2 (State.push state loc)) instrs
    | PUSHENVACC3 ->
        compile infos (pc + 1) (State.env_acc 3 (State.push state loc)) instrs
    | PUSHENVACC4 ->
        compile infos (pc + 1) (State.env_acc 4 (State.push state loc)) instrs
    | PUSHENVACC ->
        let n = getu code (pc + 1) in
        compile infos (pc + 2) (State.env_acc n (State.push state loc)) instrs
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
        let f, _ = State.accu state in
        let x, state = State.fresh_var state loc in
        let args, state = State.grab n state in

        if debug_parser ()
        then (
          Format.printf "%a = %a(" Var.print x Var.print f;
          for i = 0 to n - 1 do
            if i > 0 then Format.printf ", ";
            Format.printf "%a" Var.print (fst (List.nth args i))
          done;
          Format.printf ")@.");
        compile
          infos
          (pc + 2)
          (State.pop 3 state)
          ((Let (x, Apply { f; args = List.map ~f:fst args; exact = false }), loc)
          :: instrs)
    | APPLY1 ->
        let f, _ = State.accu state in
        let x, state = State.fresh_var state loc in
        let y, _ = State.peek 0 state in

        if debug_parser ()
        then Format.printf "%a = %a(%a)@." Var.print x Var.print f Var.print y;
        compile
          infos
          (pc + 1)
          (State.pop 1 state)
          ((Let (x, Apply { f; args = [ y ]; exact = false }), loc) :: instrs)
    | APPLY2 ->
        let f, _ = State.accu state in
        let x, state = State.fresh_var state loc in
        let y, _ = State.peek 0 state in
        let z, _ = State.peek 1 state in

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
          ((Let (x, Apply { f; args = [ y; z ]; exact = false }), loc) :: instrs)
    | APPLY3 ->
        let f, _ = State.accu state in
        let x, state = State.fresh_var state loc in
        let y, _ = State.peek 0 state in
        let z, _ = State.peek 1 state in
        let t, _ = State.peek 2 state in

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
          ((Let (x, Apply { f; args = [ y; z; t ]; exact = false }), loc) :: instrs)
    | APPTERM ->
        let n = getu code (pc + 1) in
        let f, loc_f = State.accu state in
        let l, state = State.grab n state in

        if debug_parser ()
        then (
          Format.printf "return %a(" Var.print f;
          for i = 0 to n - 1 do
            if i > 0 then Format.printf ", ";
            Format.printf "%a" Var.print (fst (List.nth l i))
          done;
          Format.printf ")@.");
        let x, state = State.fresh_var state loc in
        let loc = snd (List.nth l (n - 1)) ||| loc_f in
        ( (Let (x, Apply { f; args = List.map ~f:fst l; exact = false }), loc) :: instrs
        , (Return x, loc)
        , state )
    | APPTERM1 ->
        let f, loc_f = State.accu state in
        let x, loc_x = State.peek 0 state in
        let loc = loc_x ||| loc_f in
        if debug_parser () then Format.printf "return %a(%a)@." Var.print f Var.print x;
        let y, state = State.fresh_var state loc in
        ( (Let (y, Apply { f; args = [ x ]; exact = false }), loc) :: instrs
        , (Return y, loc)
        , state )
    | APPTERM2 ->
        let f, loc_f = State.accu state in
        let x, loc_x = State.peek 0 state in
        let y, loc_y = State.peek 1 state in
        let loc = loc_y ||| loc_x ||| loc_f in
        if debug_parser ()
        then Format.printf "return %a(%a, %a)@." Var.print f Var.print x Var.print y;
        let z, state = State.fresh_var state loc in
        ( (Let (z, Apply { f; args = [ x; y ]; exact = false }), loc) :: instrs
        , (Return z, loc)
        , state )
    | APPTERM3 ->
        let f, loc_f = State.accu state in
        let x, loc_x = State.peek 0 state in
        let y, loc_y = State.peek 1 state in
        let z, loc_z = State.peek 2 state in
        let loc = loc_z ||| loc_y ||| loc_x ||| loc_f in
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
        let t, state = State.fresh_var state loc in
        ( (Let (t, Apply { f; args = [ x; y; z ]; exact = false }), loc) :: instrs
        , (Return t, loc)
        , state )
    | RETURN ->
        let x, loc_x = State.accu state in

        if debug_parser () then Format.printf "return %a@." Var.print x;
        instrs, (Return x, loc ||| loc_x), state
    | RESTART -> assert false
    | GRAB -> assert false
    | CLOSURE ->
        let nvars = getu code (pc + 1) in
        let addr = pc + gets code (pc + 2) + 2 in
        let state = if nvars > 0 then State.push state loc else state in

        let vals, state = State.grab nvars state in
        let x, state = State.fresh_var state loc in
        let env = List.map vals ~f:(fun (x, loc) -> State.Var (x, loc)) in
        let env =
          let code = State.Dummy in
          let closure_info = State.Dummy in
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
        let params, state' = State.make_stack nparams state' loc in
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
          ((Let (x, Closure (List.rev params, (addr, args))), loc) :: instrs)
    | CLOSUREREC ->
        let nfuncs = getu code (pc + 1) in
        let nvars = getu code (pc + 2) in
        let state = if nvars > 0 then State.push state loc else state in
        let vals, state = State.grab nvars state in

        let state = ref state in
        let vars = ref [] in
        let rec_names = ref (Debug.find_rec infos.debug (pc + 3 + gets code (pc + 3))) in
        for i = 0 to nfuncs - 1 do
          let x, st = State.fresh_var !state loc in
          (match !rec_names with
          | (j, ident) :: rest ->
              assert (j = i);
              Var.name x (Ident.name ident);
              rec_names := rest
          | [] -> ());
          vars := (i, x) :: !vars;
          state := State.push st loc
        done;
        let env = ref (List.map vals ~f:(fun (x, loc) -> State.Var (x, loc))) in
        List.iter !vars ~f:(fun (i, x) ->
            let code = State.Var (x, noloc) in
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
              let nparams, addr =
                match (get_instr_exn code addr).Instr.code with
                | GRAB -> getu code (addr + 1) + 1, addr + 2
                | _ -> 1, addr
              in
              let offset = i * clo_offset_3 in
              let state' = State.start_function state env offset in
              let params, state' = State.make_stack nparams state' loc in
              if debug_parser () then Format.printf ") {@.";
              let state' = State.clear_accu state' in
              compile_block infos.blocks infos.debug code addr state';
              if debug_parser () then Format.printf "}@.";
              let args = State.stack_vars state' in
              let state'', _, _ = Addr.Map.find addr !compiled_blocks in
              Debug.propagate (State.stack_vars state'') args;
              (Let (x, Closure (List.rev params, (addr, args))), loc) :: instr)
        in
        compile infos (pc + 3 + nfuncs) (State.acc (nfuncs - 1) state loc) instrs
    | OFFSETCLOSUREM3 ->
        compile infos (pc + 1) (State.env_acc (-clo_offset_3) state) instrs
    | OFFSETCLOSURE0 -> compile infos (pc + 1) (State.env_acc 0 state) instrs
    | OFFSETCLOSURE3 -> compile infos (pc + 1) (State.env_acc clo_offset_3 state) instrs
    | OFFSETCLOSURE ->
        let n = gets code (pc + 1) in
        compile infos (pc + 2) (State.env_acc n state) instrs
    | PUSHOFFSETCLOSUREM3 ->
        let state = State.push state loc in
        compile infos (pc + 1) (State.env_acc (-clo_offset_3) state) instrs
    | PUSHOFFSETCLOSURE0 ->
        let state = State.push state loc in
        compile infos (pc + 1) (State.env_acc 0 state) instrs
    | PUSHOFFSETCLOSURE3 ->
        let state = State.push state loc in
        compile infos (pc + 1) (State.env_acc clo_offset_3 state) instrs
    | PUSHOFFSETCLOSURE ->
        let state = State.push state loc in
        let n = gets code (pc + 1) in
        compile infos (pc + 2) (State.env_acc n state) instrs
    | GETGLOBAL ->
        let i = getu code (pc + 1) in
        let _, state, instrs = get_global state instrs i loc in
        compile infos (pc + 2) state instrs
    | PUSHGETGLOBAL ->
        let state = State.push state loc in
        let i = getu code (pc + 1) in
        let _, state, instrs = get_global state instrs i loc in
        compile infos (pc + 2) state instrs
    | GETGLOBALFIELD ->
        let i = getu code (pc + 1) in
        let x, state, instrs = get_global state instrs i loc in
        let j = getu code (pc + 2) in
        let y, state = State.fresh_var state loc in
        if debug_parser () then Format.printf "%a = %a[%d]@." Var.print y Var.print x j;
        compile infos (pc + 3) state ((Let (y, Field (x, j)), loc) :: instrs)
    | PUSHGETGLOBALFIELD ->
        let state = State.push state loc in

        let i = getu code (pc + 1) in
        let x, state, instrs = get_global state instrs i loc in
        let j = getu code (pc + 2) in
        let y, state = State.fresh_var state loc in
        if debug_parser () then Format.printf "%a = %a[%d]@." Var.print y Var.print x j;
        compile infos (pc + 3) state ((Let (y, Field (x, j)), loc) :: instrs)
    | SETGLOBAL ->
        let i = getu code (pc + 1) in
        State.size_globals state (i + 1);
        let y, _ = State.accu state in
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
        let x, state = State.fresh_var state loc in
        if debug_parser () then Format.printf "%a = 0@." Var.print x;
        let instrs = register_global g i loc instrs in
        compile infos (pc + 2) state ((Let (x, const 0l), loc) :: instrs)
    | ATOM0 ->
        let x, state = State.fresh_var state loc in

        if debug_parser () then Format.printf "%a = ATOM(0)@." Var.print x;
        compile infos (pc + 1) state ((Let (x, Block (0, [||], Unknown)), loc) :: instrs)
    | ATOM ->
        let i = getu code (pc + 1) in
        let x, state = State.fresh_var state loc in

        if debug_parser () then Format.printf "%a = ATOM(%d)@." Var.print x i;
        compile infos (pc + 2) state ((Let (x, Block (i, [||], NotArray)), loc) :: instrs)
    | PUSHATOM0 ->
        let state = State.push state loc in
        let x, state = State.fresh_var state loc in

        if debug_parser () then Format.printf "%a = ATOM(0)@." Var.print x;
        compile infos (pc + 1) state ((Let (x, Block (0, [||], Unknown)), loc) :: instrs)
    | PUSHATOM ->
        let state = State.push state loc in

        let i = getu code (pc + 1) in
        let x, state = State.fresh_var state loc in
        if debug_parser () then Format.printf "%a = ATOM(%d)@." Var.print x i;
        compile infos (pc + 2) state ((Let (x, Block (i, [||], NotArray)), loc) :: instrs)
    | MAKEBLOCK ->
        let size = getu code (pc + 1) in
        let tag = getu code (pc + 2) in
        let state = State.push state loc in

        let x, state = State.fresh_var state loc in
        let contents, state = State.grab size state in
        if debug_parser ()
        then (
          Format.printf "%a = { " Var.print x;
          for i = 0 to size - 1 do
            Format.printf "%d = %a; " i Var.print (fst (List.nth contents i))
          done;
          Format.printf "}@.");
        compile
          infos
          (pc + 3)
          state
          ((Let (x, Block (tag, Array.of_list (List.map ~f:fst contents), Unknown)), loc)
          :: instrs)
    | MAKEBLOCK1 ->
        let tag = getu code (pc + 1) in
        let y, _ = State.accu state in
        let x, state = State.fresh_var state loc in

        if debug_parser () then Format.printf "%a = { 0 = %a; }@." Var.print x Var.print y;
        compile
          infos
          (pc + 2)
          state
          ((Let (x, Block (tag, [| y |], NotArray)), loc) :: instrs)
    | MAKEBLOCK2 ->
        let tag = getu code (pc + 1) in
        let y, _ = State.accu state in
        let z, _ = State.peek 0 state in
        let x, state = State.fresh_var state loc in

        if debug_parser ()
        then
          Format.printf "%a = { 0 = %a; 1 = %a; }@." Var.print x Var.print y Var.print z;
        compile
          infos
          (pc + 2)
          (State.pop 1 state)
          ((Let (x, Block (tag, [| y; z |], NotArray)), loc) :: instrs)
    | MAKEBLOCK3 ->
        let tag = getu code (pc + 1) in
        let y, _ = State.accu state in
        let z, _ = State.peek 0 state in
        let t, _ = State.peek 1 state in
        let x, state = State.fresh_var state loc in

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
          ((Let (x, Block (tag, [| y; z; t |], NotArray)), loc) :: instrs)
    | MAKEFLOATBLOCK ->
        let size = getu code (pc + 1) in
        let state = State.push state loc in
        let x, state = State.fresh_var state loc in
        let contents, state = State.grab size state in

        if debug_parser ()
        then (
          Format.printf "%a = { " Var.print x;
          for i = 0 to size - 1 do
            Format.printf "%d = %a; " i Var.print (fst (List.nth contents i))
          done;
          Format.printf "}@.");
        compile
          infos
          (pc + 2)
          state
          ((Let (x, Block (254, Array.of_list (List.map ~f:fst contents), Unknown)), loc)
          :: instrs)
    | GETFIELD0 ->
        let y, _ = State.accu state in
        let x, state = State.fresh_var state loc in

        if debug_parser () then Format.printf "%a = %a[0]@." Var.print x Var.print y;
        compile infos (pc + 1) state ((Let (x, Field (y, 0)), loc) :: instrs)
    | GETFIELD1 ->
        let y, _ = State.accu state in
        let x, state = State.fresh_var state loc in

        if debug_parser () then Format.printf "%a = %a[1]@." Var.print x Var.print y;
        compile infos (pc + 1) state ((Let (x, Field (y, 1)), loc) :: instrs)
    | GETFIELD2 ->
        let y, _ = State.accu state in
        let x, state = State.fresh_var state loc in

        if debug_parser () then Format.printf "%a = %a[2]@." Var.print x Var.print y;
        compile infos (pc + 1) state ((Let (x, Field (y, 2)), loc) :: instrs)
    | GETFIELD3 ->
        let y, _ = State.accu state in
        let x, state = State.fresh_var state loc in

        if debug_parser () then Format.printf "%a = %a[3]@." Var.print x Var.print y;
        compile infos (pc + 1) state ((Let (x, Field (y, 3)), loc) :: instrs)
    | GETFIELD ->
        let y, _ = State.accu state in
        let n = getu code (pc + 1) in
        let x, state = State.fresh_var state loc in

        if debug_parser () then Format.printf "%a = %a[%d]@." Var.print x Var.print y n;
        compile infos (pc + 2) state ((Let (x, Field (y, n)), loc) :: instrs)
    | GETFLOATFIELD ->
        let y, _ = State.accu state in
        let n = getu code (pc + 1) in
        let x, state = State.fresh_var state loc in

        if debug_parser () then Format.printf "%a = %a[%d]@." Var.print x Var.print y n;
        compile infos (pc + 2) state ((Let (x, Field (y, n)), loc) :: instrs)
    | SETFIELD0 ->
        let y, _ = State.accu state in
        let z, _ = State.peek 0 state in

        if debug_parser () then Format.printf "%a[0] = %a@." Var.print y Var.print z;
        let x, state = State.fresh_var state loc in
        if debug_parser () then Format.printf "%a = 0@." Var.print x;
        compile
          infos
          (pc + 1)
          (State.pop 1 state)
          ((Let (x, const 0l), loc) :: (Set_field (y, 0, z), loc) :: instrs)
    | SETFIELD1 ->
        let y, _ = State.accu state in
        let z, _ = State.peek 0 state in

        if debug_parser () then Format.printf "%a[1] = %a@." Var.print y Var.print z;
        let x, state = State.fresh_var state loc in
        if debug_parser () then Format.printf "%a = 0@." Var.print x;
        compile
          infos
          (pc + 1)
          (State.pop 1 state)
          ((Let (x, const 0l), loc) :: (Set_field (y, 1, z), loc) :: instrs)
    | SETFIELD2 ->
        let y, _ = State.accu state in
        let z, _ = State.peek 0 state in

        if debug_parser () then Format.printf "%a[2] = %a@." Var.print y Var.print z;
        let x, state = State.fresh_var state loc in
        if debug_parser () then Format.printf "%a = 0@." Var.print x;
        compile
          infos
          (pc + 1)
          (State.pop 1 state)
          ((Let (x, const 0l), loc) :: (Set_field (y, 2, z), loc) :: instrs)
    | SETFIELD3 ->
        let y, _ = State.accu state in
        let z, _ = State.peek 0 state in

        if debug_parser () then Format.printf "%a[3] = %a@." Var.print y Var.print z;
        let x, state = State.fresh_var state loc in
        if debug_parser () then Format.printf "%a = 0@." Var.print x;
        compile
          infos
          (pc + 1)
          (State.pop 1 state)
          ((Let (x, const 0l), loc) :: (Set_field (y, 3, z), loc) :: instrs)
    | SETFIELD ->
        let y, _ = State.accu state in
        let z, _ = State.peek 0 state in
        let n = getu code (pc + 1) in

        if debug_parser () then Format.printf "%a[%d] = %a@." Var.print y n Var.print z;
        let x, state = State.fresh_var state loc in
        if debug_parser () then Format.printf "%a = 0@." Var.print x;
        compile
          infos
          (pc + 2)
          (State.pop 1 state)
          ((Let (x, const 0l), loc) :: (Set_field (y, n, z), loc) :: instrs)
    | SETFLOATFIELD ->
        let y, _ = State.accu state in
        let z, _ = State.peek 0 state in
        let n = getu code (pc + 1) in

        if debug_parser () then Format.printf "%a[%d] = %a@." Var.print y n Var.print z;
        let x, state = State.fresh_var state loc in
        if debug_parser () then Format.printf "%a = 0@." Var.print x;
        compile
          infos
          (pc + 2)
          (State.pop 1 state)
          ((Let (x, const 0l), loc) :: (Set_field (y, n, z), loc) :: instrs)
    | VECTLENGTH ->
        let y, _ = State.accu state in
        let x, state = State.fresh_var state loc in

        if debug_parser () then Format.printf "%a = %a.length@." Var.print x Var.print y;
        compile
          infos
          (pc + 1)
          state
          ((Let (x, Prim (Vectlength, [ Pv y ])), loc) :: instrs)
    | GETVECTITEM ->
        let y, _ = State.accu state in
        let z, _ = State.peek 0 state in
        let x, state = State.fresh_var state loc in

        if debug_parser ()
        then Format.printf "%a = %a[%a]@." Var.print x Var.print y Var.print z;
        compile
          infos
          (pc + 1)
          (State.pop 1 state)
          ((Let (x, Prim (Array_get, [ Pv y; Pv z ])), loc) :: instrs)
    | SETVECTITEM ->
        let x, _ = State.accu state in
        let y, _ = State.peek 0 state in
        let z, _ = State.peek 1 state in
        if debug_parser ()
        then Format.printf "%a[%a] = %a@." Var.print x Var.print y Var.print z;
        let instrs = (Array_set (x, y, z), loc) :: instrs in
        let x, state = State.fresh_var state loc in
        if debug_parser () then Format.printf "%a = 0@." Var.print x;
        compile infos (pc + 1) (State.pop 2 state) ((Let (x, const 0l), loc) :: instrs)
    | GETSTRINGCHAR ->
        let y, _ = State.accu state in
        let z, _ = State.peek 0 state in
        let x, state = State.fresh_var state loc in

        if debug_parser ()
        then Format.printf "%a = %a[%a]@." Var.print x Var.print y Var.print z;
        compile
          infos
          (pc + 1)
          (State.pop 1 state)
          ((Let (x, Prim (Extern "caml_string_unsafe_get", [ Pv y; Pv z ])), loc)
          :: instrs)
    | GETBYTESCHAR ->
        let y, _ = State.accu state in
        let z, _ = State.peek 0 state in
        let x, state = State.fresh_var state loc in

        if debug_parser ()
        then Format.printf "%a = %a[%a]@." Var.print x Var.print y Var.print z;
        compile
          infos
          (pc + 1)
          (State.pop 1 state)
          ((Let (x, Prim (Extern "caml_bytes_unsafe_get", [ Pv y; Pv z ])), loc) :: instrs)
    | SETBYTESCHAR ->
        let x, _ = State.accu state in
        let y, _ = State.peek 0 state in
        let z, _ = State.peek 1 state in
        if debug_parser ()
        then Format.printf "%a[%a] = %a@." Var.print x Var.print y Var.print z;
        let t, state = State.fresh_var state loc in
        let instrs =
          (Let (t, Prim (Extern "caml_bytes_unsafe_set", [ Pv x; Pv y; Pv z ])), loc)
          :: instrs
        in
        let x, state = State.fresh_var state loc in
        if debug_parser () then Format.printf "%a = 0@." Var.print x;
        compile infos (pc + 1) (State.pop 2 state) ((Let (x, const 0l), loc) :: instrs)
    | BRANCH ->
        let offset = gets code (pc + 1) in
        if debug_parser () then Format.printf "... (branch)@.";
        instrs, (Branch (pc + offset + 1, State.stack_vars state), loc), state
    | BRANCHIF ->
        let offset = gets code (pc + 1) in
        let x, loc_x = State.accu state in
        let loc = loc ||| loc_x in
        let args = State.stack_vars state in
        instrs, (Cond (x, (pc + offset + 1, args), (pc + 2, args)), loc), state
    | BRANCHIFNOT ->
        let offset = gets code (pc + 1) in
        let x, _ = State.accu state in
        let args = State.stack_vars state in
        instrs, (Cond (x, (pc + 2, args), (pc + offset + 1, args)), loc), state
    | SWITCH ->
        if debug_parser () then Format.printf "switch ...@.";

        let sz = getu code (pc + 1) in
        let x, _ = State.accu state in
        let args = State.stack_vars state in
        let l = sz land 0xFFFF in
        let it =
          Array.init (sz land 0XFFFF) ~f:(fun i -> pc + 2 + gets code (pc + 2 + i), args)
        in
        let bt =
          Array.init (sz lsr 16) ~f:(fun i -> pc + 2 + gets code (pc + 2 + l + i), args)
        in
        instrs, (Switch (x, it, bt), loc), state
    | BOOLNOT ->
        let y, _ = State.accu state in
        let x, state = State.fresh_var state loc in
        if debug_parser () then Format.printf "%a = !%a@." Var.print x Var.print y;
        compile infos (pc + 1) state ((Let (x, Prim (Not, [ Pv y ])), loc) :: instrs)
    | PUSHTRAP ->
        (* We insert an intermediate block that binds the handler's
           context, so that it is also in the scope of the body. Then,
           when a mutable variable is assigned in the body, we can
           update this context. *)
        let interm_addr = pc + 1 in
        let handler_ctx_state = State.start_block interm_addr state in
        let body_addr = pc + 2 in
        let handler_addr = pc + 1 + gets code (pc + 1) in
        let x, handler_state = State.fresh_var handler_ctx_state loc in

        tagged_blocks := Addr.Set.add interm_addr !tagged_blocks;
        compiled_blocks :=
          Addr.Map.add
            interm_addr
            ( handler_ctx_state
            , []
            , ( Pushtrap
                  ( (body_addr, State.stack_vars state)
                  , x
                  , (handler_addr, State.stack_vars handler_state)
                  , Addr.Set.empty )
              , loc ) )
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
              State.Dummy
              :: State.Dummy
              :: State.Dummy
              :: State.Dummy
              :: state.State.stack
          };
        instrs, (Branch (interm_addr, State.stack_vars state), loc), state
    | POPTRAP ->
        let addr = pc + 1 in
        let handler_addr = State.addr_of_current_handler state in
        let set =
          try Addr.Set.add addr (Addr.Map.find handler_addr !pushpop)
          with Not_found -> Addr.Set.singleton addr
        in
        pushpop := Addr.Map.add handler_addr set !pushpop;
        compile_block
          infos.blocks
          infos.debug
          code
          addr
          (State.pop 4 (State.pop_handler state));
        instrs, (Poptrap (addr, State.stack_vars state), loc), state
    | RERAISE | RAISE_NOTRACE | RAISE ->
        let x, _ = State.accu state in
        let kind =
          match instr.Instr.code with
          | RERAISE -> `Reraise
          | RAISE_NOTRACE -> `Notrace
          | RAISE -> `Normal
          | _ -> assert false
        in
        if debug_parser () then Format.printf "throw(%a)@." Var.print x;
        instrs, (Raise (x, kind), loc), state
    | CHECK_SIGNALS -> compile infos (pc + 1) state instrs
    | C_CALL1 ->
        let prim = primitive_name state (getu code (pc + 1)) in

        if String.equal (Primitive.resolve prim) "%identity"
        then (* This is a no-op *)
          compile infos (pc + 2) state instrs
        else
          let y, _ = State.accu state in
          let x, state = State.fresh_var state loc in
          if debug_parser ()
          then Format.printf "%a = ccall \"%s\" (%a)@." Var.print x prim Var.print y;
          compile
            infos
            (pc + 2)
            state
            ((Let (x, Prim (Extern prim, [ Pv y ])), loc) :: instrs)
    | C_CALL2 ->
        let prim = primitive_name state (getu code (pc + 1)) in
        let y, _ = State.accu state in
        let z, _ = State.peek 0 state in
        let x, state = State.fresh_var state loc in

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
          ((Let (x, Prim (Extern prim, [ Pv y; Pv z ])), loc) :: instrs)
    | C_CALL3 ->
        let prim = primitive_name state (getu code (pc + 1)) in
        let y, _ = State.accu state in
        let z, _ = State.peek 0 state in
        let t, _ = State.peek 1 state in
        let x, state = State.fresh_var state loc in

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
          ((Let (x, Prim (Extern prim, [ Pv y; Pv z; Pv t ])), loc) :: instrs)
    | C_CALL4 ->
        let nargs = 4 in
        let prim = primitive_name state (getu code (pc + 1)) in
        let state = State.push state loc in
        let x, state = State.fresh_var state loc in
        let args, state = State.grab nargs state in

        if debug_parser ()
        then (
          Format.printf "%a = ccal \"%s\" (" Var.print x prim;
          for i = 0 to nargs - 1 do
            if i > 0 then Format.printf ", ";
            Format.printf "%a" Var.print (fst (List.nth args i))
          done;
          Format.printf ")@.");
        compile
          infos
          (pc + 2)
          state
          ((Let (x, Prim (Extern prim, List.map args ~f:(fun (x, _) -> Pv x))), loc)
          :: instrs)
    | C_CALL5 ->
        let nargs = 5 in
        let prim = primitive_name state (getu code (pc + 1)) in
        let state = State.push state loc in
        let x, state = State.fresh_var state loc in
        let args, state = State.grab nargs state in

        if debug_parser ()
        then (
          Format.printf "%a = ccal \"%s\" (" Var.print x prim;
          for i = 0 to nargs - 1 do
            if i > 0 then Format.printf ", ";
            Format.printf "%a" Var.print (fst (List.nth args i))
          done;
          Format.printf ")@.");
        compile
          infos
          (pc + 2)
          state
          ((Let (x, Prim (Extern prim, List.map args ~f:(fun (x, _) -> Pv x))), loc)
          :: instrs)
    | C_CALLN ->
        let nargs = getu code (pc + 1) in
        let prim = primitive_name state (getu code (pc + 2)) in
        let state = State.push state loc in
        let x, state = State.fresh_var state loc in
        let args, state = State.grab nargs state in

        if debug_parser ()
        then (
          Format.printf "%a = ccal \"%s\" (" Var.print x prim;
          for i = 0 to nargs - 1 do
            if i > 0 then Format.printf ", ";
            Format.printf "%a" Var.print (fst (List.nth args i))
          done;
          Format.printf ")@.");
        compile
          infos
          (pc + 3)
          state
          ((Let (x, Prim (Extern prim, List.map args ~f:(fun (x, _) -> Pv x))), loc)
          :: instrs)
    | (CONST0 | CONST1 | CONST2 | CONST3) as cc ->
        let x, state = State.fresh_var state loc in
        let n =
          match cc with
          | CONST0 -> 0l
          | CONST1 -> 1l
          | CONST2 -> 2l
          | CONST3 -> 3l
          | _ -> assert false
        in

        if debug_parser () then Format.printf "%a = %ld@." Var.print x n;
        compile infos (pc + 1) state ((Let (x, const n), loc) :: instrs)
    | CONSTINT ->
        let n = gets32 code (pc + 1) in
        let x, state = State.fresh_var state loc in

        if debug_parser () then Format.printf "%a = %ld@." Var.print x n;
        compile infos (pc + 2) state ((Let (x, const n), loc) :: instrs)
    | (PUSHCONST0 | PUSHCONST1 | PUSHCONST2 | PUSHCONST3) as cc ->
        let state = State.push state loc in
        let x, state = State.fresh_var state loc in
        let n =
          match cc with
          | PUSHCONST0 -> 0l
          | PUSHCONST1 -> 1l
          | PUSHCONST2 -> 2l
          | PUSHCONST3 -> 3l
          | _ -> assert false
        in

        if debug_parser () then Format.printf "%a = %ld@." Var.print x n;
        compile infos (pc + 1) state ((Let (x, const n), loc) :: instrs)
    | PUSHCONSTINT ->
        let state = State.push state loc in
        let n = gets32 code (pc + 1) in
        let x, state = State.fresh_var state loc in

        if debug_parser () then Format.printf "%a = %ld@." Var.print x n;
        compile infos (pc + 2) state ((Let (x, const n), loc) :: instrs)
    | NEGINT ->
        let y, _ = State.accu state in
        let x, state = State.fresh_var state loc in

        if debug_parser () then Format.printf "%a = -%a@." Var.print x Var.print y;
        compile
          infos
          (pc + 1)
          state
          ((Let (x, Prim (Extern "%int_neg", [ Pv y ])), loc) :: instrs)
    | ADDINT ->
        let y, _ = State.accu state in
        let z, _ = State.peek 0 state in
        let x, state = State.fresh_var state loc in

        if debug_parser ()
        then Format.printf "%a = %a + %a@." Var.print x Var.print y Var.print z;
        compile
          infos
          (pc + 1)
          (State.pop 1 state)
          ((Let (x, Prim (Extern "%int_add", [ Pv y; Pv z ])), loc) :: instrs)
    | SUBINT ->
        let y, _ = State.accu state in
        let z, _ = State.peek 0 state in
        let x, state = State.fresh_var state loc in

        if debug_parser ()
        then Format.printf "%a = %a - %a@." Var.print x Var.print y Var.print z;
        compile
          infos
          (pc + 1)
          (State.pop 1 state)
          ((Let (x, Prim (Extern "%int_sub", [ Pv y; Pv z ])), loc) :: instrs)
    | MULINT ->
        let y, _ = State.accu state in
        let z, _ = State.peek 0 state in
        let x, state = State.fresh_var state loc in

        if debug_parser ()
        then Format.printf "%a = %a * %a@." Var.print x Var.print y Var.print z;
        compile
          infos
          (pc + 1)
          (State.pop 1 state)
          ((Let (x, Prim (Extern "%int_mul", [ Pv y; Pv z ])), loc) :: instrs)
    | DIVINT ->
        let y, _ = State.accu state in
        let z, _ = State.peek 0 state in
        let x, state = State.fresh_var state loc in

        if debug_parser ()
        then Format.printf "%a = %a / %a@." Var.print x Var.print y Var.print z;
        compile
          infos
          (pc + 1)
          (State.pop 1 state)
          ((Let (x, Prim (Extern "%int_div", [ Pv y; Pv z ])), loc) :: instrs)
    | MODINT ->
        let y, _ = State.accu state in
        let z, _ = State.peek 0 state in
        let x, state = State.fresh_var state loc in

        if debug_parser ()
        then Format.printf "%a = %a %% %a@." Var.print x Var.print y Var.print z;
        compile
          infos
          (pc + 1)
          (State.pop 1 state)
          ((Let (x, Prim (Extern "%int_mod", [ Pv y; Pv z ])), loc) :: instrs)
    | ANDINT ->
        let y, _ = State.accu state in
        let z, _ = State.peek 0 state in
        let x, state = State.fresh_var state loc in

        if debug_parser ()
        then Format.printf "%a = %a & %a@." Var.print x Var.print y Var.print z;
        compile
          infos
          (pc + 1)
          (State.pop 1 state)
          ((Let (x, Prim (Extern "%int_and", [ Pv y; Pv z ])), loc) :: instrs)
    | ORINT ->
        let y, _ = State.accu state in
        let z, _ = State.peek 0 state in
        let x, state = State.fresh_var state loc in

        if debug_parser ()
        then Format.printf "%a = %a | %a@." Var.print x Var.print y Var.print z;
        compile
          infos
          (pc + 1)
          (State.pop 1 state)
          ((Let (x, Prim (Extern "%int_or", [ Pv y; Pv z ])), loc) :: instrs)
    | XORINT ->
        let y, _ = State.accu state in
        let z, _ = State.peek 0 state in
        let x, state = State.fresh_var state loc in

        if debug_parser ()
        then Format.printf "%a = %a ^ %a@." Var.print x Var.print y Var.print z;
        compile
          infos
          (pc + 1)
          (State.pop 1 state)
          ((Let (x, Prim (Extern "%int_xor", [ Pv y; Pv z ])), loc) :: instrs)
    | LSLINT ->
        let y, _ = State.accu state in
        let z, _ = State.peek 0 state in
        let x, state = State.fresh_var state loc in

        if debug_parser ()
        then Format.printf "%a = %a << %a@." Var.print x Var.print y Var.print z;
        compile
          infos
          (pc + 1)
          (State.pop 1 state)
          ((Let (x, Prim (Extern "%int_lsl", [ Pv y; Pv z ])), loc) :: instrs)
    | LSRINT ->
        let y, _ = State.accu state in
        let z, _ = State.peek 0 state in
        let x, state = State.fresh_var state loc in

        if debug_parser ()
        then Format.printf "%a = %a >>> %a@." Var.print x Var.print y Var.print z;
        compile
          infos
          (pc + 1)
          (State.pop 1 state)
          ((Let (x, Prim (Extern "%int_lsr", [ Pv y; Pv z ])), loc) :: instrs)
    | ASRINT ->
        let y, _ = State.accu state in
        let z, _ = State.peek 0 state in
        let x, state = State.fresh_var state loc in

        if debug_parser ()
        then Format.printf "%a = %a >> %a@." Var.print x Var.print y Var.print z;
        compile
          infos
          (pc + 1)
          (State.pop 1 state)
          ((Let (x, Prim (Extern "%int_asr", [ Pv y; Pv z ])), loc) :: instrs)
    | EQ ->
        let y, _ = State.accu state in
        let z, _ = State.peek 0 state in
        let x, state = State.fresh_var state loc in

        if debug_parser ()
        then Format.printf "%a = mk_bool(%a == %a)@." Var.print x Var.print y Var.print z;
        compile
          infos
          (pc + 1)
          (State.pop 1 state)
          ((Let (x, Prim (Eq, [ Pv y; Pv z ])), loc) :: instrs)
    | NEQ ->
        let y, _ = State.accu state in
        let z, _ = State.peek 0 state in
        let x, state = State.fresh_var state loc in

        if debug_parser ()
        then Format.printf "%a = mk_bool(%a != %a)@." Var.print x Var.print y Var.print z;
        compile
          infos
          (pc + 1)
          (State.pop 1 state)
          ((Let (x, Prim (Neq, [ Pv y; Pv z ])), loc) :: instrs)
    | LTINT ->
        let y, _ = State.accu state in
        let z, _ = State.peek 0 state in
        let x, state = State.fresh_var state loc in

        if debug_parser ()
        then Format.printf "%a = mk_bool(%a < %a)@." Var.print x Var.print y Var.print z;
        compile
          infos
          (pc + 1)
          (State.pop 1 state)
          ((Let (x, Prim (Lt, [ Pv y; Pv z ])), loc) :: instrs)
    | LEINT ->
        let y, _ = State.accu state in
        let z, _ = State.peek 0 state in
        let x, state = State.fresh_var state loc in

        if debug_parser ()
        then Format.printf "%a = mk_bool(%a <= %a)@." Var.print x Var.print y Var.print z;
        compile
          infos
          (pc + 1)
          (State.pop 1 state)
          ((Let (x, Prim (Le, [ Pv y; Pv z ])), loc) :: instrs)
    | GTINT ->
        let y, _ = State.accu state in
        let z, _ = State.peek 0 state in
        let x, state = State.fresh_var state loc in

        if debug_parser ()
        then Format.printf "%a = mk_bool(%a > %a)@." Var.print x Var.print y Var.print z;
        compile
          infos
          (pc + 1)
          (State.pop 1 state)
          ((Let (x, Prim (Lt, [ Pv z; Pv y ])), loc) :: instrs)
    | GEINT ->
        let y, _ = State.accu state in
        let z, _ = State.peek 0 state in
        let x, state = State.fresh_var state loc in

        if debug_parser ()
        then Format.printf "%a = mk_bool(%a >= %a)@." Var.print x Var.print y Var.print z;
        compile
          infos
          (pc + 1)
          (State.pop 1 state)
          ((Let (x, Prim (Le, [ Pv z; Pv y ])), loc) :: instrs)
    | OFFSETINT ->
        let n = gets32 code (pc + 1) in
        let y, loc_y = State.accu state in
        let loc = loc_y ||| loc in
        let z, state = State.fresh_var state loc in
        let x, state = State.fresh_var state loc in
        if debug_parser () then Format.printf "%a = %a + %ld@." Var.print x Var.print y n;
        compile
          infos
          (pc + 2)
          state
          ((Let (x, Prim (Extern "%int_add", [ Pv y; Pv z ])), loc)
          :: (Let (z, const n), loc)
          :: instrs)
    | OFFSETREF ->
        let n = gets code (pc + 1) in
        let x, _ = State.accu state in

        if debug_parser () then Format.printf "%a += %d@." Var.print x n;
        let instrs = (Offset_ref (x, n), loc) :: instrs in
        let x, state = State.fresh_var state loc in
        if debug_parser () then Format.printf "x = 0@.";
        compile infos (pc + 2) state ((Let (x, const 0l), loc) :: instrs)
    | ISINT ->
        let y, _ = State.accu state in
        let x, state = State.fresh_var state loc in

        if debug_parser () then Format.printf "%a = !%a@." Var.print x Var.print y;
        compile infos (pc + 1) state ((Let (x, Prim (IsInt, [ Pv y ])), loc) :: instrs)
    | BEQ ->
        let n = gets32 code (pc + 1) in
        let offset = gets code (pc + 2) in
        let x, _ = State.accu state in
        let args = State.stack_vars state in
        let y = Var.fresh () in

        ( (Let (y, Prim (Eq, [ Pc (Int (Regular, n)); Pv x ])), loc) :: instrs
        , (Cond (y, (pc + offset + 2, args), (pc + 3, args)), loc)
        , state )
    | BNEQ ->
        let n = gets32 code (pc + 1) in
        let offset = gets code (pc + 2) in
        let x, _ = State.accu state in
        let args = State.stack_vars state in
        let y = Var.fresh () in

        ( (Let (y, Prim (Eq, [ Pc (Int (Regular, n)); Pv x ])), loc) :: instrs
        , (Cond (y, (pc + 3, args), (pc + offset + 2, args)), loc)
        , state )
    | BLTINT ->
        let n = gets32 code (pc + 1) in
        let offset = gets code (pc + 2) in
        let x, _ = State.accu state in
        let args = State.stack_vars state in
        let y = Var.fresh () in

        ( (Let (y, Prim (Lt, [ Pc (Int (Regular, n)); Pv x ])), loc) :: instrs
        , (Cond (y, (pc + offset + 2, args), (pc + 3, args)), loc)
        , state )
    | BLEINT ->
        let n = gets32 code (pc + 1) in
        let offset = gets code (pc + 2) in
        let x, _ = State.accu state in
        let args = State.stack_vars state in
        let y = Var.fresh () in

        ( (Let (y, Prim (Le, [ Pc (Int (Regular, n)); Pv x ])), loc) :: instrs
        , (Cond (y, (pc + offset + 2, args), (pc + 3, args)), loc)
        , state )
    | BGTINT ->
        let n = gets32 code (pc + 1) in
        let offset = gets code (pc + 2) in
        let x, _ = State.accu state in
        let args = State.stack_vars state in
        let y = Var.fresh () in

        ( (Let (y, Prim (Le, [ Pc (Int (Regular, n)); Pv x ])), loc) :: instrs
        , (Cond (y, (pc + 3, args), (pc + offset + 2, args)), loc)
        , state )
    | BGEINT ->
        let n = gets32 code (pc + 1) in
        let offset = gets code (pc + 2) in
        let x, _ = State.accu state in
        let args = State.stack_vars state in
        let y = Var.fresh () in

        ( (Let (y, Prim (Lt, [ Pc (Int (Regular, n)); Pv x ])), loc) :: instrs
        , (Cond (y, (pc + 3, args), (pc + offset + 2, args)), loc)
        , state )
    | BULTINT ->
        let n = getu32 code (pc + 1) in
        let offset = gets code (pc + 2) in
        let x, _ = State.accu state in
        let args = State.stack_vars state in
        let y = Var.fresh () in

        ( (Let (y, Prim (Ult, [ Pc (Int (Regular, n)); Pv x ])), loc) :: instrs
        , (Cond (y, (pc + offset + 2, args), (pc + 3, args)), loc)
        , state )
    | BUGEINT ->
        let n = getu32 code (pc + 1) in
        let offset = gets code (pc + 2) in
        let x, _ = State.accu state in
        let args = State.stack_vars state in
        let y = Var.fresh () in

        ( (Let (y, Prim (Ult, [ Pc (Int (Regular, n)); Pv x ])), loc) :: instrs
        , (Cond (y, (pc + 3, args), (pc + offset + 2, args)), loc)
        , state )
    | ULTINT ->
        let y, _ = State.accu state in
        let z, _ = State.peek 0 state in
        let x, state = State.fresh_var state loc in

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
          ((Let (x, Prim (Ult, [ Pv y; Pv z ])), loc) :: instrs)
    | UGEINT ->
        let y, _ = State.accu state in
        let z, _ = State.peek 0 state in
        let x, state = State.fresh_var state loc in

        if debug_parser ()
        then Format.printf "%a = mk_bool(%a >= %a)@." Var.print x Var.print y Var.print z;
        compile
          infos
          (pc + 1)
          (State.pop 1 state)
          ((Let (x, Prim (Ult, [ Pv z; Pv y ])), loc) :: instrs)
    | GETPUBMET ->
        let n = gets32 code (pc + 1) in
        let cache = !method_cache_id in
        incr method_cache_id;
        let obj, _ = State.accu state in
        let state = State.push state loc in
        let tag, state = State.fresh_var state loc in
        let m, state = State.fresh_var state loc in

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
          (( Let
               ( m
               , Prim
                   ( Extern "caml_get_public_method"
                   , [ Pv obj; Pv tag; Pc (Int (Regular, Int32.of_int cache)) ] ) )
           , loc )
          :: (Let (tag, const n), loc)
          :: instrs)
    | GETDYNMET ->
        let tag, _ = State.accu state in
        let obj, _ = State.peek 0 state in
        let m, state = State.fresh_var state loc in

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
          (( Let
               ( m
               , Prim
                   ( Extern "caml_get_public_method"
                   , [ Pv obj; Pv tag; Pc (Int (Regular, 0l)) ] ) )
           , loc )
          :: instrs)
    | GETMETHOD ->
        let lab, _ = State.accu state in
        let obj, _ = State.peek 0 state in
        let meths, state = State.fresh_var state loc in
        let m, state = State.fresh_var state loc in

        if debug_parser ()
        then Format.printf "%a = lookup(%a, %a)@." Var.print m Var.print obj Var.print lab;
        compile
          infos
          (pc + 1)
          state
          ((Let (m, Prim (Array_get, [ Pv meths; Pv lab ])), loc)
          :: (Let (meths, Field (obj, 0)), loc)
          :: instrs)
    | STOP -> instrs, (Stop, loc), state
    | RESUME ->
        let stack, _ = State.accu state in
        let func, _ = State.peek 0 state in
        let arg, _ = State.peek 1 state in
        let x, state = State.fresh_var state loc in

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
        compile
          infos
          (pc + 1)
          (State.pop 2 state)
          ((Let (x, Prim (Extern "%resume", [ Pv stack; Pv func; Pv arg ])), loc)
          :: instrs)
    | RESUMETERM ->
        let stack, _ = State.accu state in
        let func, func_loc = State.peek 0 state in
        let arg, arg_loc = State.peek 1 state in
        let loc = loc ||| func_loc ||| arg_loc in
        let x, state = State.fresh_var state loc in

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
        ( (Let (x, Prim (Extern "%resume", [ Pv stack; Pv func; Pv arg ])), loc) :: instrs
        , (Return x, loc)
        , state )
    | PERFORM ->
        let eff, _ = State.accu state in
        let x, state = State.fresh_var state loc in

        if debug_parser ()
        then Format.printf "%a = perform(%a)@." Var.print x Var.print eff;
        compile
          infos
          (pc + 1)
          state
          ((Let (x, Prim (Extern "%perform", [ Pv eff ])), loc) :: instrs)
    | REPERFORMTERM ->
        let eff, _ = State.accu state in
        let stack, _ = State.peek 0 state in
        let _, loc' = State.peek 1 state in
        let state = State.pop 2 state in
        let loc = loc ||| loc' in
        let x, state = State.fresh_var state loc in

        if debug_parser ()
        then Format.printf "return reperform(%a, %a)@." Var.print eff Var.print stack;
        ( (Let (x, Prim (Extern "%reperform", [ Pv eff; Pv stack ])), loc) :: instrs
        , (Return x, loc)
        , state )
    | EVENT | BREAK | FIRST_UNIMPLEMENTED_OP -> assert false)

(****)

let match_exn_traps (blocks : 'a Addr.Map.t) =
  Addr.Map.fold
    (fun pc conts' blocks ->
      match Addr.Map.find pc blocks with
      | { branch = Pushtrap (cont1, x, cont2, conts), loc; _ } as block ->
          assert (Addr.Set.is_empty conts);
          let branch = Pushtrap (cont1, x, cont2, conts'), loc in
          Addr.Map.add pc { block with branch } blocks
      | _ -> assert false)
    !pushpop
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
      let blocks = match_exn_traps blocks in
      let free_pc = String.length code / 4 in
      { start; blocks; free_pc })
    else Code.empty
  in
  pushpop := Addr.Map.empty;
  compiled_blocks := Addr.Map.empty;
  tagged_blocks := Addr.Set.empty;
  p

(* HACK - override module *)

let override_global =
  match Ocaml_version.v with
  | `V4_13 | `V4_14 | `V5_00 | `V5_01 | `V5_02 -> []
  | `V4_08 | `V4_09 | `V4_10 | `V4_11 | `V4_12 ->
      let jsmodule name func =
        Prim (Extern "%overrideMod", [ Pc (String name); Pc (String func) ])
      in
      [ ( "CamlinternalMod"
        , fun _orig instrs ->
            let x = Var.fresh_n "internalMod" in
            let init_mod = Var.fresh_n "init_mod" in
            let update_mod = Var.fresh_n "update_mod" in
            ( x
            , (Let (x, Block (0, [| init_mod; update_mod |], NotArray)), noloc)
              :: (Let (init_mod, jsmodule "CamlinternalMod" "init_mod"), noloc)
              :: (Let (update_mod, jsmodule "CamlinternalMod" "update_mod"), noloc)
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

let from_exe
    ~target
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
  let init_data = Array.map ~f:(Constants.parse ~target) init_data in
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
        let body = register_global ~force:true globals i noloc body in
        globals.is_exported.(i) <- false;
        body)
  in
  let body =
    Array.fold_right_i globals.constants ~init:body ~f:(fun i _ l ->
        match globals.vars.(i) with
        | Some x when globals.is_const.(i) ->
            let l = register_global globals i noloc l in
            (Let (x, Constant globals.constants.(i)), noloc) :: l
        | _ -> l)
  in
  let body =
    if link_info
    then
      let symtable_js =
        Ocaml_compiler.Symtable.GlobalMap.fold
          (fun i p acc -> (Ocaml_compiler.Symtable.Global.name i, p) :: acc)
          symbols
          []
        |> Array.of_list
      in
      (* Include linking information *)
      let toc =
        [ "SYMB", Obj.repr symbols
        ; "SYJS", Obj.repr symtable_js
        ; "CRCS", Obj.repr crcs
        ; "PRIM", Obj.repr (String.concat ~sep:"\000" primitives ^ "\000")
        ]
      in
      let gdata = Var.fresh () in
      let need_gdata = ref false in
      let infos =
        [ "toc", Constants.parse ~target (Obj.repr toc)
        ; "prim_count", Int (Regular, Int32.of_int (Array.length globals.primitives))
        ]
      in
      let body =
        List.fold_left infos ~init:body ~f:(fun rem (name, const) ->
            assert (String.is_valid_utf_8 name);
            need_gdata := true;
            let c = Var.fresh () in
            (Let (c, Constant const), noloc)
            :: ( Let
                   ( Var.fresh ()
                   , Prim
                       ( Extern "caml_js_set"
                       , [ Pv gdata
                         ; Pc (NativeString (Code.Native_string.of_string name))
                         ; Pv c
                         ] ) )
               , noloc )
            :: rem)
      in
      if !need_gdata
      then (Let (gdata, Prim (Extern "caml_get_global_data", [])), noloc) :: body
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
            (Let (x, Field (gdata, i)), noloc) :: l
        | _ -> l)
  in
  let body =
    if !need_gdata
    then (Let (gdata, Prim (Extern "caml_get_global_data", [])), noloc) :: body
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
  let step1 ~target t compunit code =
    if t.step2_started then assert false;
    let open Cmo_format in
    List.iter compunit.cu_primitives ~f:(fun name ->
        Hashtbl.add t.primitives name (Hashtbl.length t.primitives));
    let slot_for_literal sc =
      t.constants <- constant_of_const ~target sc :: t.constants;
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

let from_compilation_units ~target ~includes:_ ~include_cmis ~debug_data l =
  let reloc = Reloc.create () in
  List.iter l ~f:(fun (compunit, code) -> Reloc.step1 ~target reloc compunit code);
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
                let l = register_global globals i noloc l in
                let cst = globals.constants.(i) in
                (match cst, Code.Var.get_name x with
                | String str, None -> Code.Var.name x (Printf.sprintf "cst_%s" str)
                | _ -> ());
                (Let (x, Constant cst), noloc) :: l
            | Some name ->
                Var.name x name;
                need_gdata := true;
                ( Let
                    ( x
                    , Prim
                        ( Extern "caml_js_get"
                        , [ Pv gdata; Pc (NativeString (Native_string.of_string name)) ]
                        ) )
                , noloc )
                :: l)
        | _ -> l)
  in
  let body =
    if !need_gdata
    then (Let (gdata, Prim (Extern "caml_get_global_data", [])), noloc) :: body
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

let from_cmo ~target ?(includes = []) ?(include_cmis = false) ?(debug = false) compunit ic
    =
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
  let p =
    from_compilation_units ~target ~includes ~include_cmis ~debug_data [ compunit, code ]
  in
  Code.invariant p.code;
  p

let from_cma ~target ?(includes = []) ?(include_cmis = false) ?(debug = false) lib ic =
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
  let p = from_compilation_units ~target ~includes ~include_cmis ~debug_data units in
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
        assert (String.is_valid_utf_8 name);
        let exn = Var.fresh () in
        let v_name = Var.fresh () in
        let v_name_js = Var.fresh () in
        let v_index = Var.fresh () in
        [ Let (v_name, Constant (String name)), noloc
        ; Let (v_name_js, Constant (NativeString (Native_string.of_string name))), noloc
        ; ( Let
              ( v_index
              , Constant
                  (Int
                     ( (* Predefined exceptions are registered in
                          Symtable.init with [-index - 1] *)
                       Regular
                     , Int32.of_int (-index - 1) )) )
          , noloc )
        ; Let (exn, Block (248, [| v_name; v_index |], NotArray)), noloc
        ; ( Let
              ( Var.fresh ()
              , Prim
                  ( Extern "caml_register_global"
                  , [ Pc (Int (Regular, Int32.of_int index)); Pv exn; Pv v_name_js ] ) )
          , noloc )
        ])
    |> List.concat
  in
  let block = { params = []; body; branch = Stop, noloc } in
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

let link_info ~target ~symtable ~primitives ~crcs =
  let gdata = Code.Var.fresh_n "global_data" in
  let symtable_js =
    Ocaml_compiler.Symtable.GlobalMap.fold
      (fun i p acc -> (Ocaml_compiler.Symtable.Global.name i, p) :: acc)
      symtable
      []
    |> Array.of_list
  in
  let body = [] in
  let body =
    (* Include linking information *)
    let toc =
      [ "SYMB", Obj.repr symtable
      ; "SYJS", Obj.repr symtable_js
      ; "CRCS", Obj.repr crcs
      ; "PRIM", Obj.repr (String.concat ~sep:"\000" primitives ^ "\000")
      ]
    in
    let infos =
      [ "toc", Constants.parse ~target (Obj.repr toc)
      ; "prim_count", Int (Regular, Int32.of_int (List.length primitives))
      ]
    in
    let body =
      List.fold_left infos ~init:body ~f:(fun rem (name, const) ->
          let c = Var.fresh () in
          (Let (c, Constant const), noloc)
          :: ( Let
                 ( Var.fresh ()
                 , Prim
                     ( Extern "caml_js_set"
                     , [ Pv gdata
                       ; Pc (NativeString (Native_string.of_string name))
                       ; Pv c
                       ] ) )
             , noloc )
          :: rem)
    in
    (Let (gdata, Prim (Extern "caml_get_global_data", [])), noloc) :: body
  in
  let block = { params = []; body; branch = Stop, noloc } in
  { start = 0; blocks = Addr.Map.singleton 0 block; free_pc = 1 }
