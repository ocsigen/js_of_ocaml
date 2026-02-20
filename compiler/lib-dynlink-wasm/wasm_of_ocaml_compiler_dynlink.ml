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

open Js_of_ocaml_compiler

external caml_wasm_load_wasmo : bytes -> unit = "caml_wasm_load_wasmo"

let read_file filename =
  let ic = open_in_bin filename in
  let n = in_channel_length ic in
  let b = Bytes.create n in
  really_input ic b 0 n;
  close_in ic;
  b

let loadfile filename =
  let b = read_file filename in
  caml_wasm_load_wasmo b

external wasm_load_module : string -> Obj.t = "caml_wasm_load_module"

external wasm_register_fragments : string -> string -> unit
  = "caml_wasm_register_fragments"

external toplevel_init_compile :
  (Obj.t -> Instruct.debug_event list array -> unit -> Obj.t) -> unit
  = "wasm_toplevel_init_compile"

external dynlink_init_sections : Obj.t -> unit = "wasm_dynlink_init_sections"

external get_named_global : string -> Obj.t = "wasm_get_named_global"

external get_ocaml_unit_list : unit -> string = "wasm_get_ocaml_unit_list"

external get_prim_list : unit -> string = "wasm_get_prim_list"

external get_crcs : unit -> string = "wasm_get_crcs"

external caml_register_global : int -> Obj.t -> string -> unit = "caml_register_global"

external caml_realloc_global : int -> unit = "caml_realloc_global"

type bytecode_sections =
  { symb : Ocaml_compiler.Symtable.GlobalMap.t
  ; crcs : (string * Digest.t option) list
  ; prim : string list
  ; dlpt : string list
  }
[@@ocaml.warning "-unused-field"]

let normalize_bytecode code =
  match Ocaml_version.compare Ocaml_version.current [ 5; 2 ] < 0 with
  | true -> code
  | false ->
      (* Starting with OCaml 5.2, the toplevel no longer appends [RETURN 1] *)
      let { Instr.opcode; _ } = Instr.find RETURN in
      let len = String.length code in
      let b = Bytes.create (len + 8) in
      Bytes.blit_string code 0 b 0 len;
      Bytes.set_int32_le b len (Int32.of_int opcode);
      Bytes.set_int32_le b (len + 4) 1l;
      Bytes.to_string b

let bigarray_to_string ba =
  let ba : (char, Bigarray.int8_unsigned_elt, Bigarray.c_layout) Bigarray.Array1.t =
    Obj.obj ba
  in
  let len = Bigarray.Array1.dim ba in
  let b = Bytes.create len in
  for i = 0 to len - 1 do
    Bytes.unsafe_set b i (Bigarray.Array1.unsafe_get ba i)
  done;
  Bytes.unsafe_to_string b

let fragments_to_js_source fragments =
  let open Javascript in
  let props =
    List.map
      (fun (nm, e) ->
        Property (PNS (Js_of_ocaml_compiler.Stdlib.Utf8_string.of_string_exn nm), e))
      fragments
  in
  let expr = EObj props in
  let prog = [ Expression_statement expr, N ] in
  Wasm_of_ocaml_compiler.Link.output_js prog

let register_fragments unit_name fragments =
  if fragments <> []
  then
    let source = fragments_to_js_source fragments in
    wasm_register_fragments unit_name source

let hex_to_digest s =
  (* Convert a 32-char hex string to a 16-byte binary digest *)
  let len = String.length s in
  if len <> 32
  then None
  else
    let b = Bytes.create 16 in
    let hex_val c =
      match c with
      | '0' .. '9' -> Char.code c - Char.code '0'
      | 'a' .. 'f' -> Char.code c - Char.code 'a' + 10
      | 'A' .. 'F' -> Char.code c - Char.code 'A' + 10
      | _ -> -1
    in
    try
      for i = 0 to 15 do
        let hi = hex_val s.[i * 2] in
        let lo = hex_val s.[(i * 2) + 1] in
        if hi < 0 || lo < 0 then raise Exit;
        Bytes.set b i (Char.chr ((hi lsl 4) lor lo))
      done;
      Some (Bytes.unsafe_to_string b)
    with Exit -> None

let parse_crcs s =
  if s = ""
  then []
  else
    s
    |> String.split_on_char '\x00'
    |> List.filter_map (fun entry ->
        if entry = ""
        then None
        else
          match String.index_opt entry '\x01' with
          | None -> None
          | Some idx ->
              let name = String.sub entry 0 idx in
              let hex = String.sub entry (idx + 1) (String.length entry - idx - 1) in
              let digest = if hex = "" then None else hex_to_digest hex in
              Some (name, digest))

let () =
  Config.set_target `Wasm;
  Config.set_effects_backend (Jsoo_runtime.Sys.Config.effects ());
  Wasm_of_ocaml_compiler.Generate.init ();
  (* Build GlobalMap *)
  let symb = ref Ocaml_compiler.Symtable.GlobalMap.empty in
  let predef_exns = Runtimedef.builtin_exceptions in
  Array.iter
    (fun name ->
      ignore
        (Ocaml_compiler.Symtable.GlobalMap.enter
           symb
           (Ocaml_compiler.Symtable.Global.Glob_predef name)))
    predef_exns;
  let unit_names =
    get_ocaml_unit_list ()
    |> String.split_on_char '\x00'
    |> List.filter (fun n -> not (Array.mem n predef_exns))
  in
  let num_predef = Array.length predef_exns in
  let total = num_predef + List.length unit_names in
  caml_realloc_global total;
  List.iter
    (fun name ->
      let idx =
        Ocaml_compiler.Symtable.GlobalMap.enter
          symb
          (Ocaml_compiler.Symtable.Global.Glob_compunit name)
      in
      let data = get_named_global name in
      caml_register_global idx data name)
    unit_names;
  (* Build and register bytecode sections *)
  let crcs =
    let parsed = parse_crcs (get_crcs ()) in
    if parsed = [] then List.map (fun n -> n, None) unit_names else parsed
  in
  let prim =
    let wasm_prims =
      get_prim_list () |> String.split_on_char '\x00' |> List.filter (fun s -> s <> "")
    in
    (* Some primitives are handled internally by wasm_of_ocaml and have
       no Wasm counterpart. *)
    let known = Hashtbl.create 512 in
    List.iter (fun p -> Hashtbl.replace known p ()) wasm_prims;
    let compiler_prims = Primitive.get_external () in
    let extra =
      Js_of_ocaml_compiler.Stdlib.StringSet.fold
        (fun p acc -> if Hashtbl.mem known p then acc else p :: acc)
        compiler_prims
        []
    in
    wasm_prims @ extra
  in
  let prims = Array.of_list prim in
  let sections = { symb = !symb; crcs; prim; dlpt = [] } in
  dynlink_init_sections (Obj.repr sections);
  let orig_units = Js_of_ocaml_compiler.Stdlib.StringSet.of_list unit_names in
  (* Register compile callback *)
  let toplevel_compile (code : Obj.t) (debug : Instruct.debug_event list array) :
      unit -> Obj.t =
    let s = bigarray_to_string code in
    let s = normalize_bytecode s in
    let wasm_binary, fragments =
      Wasm_of_ocaml_compiler.Generate.from_string
        ~prims
        ~debug
        ~orig_units
        ~unit_name:(Some "_dynlink")
        s
    in
    fun () ->
      register_fragments "_dynlink" fragments;
      wasm_load_module wasm_binary
  in
  toplevel_init_compile toplevel_compile
