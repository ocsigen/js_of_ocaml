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

external toplevel_init_compile :
  (Obj.t -> Instruct.debug_event list array -> unit -> Obj.t) -> unit
  = "wasm_toplevel_init_compile"

external dynlink_init_sections : Obj.t -> unit = "wasm_dynlink_init_sections"

external get_named_global : string -> Obj.t = "wasm_get_named_global"

external get_ocaml_unit_list : unit -> string = "wasm_get_ocaml_unit_list"

external caml_register_global : int -> Obj.t -> string -> unit = "caml_register_global"

external caml_realloc_global : int -> unit = "caml_realloc_global"

type bytecode_sections =
  { symb : Symtable.global_map
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

let loadfile filename =
  let ic = open_in_bin filename in
  Fun.protect ~finally:(fun () -> close_in ic) (fun () ->
      match Parse_bytecode.from_channel ic with
      | `Cmo cmo ->
          let unit_name = Ocaml_compiler.Cmo_format.name cmo in
          let one = Parse_bytecode.from_cmo ~debug:false cmo ic in
          let wasm_binary, _fragments =
            Wasm_of_ocaml_compiler.Generate.compile ~unit_name:(Some unit_name) one.code
          in
          ignore (Wasm_of_ocaml_dynlink.load_module_bytes (Bytes.of_string wasm_binary))
      | `Cma cma ->
          List.iter
            (fun cmo ->
              let unit_name = Ocaml_compiler.Cmo_format.name cmo in
              let one = Parse_bytecode.from_cmo ~debug:false cmo ic in
              let wasm_binary, _fragments =
                Wasm_of_ocaml_compiler.Generate.compile
                  ~unit_name:(Some unit_name)
                  one.code
              in
              ignore
                (Wasm_of_ocaml_dynlink.load_module_bytes (Bytes.of_string wasm_binary)))
            cma.Cmo_format.lib_units
      | `Exe -> failwith "loadfile: executable files not supported")

let () =
  Config.set_target `Wasm;
  Config.set_effects_backend (Jsoo_runtime.Sys.Config.effects ());
  Wasm_of_ocaml_compiler.Generate.init ();
  (* Discover all named Wasm globals from imports.OCaml *)
  let all_names =
    get_ocaml_unit_list ()
    |> String.split_on_char '\x00'
    |> List.filter (fun s -> s <> "")
  in
  let predef_exns = Array.to_list Runtimedef.builtin_exceptions in
  let unit_names =
    List.filter (fun n -> not (List.mem n predef_exns)) all_names
  in
  (* Build GlobalMap — predefs first (indices 0..N-1), then units (N..) *)
  let symb_ref = ref Ocaml_compiler.Symtable.GlobalMap.empty in
  List.iter
    (fun name ->
      ignore
        (Ocaml_compiler.Symtable.GlobalMap.enter
           symb_ref
           (Ocaml_compiler.Symtable.Global.Glob_predef name)))
    predef_exns;
  (* Grow caml_global_data and populate with unit data *)
  let num_predef = List.length predef_exns in
  let total = num_predef + List.length unit_names in
  caml_realloc_global total;
  List.iter
    (fun name ->
      let idx =
        Ocaml_compiler.Symtable.GlobalMap.enter
          symb_ref
          (Ocaml_compiler.Symtable.Global.Glob_compunit name)
      in
      let data = get_named_global name in
      caml_register_global idx data name)
    unit_names;
  (* Build and register bytecode sections *)
  let symb : Symtable.global_map = Obj.magic !symb_ref in
  let crcs = List.map (fun n -> (n, None)) (predef_exns @ unit_names) in
  let prim =
    let runtime_prims = Ocaml_compiler.Symtable.all_primitives () in
    let compiler_prims = Primitive.get_external () in
    let known = Hashtbl.create 512 in
    List.iter (fun p -> Hashtbl.replace known p ()) runtime_prims;
    let extra =
      Js_of_ocaml_compiler.Stdlib.StringSet.fold
        (fun p acc -> if Hashtbl.mem known p then acc else p :: acc)
        compiler_prims
        []
    in
    runtime_prims @ extra
  in
  let sections = { symb; crcs; prim; dlpt = [] } in
  dynlink_init_sections (Obj.repr sections);
  (* Also initialize compiler-libs.bytecomp's Symtable so that
     Parse_bytecode.from_bytes can resolve global names *)
  Symtable.restore_state symb;
  (* Register compile callback *)
  let toplevel_compile (code : Obj.t) (debug : Instruct.debug_event list array) :
      unit -> Obj.t =
    let s = bigarray_to_string code in
    let s = normalize_bytecode s in
    let prims = Array.of_list (Ocaml_compiler.Symtable.all_primitives ()) in
    let wasm_binary, _fragments =
      Wasm_of_ocaml_compiler.Generate.from_string
        ~prims
        ~debug
        ~unit_name:(Some "_dynlink")
        s
    in
    fun () ->
      Wasm_of_ocaml_dynlink.load_module_bytes (Bytes.of_string wasm_binary)
  in
  toplevel_init_compile toplevel_compile
