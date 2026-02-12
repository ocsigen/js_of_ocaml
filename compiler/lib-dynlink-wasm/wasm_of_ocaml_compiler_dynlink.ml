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
  (Obj.t -> Instruct.debug_event list array -> (unit -> unit)) -> unit
  = "wasm_toplevel_init_compile"

external dynlink_init_sections : Obj.t -> unit = "wasm_dynlink_init_sections"

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
          Wasm_of_ocaml_dynlink.load_module_bytes (Bytes.of_string wasm_binary)
      | `Cma _ -> failwith "loadfile: .cma files not yet supported"
      | `Exe -> failwith "loadfile: executable files not supported")

let () =
  Config.set_target `Wasm;
  Config.set_effects_backend `Disabled;
  Wasm_of_ocaml_compiler.Generate.init ();
  (* Register dummy bytecode sections so that Symtable.init_toplevel succeeds.
     A proper implementation will embed real sections at link time (step 5). *)
  let sections =
    { symb = Symtable.empty_global_map
    ; crcs = [ "Stdlib", None ]
    ; prim = Ocaml_compiler.Symtable.all_primitives ()
    ; dlpt = []
    }
  in
  dynlink_init_sections (Obj.repr sections);
  let toplevel_compile (code : Obj.t) (debug : Instruct.debug_event list array) :
      unit -> unit =
    let s = bigarray_to_string code in
    let s = normalize_bytecode s in
    let prims = Array.of_list (Ocaml_compiler.Symtable.all_primitives ()) in
    let wasm_binary, _fragments =
      Wasm_of_ocaml_compiler.Generate.from_string ~prims ~debug ~unit_name:None s
    in
    fun () -> Wasm_of_ocaml_dynlink.load_module_bytes (Bytes.of_string wasm_binary)
  in
  toplevel_init_compile toplevel_compile
