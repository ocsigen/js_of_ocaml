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

external caml_wasm_load_wasmo : string -> unit = "caml_wasm_load_wasmo"

let read_file filename =
  let ic = open_in_bin filename in
  let n = in_channel_length ic in
  let s = really_input_string ic n in
  close_in ic;
  s

let loadfile filename =
  let b = read_file filename in
  caml_wasm_load_wasmo b

external wasm_load_module : string -> Obj.t = "caml_wasm_load_module"

external wasm_register_fragments : string -> string -> unit
  = "caml_wasm_register_fragments"

external toplevel_init_compile :
  (string -> Instruct.debug_event list array -> unit -> Obj.t) -> unit
  = "wasm_toplevel_init_compile"

external get_bytecode_sections : unit -> Parse_bytecode.bytesections
  = "wasm_get_bytecode_sections"

external toplevel_init_reloc : (string -> int) -> unit = "wasm_toplevel_init_reloc"

external get_runtime_aliases : unit -> (string * string) list = "wasm_get_runtime_aliases"

let fragments_to_js_source fragments =
  let open Javascript in
  let props =
    List.map
      (fun (nm, e) -> Property (PNS (Stdlib.Utf8_string.of_string_exn nm), e))
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

let () =
  Config.set_target `Wasm;
  Config.set_effects_backend (Jsoo_runtime.Sys.Config.effects ());
  Wasm_of_ocaml_compiler.Generate.init ();
  let aliases = get_runtime_aliases () in
  List.iter (fun (a, b) -> Primitive.alias a b) aliases;
  (* Read bytecode sections set up by the _link_info module *)
  let toc = get_bytecode_sections () in
  let prims = Array.of_list toc.prim in
  (* Build a name->index map from the symtable for the reloc callback
     (same as the JS dynlink module) *)
  let sym =
    let t : Ocaml_compiler.Symtable.GlobalMap.t = toc.symb in
    Ocaml_compiler.Symtable.GlobalMap.fold
      (fun i n acc -> Stdlib.StringMap.add (Ocaml_compiler.Symtable.Global.name i) n acc)
      t
      Stdlib.StringMap.empty
  in
  let toplevel_reloc (name : string) : int =
    match Stdlib.StringMap.find_opt name sym with
    | Some i -> i
    | None -> Ocaml_compiler.Symtable.reloc_ident name
  in
  let toplevel_compile code (debug : Instruct.debug_event list array) : unit -> Obj.t =
    let s = Parse_bytecode.normalize_bytecode code in
    let wasm_binary, fragments =
      Wasm_of_ocaml_compiler.Generate.from_string
        ~prims
        ~debug
        ~unit_name:(Some "_dynlink")
        s
    in
    fun () ->
      register_fragments "_dynlink" fragments;
      wasm_load_module wasm_binary
  in
  toplevel_init_compile toplevel_compile;
  toplevel_init_reloc toplevel_reloc
