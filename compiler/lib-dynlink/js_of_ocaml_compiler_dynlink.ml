open Js_of_ocaml_compiler.Stdlib
open Js_of_ocaml_compiler
module J = Jsoo_runtime.Js

type bytecode_sections =
  { symb : Ocaml_compiler.Symtable.GlobalMap.t
  ; crcs : (string * Digest.t option) list
  ; prim : string list
  ; dlpt : string list
  }
[@@ocaml.warning "-unused-field"]

external get_bytecode_sections : unit -> bytecode_sections = "jsoo_get_bytecode_sections"

external get_runtime_aliases : unit -> (string * string) list = "jsoo_get_runtime_aliases"

external toplevel_init_compile :
  (string -> Instruct.debug_event list array -> unit -> J.t) -> unit
  = "jsoo_toplevel_init_compile"

external toplevel_init_reloc : (J.t -> int) -> unit = "jsoo_toplevel_init_reloc"

let eval_ref = ref (fun (_ : string) -> failwith "toplevel: eval not initialized")

let normalize_bytecode code =
  match Ocaml_version.compare Ocaml_version.current [ 5; 2 ] < 0 with
  | true -> code
  | false ->
      (* starting with ocaml 5.2, The toplevel no longer append [RETURN 1] *)
      let { Instr.opcode; _ } = Instr.find RETURN in
      let len = String.length code in
      let b = Bytes.create (len + 8) in
      Bytes.blit_string ~src:code ~src_pos:0 ~dst:b ~dst_pos:0 ~len;
      Bytes.set_int32_le b len (Int32.of_int opcode);
      Bytes.set_int32_le b (len + 4) 1l;
      Bytes.to_string b

let () =
  (match Sys.backend_type with
  | Sys.Other "js_of_ocaml" -> Config.set_target `JavaScript
  | Sys.(Native | Bytecode | Other _) -> failwith "Expected backend `js_of_ocaml`");
  let aliases = get_runtime_aliases () in
  let global = J.pure_js_expr "globalThis" in
  Config.Flag.set "use-js-string" (Jsoo_runtime.Sys.Config.use_js_string ());
  Config.set_effects_backend (Jsoo_runtime.Sys.Config.effects ());
  Linker.reset ();
  List.iter aliases ~f:(fun (a, b) -> Primitive.alias a b);
  (* this needs to stay synchronized with toplevel.js *)
  let toplevel_compile (s : string) (debug : Instruct.debug_event list array) :
      unit -> J.t =
    let s = normalize_bytecode s in
    let prims = Array.of_list (Ocaml_compiler.Symtable.all_primitives ()) in
    let b = Buffer.create 100 in
    let fmt = Pretty_print.to_buffer b in
    Driver.configure fmt;
    Driver.from_string ~prims ~debug s fmt;
    Format.(pp_print_flush std_formatter ());
    Format.(pp_print_flush err_formatter ());
    flush stdout;
    flush stderr;
    let js = Buffer.contents b in
    !eval_ref js
  in
  let toplevel_eval (x : string) : unit -> J.t =
    let f : J.t = J.eval_string x in
    fun () ->
      let res = J.fun_call f [| global |] in
      Format.(pp_print_flush std_formatter ());
      Format.(pp_print_flush err_formatter ());
      flush stdout;
      flush stderr;
      res
  in
  let toc = get_bytecode_sections () in
  let sym =
    let t : Ocaml_compiler.Symtable.GlobalMap.t = toc.symb in
    Ocaml_compiler.Symtable.GlobalMap.fold
      (fun i n acc -> StringMap.add (Ocaml_compiler.Symtable.Global.name i) n acc)
      t
      StringMap.empty
  in
  let toplevel_reloc (name : J.t) : int =
    let name = J.to_string name in
    match StringMap.find_opt name sym with
    | Some i -> i
    | None -> Js_of_ocaml_compiler.Ocaml_compiler.Symtable.reloc_ident name
  in
  eval_ref := toplevel_eval;
  toplevel_init_compile toplevel_compile;
  toplevel_init_reloc toplevel_reloc
