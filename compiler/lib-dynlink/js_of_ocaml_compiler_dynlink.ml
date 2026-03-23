open Js_of_ocaml_compiler.Stdlib
open Js_of_ocaml_compiler
module J = Jsoo_runtime.Js

external get_bytecode_sections : unit -> Parse_bytecode.bytesections
  = "jsoo_get_bytecode_sections"

external get_runtime_aliases : unit -> (string * string) list = "jsoo_get_runtime_aliases"

external toplevel_init_compile :
  (string -> Instruct.debug_event list array -> unit -> J.t) -> unit
  = "jsoo_toplevel_init_compile"

external toplevel_init_reloc : (Parse_bytecode.Global_name.t -> int) -> unit
  = "jsoo_toplevel_init_reloc"

let eval_ref = ref (fun (_ : string) -> failwith "toplevel: eval not initialized")

let () =
  (match Sys.backend_type with
  | Sys.Other "js_of_ocaml" -> Config.set_target `JavaScript
  | Sys.(Native | Bytecode | Other _) -> failwith "Expected backend `js_of_ocaml`");
  let aliases = get_runtime_aliases () in
  let global = J.pure_js_expr "globalThis" in
  Build_info.set_values
    (Build_info.config_keys `JavaScript)
    (Build_info.parse_config_string (Jsoo_runtime.Sys.Config.build_config ()));
  Linker.reset ();
  List.iter aliases ~f:(fun (a, b) -> Primitive.alias a b);
  (* this needs to stay synchronized with toplevel.js *)
  let toplevel_compile (s : string) (debug : Instruct.debug_event list array) :
      unit -> J.t =
    let s = Parse_bytecode.normalize_bytecode s in
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
  let sym : Ocaml_compiler.Symtable.GlobalMap.t = toc.symb in
  let toplevel_reloc (gn : Parse_bytecode.Global_name.t) : int =
    match
      Ocaml_compiler.Symtable.GlobalMap.find
        (Parse_bytecode.Global_name.to_symtable_global gn)
        sym
    with
    | i -> i
    | exception Not_found ->
        Ocaml_compiler.Symtable.reloc_ident (Parse_bytecode.Global_name.to_string gn)
  in
  eval_ref := toplevel_eval;
  toplevel_init_compile toplevel_compile;
  toplevel_init_reloc toplevel_reloc
