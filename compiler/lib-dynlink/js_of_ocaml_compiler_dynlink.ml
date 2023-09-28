open Js_of_ocaml_compiler.Stdlib
open Js_of_ocaml_compiler
module J = Jsoo_runtime.Js

let split_primitives p =
  let len = String.length p in
  let rec split beg cur =
    if cur >= len
    then []
    else if Char.equal p.[cur] '\000'
    then String.sub p ~pos:beg ~len:(cur - beg) :: split (cur + 1) (cur + 1)
    else split beg (cur + 1)
  in
  Array.of_list (split 0 0)

external get_section_table : unit -> (string * Obj.t) list = "caml_get_section_table"

let () =
  let global = J.pure_js_expr "globalThis" in
  Config.Flag.set "use-js-string" (Jsoo_runtime.Sys.Config.use_js_string ());
  Config.Flag.set "effects" (Jsoo_runtime.Sys.Config.effects ());
  (* this needs to stay synchronized with toplevel.js *)
  let toplevel_compile (s : string) (debug : Instruct.debug_event list array) :
      unit -> J.t =
    let prims = split_primitives (Symtable.data_primitive_names ()) in
    let b = Buffer.create 100 in
    let fmt = Pretty_print.to_buffer b in
    Driver.configure fmt;
    Driver.from_string ~prims ~debug s fmt;
    Format.(pp_print_flush std_formatter ());
    Format.(pp_print_flush err_formatter ());
    flush stdout;
    flush stderr;
    let js = Buffer.contents b in
    let res : string -> unit -> J.t =
      Obj.magic (J.get global (J.string "toplevelEval"))
    in
    res (js : string)
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
  let toc = get_section_table () in
  let sym =
    let t : Ocaml_compiler.Symtable.GlobalMap.t = Obj.obj (List.assoc "SYMB" toc) in
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
  J.set global (J.string "toplevelCompile") (Obj.magic toplevel_compile) (*XXX HACK!*);
  J.set global (J.string "toplevelEval") (Obj.magic toplevel_eval);
  J.set global (J.string "toplevelReloc") (Obj.magic toplevel_reloc)
