(* Regression test for the bytecode-sections accessors in both the JS
   (toplevel.js) and Wasm (toplevel.wat) runtimes. When the program is not
   compiled with --toplevel, the accessor must raise
   Failure "Program not compiled with --toplevel". The wasm runtime used to
   cast the absent sections to a block (trapping with "illegal cast") or
   return it silently instead. *)

external get_bytecode_sections : unit -> Obj.t = "caml_dynlink_get_bytecode_sections"

let () =
  match get_bytecode_sections () with
  | (_ : Obj.t) -> assert false
  | exception Failure msg -> assert (msg = "Program not compiled with --toplevel")
