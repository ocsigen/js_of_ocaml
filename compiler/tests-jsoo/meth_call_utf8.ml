(* [caml_js_meth_call] must decode the method name as UTF-8, like the JS
   runtime (which uses [caml_jsstring_of_string]). A method name with a
   non-ASCII character otherwise resolves differently on the wasm backend
   (where it used to be read as raw bytes). Exercised on js and wasm. *)

open Js_of_ocaml

let () =
  (* "café" — the é (U+00E9) is two bytes in UTF-8. *)
  let name = "café" in
  let o = Js.Unsafe.obj [||] in
  (* Install the method under the UTF-8-decoded name (a real JS string). *)
  Js.Unsafe.set o (Js.string name) (Js.wrap_callback (fun () -> 42));
  (* Call it through meth_call with the raw OCaml string; the runtime must
     UTF-8-decode it to find the same property. *)
  let r : int = Js.Unsafe.meth_call o name [||] in
  assert (r = 42);
  print_endline "ok"
