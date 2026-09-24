(* With --enable portable-int, OCaml integers that do not fit in 31 bits are
   boxed. They must be converted from and to JavaScript numbers at the
   boundary, as integers that fit in 31 bits are. *)

open Js_of_ocaml

external nativeint_of_js : Js.Unsafe.any -> nativeint = "caml_js_to_nativeint"

external nativeint_to_js : nativeint -> Js.Unsafe.any = "caml_js_from_nativeint"

let describe : Js.Unsafe.any -> string =
 fun x ->
  Js.to_string
    (Js.Unsafe.fun_call (Js.Unsafe.pure_js_expr "(x => typeof x + ':' + x)") [| x |])

let () =
  let big = Sys.opaque_identity ((1 lsl 40) + 3) in
  print_endline (describe (Js.Unsafe.inject big));
  print_endline (describe (Js.Unsafe.inject (-big)));
  let n : int = Js.Unsafe.eval_string "2**40 + 7" in
  Printf.printf "%d\n" (n + 1);
  let n : int = Js.Unsafe.eval_string "-(2**31)" in
  Printf.printf "%d\n" (n - 1);
  Printf.printf "%.1f\n" (Js.float_of_number (Js.Unsafe.eval_string "2**40 + 0.5"));
  let s : Js.js_string Js.t =
    Js.Unsafe.meth_call
      (Js.string "hello")
      "slice"
      [| Js.Unsafe.inject 1; Js.Unsafe.inject (Sys.opaque_identity (1 lsl 40)) |]
  in
  print_endline (Js.to_string s);
  Printf.printf "%nd\n" (nativeint_of_js (Js.Unsafe.eval_string "2**40 + 3"));
  print_endline (describe (nativeint_to_js (Sys.opaque_identity 0x10000000003n)))
