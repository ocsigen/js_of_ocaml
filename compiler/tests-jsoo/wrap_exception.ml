(* caml_wrap_exception must match the JS runtime: a thrown JS value that is
   not an Error becomes Failure (String value), not a Js.Error, and a thrown
   null/undefined must not crash the wrapper (String() is null-safe, unlike
   value.toString()). Exercised on js and wasm. *)

open Js_of_ocaml

let classify f =
  match f () with
  | () -> "no exception"
  | exception Failure m -> "Failure " ^ m
  | exception e -> "other: " ^ Printexc.to_string e

let%expect_test ("caml_wrap_exception" [@when not wasi]) =
  assert (
    classify (fun () -> ignore (Js.Unsafe.eval_string "(function(){throw 'boom'})()"))
    = "Failure boom");
  assert (
    classify (fun () -> ignore (Js.Unsafe.eval_string "(function(){throw null})()"))
    = "Failure null");
  print_endline "ok";
  [%expect {| ok |}]
