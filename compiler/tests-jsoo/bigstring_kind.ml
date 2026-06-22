(* Bigstring.of_arrayBuffer must produce a char (kind 12) bigarray, like
   the JS runtime, not an int8_unsigned (kind 3) one. The kind affects
   polymorphic compare and marshalling against real char bigarrays.
   Exercised on js and wasm. *)
open Js_of_ocaml
open Typed_array

let%expect_test ("Bigstring.of_arrayBuffer produces a char bigarray" [@when not wasi]) =
  let a = Bigarray.Array1.create Bigarray.char Bigarray.c_layout 3 in
  a.{0} <- 'a';
  a.{1} <- 'b';
  a.{2} <- 'c';
  let bs = Bigstring.of_arrayBuffer (new%js arrayBuffer 3) in
  bs.{0} <- 'a';
  bs.{1} <- 'b';
  bs.{2} <- 'c';
  Printf.printf "compare=%d\n" (compare a bs);
  Printf.printf "marshal_equal=%b\n" (Marshal.to_string a [] = Marshal.to_string bs []);
  [%expect {|
    compare=0
    marshal_equal=true
    |}]
