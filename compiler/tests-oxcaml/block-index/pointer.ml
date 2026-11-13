(* TEST *)

(* Pointers: an unboxed pair of a base and a block index, read and written
   through [%unsafe_get_ptr]/[%unsafe_set_ptr].  These go through
   [caml_get_ptr_bytecode] and [caml_set_ptr_bytecode] in the JavaScript and
   Wasm runtimes. *)

open Stdlib_stable
open Stdlib_upstream_compatible

external unsafe_get_ptr : 'a ('b : any). #('a * ('a, 'b) idx_mut) -> 'b
  = "%unsafe_get_ptr"
[@@layout_poly]

external unsafe_set_ptr : 'a ('b : any). #('a * ('a, 'b) idx_mut) -> 'b -> unit
  = "%unsafe_set_ptr"
[@@layout_poly]

type boxed_record =
  { s : string
  ; mutable f : float
  }

let test_boxed_record () =
  let r = { s = "foo"; f = 1.0 } in
  assert (unsafe_get_ptr #(r, (.f)) = 1.0);
  unsafe_set_ptr #(r, (.f)) 2.0;
  assert (r.f = 2.0);
  assert (unsafe_get_ptr #(r, (.f)) = 2.0)

type mixed_record =
  { i : int
  ; mutable u : float#
  }

let test_mixed_record () =
  let r = { i = -100; u = #1.0 } in
  assert (Float_u.to_float (unsafe_get_ptr #(r, (.u))) = 1.0);
  unsafe_set_ptr #(r, (.u)) #2.0;
  assert (Float_u.to_float r.u = 2.0);
  assert (r.i = -100)

type pt =
  { x : int
  ; y : int
  }

type line =
  { mutable p : pt#
  ; mutable q : pt#
  }

(* A pointer whose index has depth 2. *)
let test_nested () =
  let l = { p = #{ x = 1; y = 2 }; q = #{ x = 3; y = 4 } } in
  assert (unsafe_get_ptr #(l, (.q.#y)) = 4);
  unsafe_set_ptr #(l, (.q.#y)) 40;
  assert (l.q.#y = 40);
  assert (l.q.#x = 3);
  assert (l.p.#y = 2)

let test_array () =
  let a = [| 10; 20; 30 |] in
  let i1 = Idx_mut.unsafe_create_into_array 1 in
  assert (unsafe_get_ptr #(a, i1) = 20);
  unsafe_set_ptr #(a, i1) 21;
  assert (a.(1) = 21);
  assert (a.(0) = 10)

(* The base is kept alive and shared: writing through a pointer is visible
   through the original value, and through a second pointer to the same
   field. *)
let test_aliasing () =
  let r = { s = "foo"; f = 0.0 } in
  let p1 = #(r, (.f)) in
  let p2 = #(r, (.f)) in
  unsafe_set_ptr p1 1.0;
  assert (unsafe_get_ptr p2 = 1.0);
  unsafe_set_ptr p2 2.0;
  assert (unsafe_get_ptr p1 = 2.0);
  assert (r.f = 2.0)

let () =
  test_boxed_record ();
  test_mixed_record ();
  test_nested ();
  test_array ();
  test_aliasing ();
  print_endline "OK"
