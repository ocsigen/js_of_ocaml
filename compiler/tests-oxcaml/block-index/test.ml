(* TEST *)

(* Block indices: reads, writes and index deepening over the block shapes the
   runtime has to walk (plain blocks, nested unboxed records).  These go
   through [caml_get_idx_bytecode], [caml_set_idx_bytecode] and
   [caml_deepen_idx_bytecode] in the JavaScript and Wasm runtimes. *)

open Stdlib_upstream_compatible

(* The accessors are declared here rather than taken from [Stdlib_stable]
   because OxCaml 5.4 renamed them: the layout-polymorphic primitives went
   from [%unsafe_get_idx]/[%unsafe_set_idx] ([Idx_mut.unsafe_get]/
   [unsafe_set]) to [%get_idx]/[%set_idx] ([Idx_mut.get]/[set]). *)

external idx_get : 'a ('b : any). 'a -> ('a, 'b) idx_mut -> 'b
  = "%unsafe_get_idx"
[@@layout_poly] [@@if ocaml_version < (5, 4, 0)]

external idx_get : ('a : value_or_null) ('b : any). 'a -> ('a, 'b) idx_mut -> 'b
  = "%get_idx"
[@@layout_poly] [@@if ocaml_version >= (5, 4, 0)]

external idx_set : 'a ('b : any). 'a -> ('a, 'b) idx_mut -> 'b -> unit
  = "%unsafe_set_idx"
[@@layout_poly] [@@if ocaml_version < (5, 4, 0)]

external idx_set
  : ('a : value_or_null) ('b : any). 'a -> ('a, 'b) idx_mut -> 'b -> unit
  = "%set_idx"
[@@layout_poly] [@@if ocaml_version >= (5, 4, 0)]

external idx_imm_get : 'a ('b : any). 'a -> ('a, 'b) idx_imm -> 'b
  = "%unsafe_get_idx_imm"
[@@layout_poly] [@@if ocaml_version < (5, 4, 0)]

external idx_imm_get
  : ('a : value_or_null) ('b : any). 'a -> ('a, 'b) idx_imm -> 'b
  = "%get_idx_imm"
[@@layout_poly] [@@if ocaml_version >= (5, 4, 0)]

type boxed_record =
  { s : string
  ; mutable f : float
  }

let test_boxed_record () =
  let r = { s = "foo"; f = 1.0 } in
  assert (idx_imm_get r (.s) = "foo");
  assert (idx_get r (.f) = 1.0);
  idx_set r (.f) 2.0;
  assert (r.f = 2.0);
  assert (idx_get r (.f) = 2.0)

type mixed_record =
  { i : int
  ; mutable u : float#
  ; str : string
  }

let test_mixed_record () =
  let r = { i = -100; u = #1.0; str = "foo" } in
  assert (Float_u.to_float (idx_get r (.u)) = 1.0);
  idx_set r (.u) #2.0;
  assert (Float_u.to_float r.u = 2.0);
  assert (r.i = -100);
  assert (r.str = "foo")

(* Nested unboxed record: the index has depth 2, so the runtime must follow
   two field positions. *)
type nested_record =
  { g : float#
  ; mutable inner : boxed_record#
  }

let test_nested_record () =
  let r = { g = -#100.0; inner = #{ s = "foo"; f = 1.0 } } in
  assert (idx_get r (.inner.#f) = 1.0);
  assert (idx_get r (.inner.#s) = "foo");
  idx_set r (.inner.#f) 2.0;
  assert (r.inner.#f = 2.0);
  assert (r.inner.#s = "foo")

type pt =
  { x : int
  ; y : int
  }

type line =
  { mutable p : pt#
  ; mutable q : pt#
  }

let test_deepening () =
  let l = { p = #{ x = 1; y = 2 }; q = #{ x = 3; y = 4 } } in
  let q : (line, pt#) idx_mut = (.q) in
  (* Deepening an existing index goes through [caml_deepen_idx_bytecode]. *)
  let qy : (line, int) idx_mut = (.idx_mut(q).#y) in
  assert (idx_get l qy = 4);
  idx_set l qy 40;
  assert (l.q.#y = 40);
  assert (l.q.#x = 3);
  assert (l.p.#y = 2)

(* Deepening twice: the runtime concatenates three positions. *)
type inner =
  { a : pt#
  ; b : int
  }

type outer = { mutable o : inner# }

let test_deepening_twice () =
  let v = { o = #{ a = #{ x = 1; y = 2 }; b = 3 } } in
  let o : (outer, inner#) idx_mut = (.o) in
  let oa : (outer, pt#) idx_mut = (.idx_mut(o).#a) in
  let oax : (outer, int) idx_mut = (.idx_mut(oa).#x) in
  assert (idx_get v oax = 1);
  idx_set v oax 10;
  assert (v.o.#a.#x = 10);
  assert (v.o.#a.#y = 2);
  assert (v.o.#b = 3)

let () =
  test_boxed_record ();
  test_mixed_record ();
  test_nested_record ();
  test_deepening ();
  test_deepening_twice ();
  print_endline "OK"
