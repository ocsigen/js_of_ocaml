(* Regression tests for ephemeron data handling, exercised through the
   low-level [Obj.Ephemeron] primitives on both the JavaScript and wasm
   runtimes.

   - [caml_ephe_get_data] used to trap in the wasm runtime (weak.wat): it
     unwrapped and recast the stored data unconditionally, so when the data
     is a JS value reached through an integer key (no weak map threads it),
     the unwrap stripped the JS box and the recast (or [ref.as_non_null] on
     JS null) trapped instead of returning the value.

   - [caml_ephe_blit_data] from an empty source left the destination's old
     data in place in the wasm runtime ([caml_ephe_set_data_opt] ignored
     [None]); native and the JS runtime clear it. *)

module E = Obj.Ephemeron
module Js = Js_of_ocaml.Js

let%expect_test "Obj.Ephemeron data" =
  (* blit_data from an empty source must clear the destination. *)
  let dst = E.create 1 in
  E.set_data dst (Obj.repr 42);
  assert (E.check_data dst);
  let src = E.create 1 in
  E.blit_data src dst;
  assert (not (E.check_data dst));

  (* get_data must not trap when the data is a JS value reached through
     an integer key. *)
  let e = E.create 1 in
  E.set_key e 0 (Obj.repr 5);
  E.set_data e (Obj.repr (Js.string "hello"));
  (match E.get_data e with
  | Some d -> assert (Js.to_string (Obj.obj d) = "hello")
  | None -> assert false);

  (* Same, with a JS value that unwraps to null (used to trap on
     [ref.as_non_null]). *)
  let e2 = E.create 1 in
  E.set_key e2 0 (Obj.repr 7);
  E.set_data e2 (Obj.repr Js.null);
  match E.get_data e2 with
  | Some _ -> () (* reached without trapping *)
  | None -> assert false
