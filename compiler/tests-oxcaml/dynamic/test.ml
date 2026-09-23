(* TEST *)

(* Dynamic variables: [caml_dynamic_make], [caml_dynamic_get],
   [caml_dynamic_push] and [caml_dynamic_pop].  A variable reads as [Null]
   outside any binding, as the innermost bound value inside one, and the
   previous state is restored on the way out -- including when the body
   raises. *)

let d : int Dynamic.t = Dynamic.make ()

let e : string Dynamic.t = Dynamic.make ()

let get t =
  match Dynamic.get t with
  | Null -> None
  | This v -> Some v

exception Boom

let test_unbound () = assert (get d = None)

let test_bound () =
  Dynamic.with_temporarily d 42 ~f:(fun () -> assert (get d = Some 42));
  assert (get d = None)

let test_nested () =
  Dynamic.with_temporarily d 1 ~f:(fun () ->
      assert (get d = Some 1);
      Dynamic.with_temporarily d 2 ~f:(fun () -> assert (get d = Some 2));
      (* Popping restores the outer binding, not the unbound state. *)
      assert (get d = Some 1));
  assert (get d = None)

(* Distinct variables do not interfere. *)
let test_independent () =
  Dynamic.with_temporarily d 7 ~f:(fun () ->
      assert (get e = None);
      Dynamic.with_temporarily e "x" ~f:(fun () ->
          assert (get d = Some 7);
          assert (get e = Some "x"));
      assert (get d = Some 7);
      assert (get e = None));
  assert (get d = None)

(* An escaping exception still pops the binding. *)
let test_unwind () =
  (try Dynamic.with_temporarily d 5 ~f:(fun () -> raise Boom) with
  | Boom -> ());
  assert (get d = None)

let () =
  test_unbound ();
  test_bound ();
  test_nested ();
  test_independent ();
  test_unwind ();
  print_endline "OK"
