(* Js_of_ocaml tests
 * http://www.ocsigen.org/js_of_ocaml/
 * Copyright (C) 2024 Hugo Heuzard
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU Lesser General Public License as published by
 * the Free Software Foundation, with linking exception;
 * either version 2.1 of the License, or (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
 *)

let%expect_test _ =
  let k1 = Some 2 in
  let k2 = Some 3 in
  let k3 = Some 4 in
  let d = k1, k2, k3 in
  let e = Ephemeron.Kn.make [| k1; k2; k3 |] d in
  (match Ephemeron.Kn.query e [| k1; k2; k3 |] with
  | None -> print_endline "none"
  | Some d' ->
      assert (d = d');
      print_endline "found");
  [%expect {| found |}]

let%expect_test _ =
  let module K = struct
    type t = int option

    let equal (a : t) (b : t) = a = b

    let hash (x : t) = Hashtbl.hash x
  end in
  let module T = Ephemeron.Kn.Make (K) in
  let f y =
    let k1 = Some 2 in
    let k2 = Some 3 in
    let k3 = Some y in
    let d = k1, k2, k3 in
    let t = T.create 10 in
    T.add t [| k1; k2; k3 |] d;
    T.add t [| k2; k3; k1 |] d;
    T.add t [| k3; k1; k2 |] d;
    match T.find_opt t [| k1; k2; k3 |] with
    | None -> print_endline "none"
    | Some d' ->
        assert (d = d');
        print_endline "found"
  in
  f 3;
  f 2;
  [%expect {|
    found
    found
  |}]

let copy_eq a b =
  if a == b
  then false
  else
    let a = Obj.repr a in
    let b = Obj.repr b in
    if Obj.size a <> Obj.size b || Obj.tag a <> Obj.tag b
    then false
    else
      let exception False in
      try
        for i = 0 to Obj.size a - 1 do
          if Obj.field a i != Obj.field b i then raise False
        done;
        true
      with False -> false

let bool x = Printf.printf "%b" x

let%expect_test _ =
  let module E = Obj.Ephemeron in
  let ki = Obj.repr None in
  let k1 = Obj.repr (Some 2) in
  let k2 = Obj.repr (Some 43) in
  let e = E.create 10 in
  let e2 = E.create 3 in
  Printf.printf "%d\n" (E.length e);
  [%expect {| 10 |}];
  E.set_key e 1 ki;
  E.set_key e 2 k1;
  E.set_key e 3 k2;
  bool (Option.get (E.get_key e 2) == k1);
  [%expect {| true |}];
  bool (Option.get (E.get_key_copy e 2) == k1);
  [%expect {| false |}];
  bool (copy_eq (Option.get (E.get_key_copy e 2)) k1);
  [%expect {| true |}];
  bool (Option.get (E.get_key e 1) == ki);
  [%expect {| true |}];
  bool (Option.get (E.get_key_copy e 1) == ki);
  [%expect {| true |}];
  bool (copy_eq (Option.get (E.get_key_copy e 1)) ki);
  [%expect {| false |}];
  bool (E.check_key e 0);
  [%expect {| false |}];
  bool (E.check_key e 2);
  [%expect {| true |}];
  bool (E.check_key e 3);
  [%expect {| true |}];
  E.unset_key e 3;
  bool (E.check_key e 3);
  [%expect {| false |}];

  bool (E.check_data e);
  [%expect {| false |}];
  E.set_data e k1;
  bool (E.check_data e);
  [%expect {| true |}];

  bool (Option.get (E.get_data e) == k1);
  [%expect {| true |}];
  bool (Option.get (E.get_data_copy e) == k1);
  [%expect {| false |}];
  bool (copy_eq (Option.get (E.get_data_copy e)) k1);
  [%expect {| true |}];

  E.set_data e ki;
  bool (Option.get (E.get_data e) == ki);
  [%expect {| true |}];
  bool (Option.get (E.get_data_copy e) == ki);
  [%expect {| true |}];
  bool (copy_eq (Option.get (E.get_data_copy e)) ki);
  [%expect {| false |}];

  bool (E.check_data e2);
  [%expect {| false |}];
  E.blit_data e e2;
  bool (E.check_data e2);
  [%expect {| true |}];

  E.blit_key e 1 e2 0 3;
  bool (E.check_key e2 0);
  [%expect {| true |}];
  bool (E.check_key e2 1);
  [%expect {| true |}];
  bool (E.check_key e2 2);
  [%expect {| false |}];

  E.unset_data e;
  bool (E.check_data e);
  [%expect {| false |}]

(* Regression test for https://github.com/ocsigen/js_of_ocaml/issues/2263:
   [blit_key] must only copy keys; it must not modify the destination's
   data. *)
let%expect_test "blit_key does not touch data" =
  let module E = Obj.Ephemeron in
  let print_data e =
    match E.get_data e with
    | None -> print_endline "no data"
    | Some d -> print_endline (Obj.obj d : string)
  in
  let k1 = Some 1 and k2 = Some 2 and k3 = Some 3 and k4 = Some 4 in
  let src = E.create 2 in
  let dst = E.create 2 in
  E.set_key src 0 (Obj.repr k1);
  E.set_key src 1 (Obj.repr k2);
  E.set_key dst 0 (Obj.repr k3);
  E.set_key dst 1 (Obj.repr k4);
  E.set_data src (Obj.repr "src data");
  E.set_data dst (Obj.repr "dst data");
  E.blit_key src 0 dst 0 2;
  print_data dst;
  [%expect {| dst data |}];
  (* blitting from a source without data must not clear the destination's
     data either *)
  E.unset_data src;
  E.set_data dst (Obj.repr "dst data");
  E.blit_key src 0 dst 0 2;
  print_data dst;
  [%expect {| dst data |}];
  ignore (Sys.opaque_identity (k1, k2, k3, k4))
