(* Js_of_ocaml
 * http://www.ocsigen.org/js_of_ocaml/
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
 *)

open Js_of_ocaml

let%expect_test "is_supported" =
  print_endline (if Promise.is_supported () then "PASSED" else "FAILED");
  [%expect {| PASSED |}]

let%expect_test "to_any returns a Promise instance" =
  let p = Promise.resolve 42 in
  let any = Promise.to_any p in
  let is_promise =
    Js.instanceof
      (Js.Unsafe.coerce any : _ Js.t)
      (Js.Unsafe.global##._Promise : _ Js.constr)
  in
  print_endline (if is_promise then "PASSED" else "FAILED");
  [%expect {| PASSED |}]

let%expect_test "make invokes the body synchronously" =
  let called = ref false in
  let _p = Promise.make (fun ~resolve:_ ~reject:_ -> called := true) in
  print_endline (if !called then "PASSED" else "FAILED");
  [%expect {| PASSED |}]

let%expect_test "reject is a Promise" =
  let p : unit Promise.t =
    Promise.reject (Promise.error_of_any (Js.Unsafe.inject (Js.string "boom")))
  in
  let any = Promise.to_any p in
  let is_promise =
    Js.instanceof
      (Js.Unsafe.coerce any : _ Js.t)
      (Js.Unsafe.global##._Promise : _ Js.constr)
  in
  (* Suppress the unhandled-rejection warning by attaching a noop catch. *)
  let _ = Promise.catch (fun _ -> Promise.resolve ()) p in
  print_endline (if is_promise then "PASSED" else "FAILED");
  [%expect {| PASSED |}]

let%expect_test "all/race build Promises without blowing up" =
  let p1 = Promise.resolve 1 in
  let p2 = Promise.resolve 2 in
  let pa = Promise.all [ p1; p2 ] in
  let pr = Promise.race [ p1; p2 ] in
  let is_promise any =
    Js.instanceof
      (Js.Unsafe.coerce any : _ Js.t)
      (Js.Unsafe.global##._Promise : _ Js.constr)
  in
  print_endline (if is_promise (Promise.to_any pa) then "all OK" else "all KO");
  print_endline (if is_promise (Promise.to_any pr) then "race OK" else "race KO");
  [%expect {|
    all OK
    race OK
    |}]

let%expect_test "all_settled/any build Promises without blowing up" =
  let p1 = Promise.resolve 1 in
  let p2 = Promise.resolve 2 in
  let pas = Promise.all_settled [ p1; p2 ] in
  let pan = Promise.any [ p1; p2 ] in
  let is_promise any =
    Js.instanceof
      (Js.Unsafe.coerce any : _ Js.t)
      (Js.Unsafe.global##._Promise : _ Js.constr)
  in
  print_endline
    (if is_promise (Promise.to_any pas) then "all_settled OK" else "all_settled KO");
  print_endline (if is_promise (Promise.to_any pan) then "any OK" else "any KO");
  [%expect {|
    all_settled OK
    any OK
    |}]

let%expect_test "with_resolvers returns a Promise" =
  let p, _resolve, _reject = Promise.with_resolvers () in
  let is_promise =
    Js.instanceof
      (Js.Unsafe.coerce (Promise.to_any p) : _ Js.t)
      (Js.Unsafe.global##._Promise : _ Js.constr)
  in
  print_endline (if is_promise then "PASSED" else "FAILED");
  [%expect {| PASSED |}]
