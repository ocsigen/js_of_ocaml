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

(* Exercises Promise chaining end-to-end. Output is captured by node after
   the microtask queue drains, so both synchronous and asynchronous prints
   end up in the .expected file. To keep ordering deterministic we run
   every step inside a single chain rather than several independent ones. *)

open Js_of_ocaml

let log s = print_endline s

let ( >>= ) p f = Promise.then_ f p

let return = Promise.resolve

let test_resolve_then_map () =
  return 1
  |> Promise.map (fun x -> x + 1)
  >>= fun x ->
  log (Printf.sprintf "then_/map got %d" x);
  return ()

let test_make () =
  Promise.make (fun ~resolve ~reject:_ -> resolve "hello")
  >>= fun s ->
  log (Printf.sprintf "make got %s" s);
  return ()

let test_catch () =
  let p = Promise.reject (Promise.error_of_any (Js.Unsafe.inject (Js.string "boom"))) in
  Promise.catch
    (fun e ->
      let s : Js.js_string Js.t = Js.Unsafe.coerce (Promise.error_to_any e) in
      log (Printf.sprintf "caught %s" (Js.to_string s));
      return ())
    p

let test_finally_ok () = return () |> Promise.finally (fun () -> log "finally ok")

let test_finally_err () =
  Promise.reject (Promise.error_of_any (Js.Unsafe.inject (Js.string "x")))
  |> Promise.finally (fun () -> log "finally err")
  |> Promise.catch (fun _ -> return ())

let test_all () =
  Promise.all [ return 10; return 20; return 30 ]
  >>= fun xs ->
  log (Printf.sprintf "all got [%s]" (String.concat "; " (List.map string_of_int xs)));
  return ()

let test_race () =
  Promise.race [ return 42; return 99 ]
  >>= fun x ->
  log (Printf.sprintf "race got %d" x);
  return ()

let test_no_flatten () =
  let inner : int Promise.t = return 7 in
  (return inner : int Promise.t Promise.t)
  >>= fun (p : int Promise.t) ->
  p
  >>= fun n ->
  log (Printf.sprintf "nested got %d" n);
  return ()

let test_of_any_foreign () =
  (* A foreign JS promise whose resolved value was never wrapped by us.
     [then_] must pass it through unchanged rather than reading [.wrapped]. *)
  let foreign : Js.Unsafe.any =
    Js.Unsafe.meth_call Js.Unsafe.global##._Promise "resolve" [| Js.Unsafe.inject 99 |]
  in
  Promise.of_any foreign
  >>= fun (n : int) ->
  log (Printf.sprintf "of_any got %d" n);
  return ()

let test_reject_with_promise () =
  (* JS [Promise.reject] does not auto-follow thenables (only [resolve]
     does), so a promise as the rejection reason should reach [catch]
     intact and be recoverable via [of_any]. *)
  let inner : int Promise.t = return 555 in
  let p : unit Promise.t = Promise.reject (Promise.error_of_any (Promise.to_any inner)) in
  Promise.catch
    (fun e ->
      let recovered : int Promise.t = Promise.of_any (Promise.error_to_any e) in
      recovered
      >>= fun n ->
      log (Printf.sprintf "reject-with-promise got %d" n);
      return ())
    p

let () =
  log "start";
  let _ : unit Promise.t =
    test_resolve_then_map ()
    >>= test_make
    >>= test_catch
    >>= test_finally_ok
    >>= test_finally_err
    >>= test_all
    >>= test_race
    >>= test_no_flatten
    >>= test_of_any_foreign
    >>= test_reject_with_promise
    >>= fun () ->
    log "done";
    return ()
  in
  log "scheduled"
