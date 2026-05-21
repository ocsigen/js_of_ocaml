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

(* Round-trip Promise <-> Lwt conversions. *)

open Js_of_ocaml
open Js_of_ocaml_lwt

let log s = print_endline s

let ( let* ) = Lwt.bind

let main : unit Lwt.t =
  log "start";
  (* Promise -> Lwt: resolved value flows through *)
  let* n = Promise.to_lwt (Promise.resolve 11) in
  log (Printf.sprintf "to_lwt resolved %d" n);
  (* Promise -> Lwt: rejection becomes Promise.Rejected *)
  let* () =
    Lwt.catch
      (fun () ->
        let* () =
          Promise.to_lwt
            (Promise.reject (Promise.error_of_any (Js.Unsafe.inject (Js.string "boom"))))
        in
        log "to_lwt rejection NOT caught";
        Lwt.return ())
      (function
        | Promise.Rejected e ->
            let s : Js.js_string Js.t = Js.Unsafe.coerce (Promise.error_to_any e) in
            log (Printf.sprintf "to_lwt rejected with %s" (Js.to_string s));
            Lwt.return ()
        | exn ->
            log (Printf.sprintf "unexpected exn %s" (Printexc.to_string exn));
            Lwt.return ())
  in
  (* Lwt -> Promise: returned value flows through *)
  let* n = Promise.to_lwt (Promise.of_lwt (Lwt.return 22)) in
  log (Printf.sprintf "of_lwt round-trip %d" n);
  (* Lwt -> Promise: failed thread rejects, then to_lwt re-raises *)
  let* () =
    Lwt.catch
      (fun () ->
        let* () = Promise.to_lwt (Promise.of_lwt (Lwt.fail (Failure "lwt-boom"))) in
        log "of_lwt failure NOT caught";
        Lwt.return ())
      (fun _ ->
        log "of_lwt failure round-trip";
        Lwt.return ())
  in
  log "done";
  Lwt.return ()

let () = Lwt.async (fun () -> main)
