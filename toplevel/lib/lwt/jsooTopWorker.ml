(* Js_of_ocaml library
 * http://www.ocsigen.org/js_of_ocaml/
 * Copyright (C) 2016 OCamlPro
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU Library General Public License as published by
 * the Free Software Foundation, with linking exception;
 * either version 2.1 of the License, or (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Library General Public License for more details.
 *
 * You should have received a copy of the GNU Library General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
 *)

open Js_of_ocaml
open! Js_of_ocaml_toplevel
open JsooTopWorkerIntf

type 'a return =
  | ReturnSuccess of 'a * JsooTopWrapped.warning list
  | ReturnError of JsooTopWrapped.error * JsooTopWrapped.warning list

let return_success v w = ReturnSuccess (v, w)

let return_unit_success = return_success () []

let return_error e w = ReturnError (e, w)

let return_exn exn = return_error (JsooTopWrapped.error_of_exn exn) []

let unwrap_result : _ JsooTopWrapped.result -> _ = function
  | Success (b, w) -> return_success b w
  | Error (err, w) -> return_error err w

(** File descriptors *)

module IntMap = Map.Make (struct
  type t = int

  let compare (x : int) (y : int) = compare x y
end)

(* Limit the frequency of sent messages to one per ms, using an active
   loop (yuck) because, well, there is no other concurrency primitive
   and we do not want to fill a memory buffer but really "pause" the
   program.

   The problem arises with debug off and developper tools off only.
   In this case, with a program that does a lot of writes (print or
   callbacks), the messages queue fills up super quickly and kills the
   browser / tab.

   A possible improvement would be to bufferize the messages channel
   per channel, and emit the buffer of each channel every ms if it has
   changed. But it could cause bad asynchronicity in case the worker
   does a big computation just after a bufferized write. And it would
   still need some kind of active waiting to limit throughput. All in
   all this spinwait is not that ugly. *)
let last = ref 0.

let rec wait () =
  let now = Sys.time () (* let's hope this yields a bit *) in
  if now -. !last > 0.001 then last := now else wait ()

let post_message (m : toploop_msg) =
  wait ();
  Worker.post_message (Json.output m)

let wrap_fd, close_fd, clear_fds =
  let fds = ref IntMap.empty in
  let wrap_fd fd =
    try IntMap.find fd !fds
    with Not_found ->
      let buf = Buffer.create 503 in
      let flush () =
        let s = Buffer.contents buf in
        if s <> ""
        then (
          Buffer.reset buf;
          post_message (Write (fd, s)))
      in
      let ppf = Format.make_formatter (Buffer.add_substring buf) flush in
      fds := IntMap.add fd ppf !fds;
      ppf
  in
  let close_fd fd =
    if IntMap.mem fd !fds then Format.pp_print_flush (IntMap.find fd !fds) ();
    fds := IntMap.remove fd !fds
  in
  let clear_fds () =
    fds :=
      IntMap.fold
        (fun id ppf fds ->
          Format.pp_print_flush ppf ();
          if id = 0 || id = 1 then IntMap.add id ppf fds else fds)
        !fds
        IntMap.empty
  in
  wrap_fd, close_fd, clear_fds

let stdout_ppf = wrap_fd 0

let stderr_ppf = wrap_fd 1

let () =
  Sys_js.set_channel_flusher stdout (fun s ->
      Format.pp_print_string stdout_ppf s;
      Format.pp_print_flush stdout_ppf ());
  Sys_js.set_channel_flusher stderr (fun s ->
      Format.pp_print_string stderr_ppf s;
      Format.pp_print_flush stderr_ppf ())

(** Code compilation and execution *)

(* TODO protect execution with a mutex! *)

(** Message dispatcher *)

let map_option f o =
  match o with
  | None -> None
  | Some o -> Some (f o)

let iter_option f o =
  match o with
  | None -> ()
  | Some o -> f o

let handler : type a. a host_msg -> a return = function
  | Init prefix ->
      Worker.import_scripts [prefix ^ "stdlib.cmis.js"];
      JsooTop.initialize ();
      return_unit_success
  | Reset ->
      clear_fds ();
      Toploop.initialize_toplevel_env ();
      return_unit_success
  | Check (setenv, code) ->
      let result = JsooTopWrapped.check () ~setenv code in
      unwrap_result result
  | Execute (fd_code, print_outcome, fd_answer, code) ->
      let ppf_code = map_option wrap_fd fd_code in
      let ppf_answer = wrap_fd fd_answer in
      let result = JsooTopWrapped.execute () ?ppf_code ~print_outcome ~ppf_answer code in
      iter_option close_fd fd_code;
      close_fd fd_answer;
      unwrap_result result
  | Use_string (filename, print_outcome, fd_answer, code) ->
      let ppf_answer = wrap_fd fd_answer in
      let result =
        JsooTopWrapped.use_string () ?filename ~print_outcome ~ppf_answer code
      in
      close_fd fd_answer;
      unwrap_result result
  | Use_mod_string (fd_answer, print_outcome, modname, sig_code, impl_code) ->
      let ppf_answer = wrap_fd fd_answer in
      let result =
        JsooTopWrapped.use_mod_string
          ()
          ~ppf_answer
          ~print_outcome
          ~modname
          ?sig_code
          impl_code
      in
      close_fd fd_answer;
      unwrap_result result
  | Import_scripts urls -> (
    try
      Worker.import_scripts urls;
      return_unit_success
    with exn -> return_exn exn)

let ty_of_host_msg : type t. t host_msg -> t msg_ty = function
  | Init _ -> Unit
  | Reset -> Unit
  | Check _ -> Unit
  | Execute _ -> Bool
  | Use_string _ -> Bool
  | Use_mod_string _ -> Bool
  | Import_scripts _ -> Unit

let () =
  let handler (type t) data =
    let (id, data) : int * t host_msg = Json.unsafe_input data in
    let ty = ty_of_host_msg data in
    match handler data with
    | ReturnSuccess (v, w) ->
        post_message (JsooTopWorkerIntf.ReturnSuccess (id, ty, v, w))
    | ReturnError (res, w) -> post_message (JsooTopWorkerIntf.ReturnError (id, res, w))
  in
  Hashtbl.add
    Toploop.directive_table
    "cmis"
    (Toploop.Directive_string (fun name -> Worker.import_scripts [name]));
  Worker.set_onmessage (fun s -> handler s)
