(* Js_of_ocaml library
 * http://www.ocsigen.org/js_of_ocaml/
 * Copyright (C) 2016 OCamlPro
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

open Js_of_ocaml
open! Js_of_ocaml_toplevel
open Js_of_ocaml_toplevel_protocol
open Worker_msg

(* [handler] returns a [Wrapped.result] directly; the dispatcher maps that
   onto the wire [Worker_msg.ReturnSuccess]/[ReturnError] constructors. *)

let return_unit_success = Wrapped.Success ((), [])

let return_exn exn = Wrapped.Error (Wrapped.error_of_exn exn, [])

(** File descriptors *)

(* Limit the frequency of sent messages to one per ms, using an active
   loop (yuck) because, well, there is no other concurrency primitive
   and we do not want to fill a memory buffer but really "pause" the
   program.

   The problem arises with debug off and developper tools off only.
   In this case, with a program that does a lot of writes (print or
   callbacks), the messages queue fills up super quickly and kills the
   browser / tab.

   The wait is adaptive: it only spins when the previous message was sent less
   than a millisecond ago, so an occasional [print] is not slowed down.

   A possible improvement would be to bufferize the messages channel
   per channel, and emit the buffer of each channel every ms if it has
   changed. But it could cause bad asynchronicity in case the worker
   does a big computation just after a bufferized write. And it would
   still need some kind of active waiting to limit throughput. All in
   all this spinwait is not that ugly.

   TODO: per-channel batching (see #833 review). [requestAnimationFrame] is
   not available in a Web Worker, so it cannot replace this. *)
let last = ref 0.

let rec wait () =
  let now = Sys.time () in
  (* [Sys.time] is [Date.now]-based and can jump backwards (NTP, suspend/resume).
     Proceed on a backward jump too, otherwise the spin would freeze (and stall
     the worker, and the host with it) until wall-clock time caught back up. *)
  if now -. !last > 0.001 || now < !last then last := now else wait ()

let post_message (m : toploop_msg) =
  (* Throttle only the high-frequency [Write] messages (stdout/stderr and
     user callbacks): a program printing in a tight loop is what floods the
     host message queue and can kill the tab. Control replies are bounded
     (one per request), so send them immediately rather than adding latency. *)
  (match m with
  | Write _ -> wait ()
  | ReturnSuccess _ | ReturnError _ -> ());
  Worker.post_message (Json.output m)

let wrap_fd, close_fd, clear_fds =
  let fds = ref Fd.Map.empty in
  let wrap_fd fd =
    try Fd.Map.find fd !fds
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
      fds := Fd.Map.add fd ppf !fds;
      ppf
  in
  let close_fd fd =
    if Fd.Map.mem fd !fds then Format.pp_print_flush (Fd.Map.find fd !fds) ();
    fds := Fd.Map.remove fd !fds
  in
  let clear_fds () =
    (* Keep the persistent stdout/stderr channels across a reset. *)
    fds :=
      Fd.Map.fold
        (fun id ppf fds ->
          Format.pp_print_flush ppf ();
          if Fd.is_reserved id then Fd.Map.add id ppf fds else fds)
        !fds
        Fd.Map.empty
  in
  wrap_fd, close_fd, clear_fds

(** Code compilation and execution *)

(* TODO protect execution with a mutex! *)

(* Parsing sessions held by the worker and stepped one phrase at a time, keyed
   by the id the host allocated. Each entry keeps the [code_fd] (if any) so the
   echo formatter can be flushed and dropped when the lexbuf is closed. *)
let lexbufs = ref Lexbuf.Map.empty

(** Message dispatcher *)

let handler : type a. a host_msg -> a Wrapped.result = function
  | Init { cmis_base_url } ->
      (* Fetch the cmi bundle only if the cmis are not already present. They are
         when the worker was built without [--no-cmis]: [js_of_ocaml] embeds them
         at [/static/cmis] (see [Pseudo_fs]), the very path [jsoo_mkcmis] and the
         bundle use, so a missing [stdlib.cmi] there means we must load it. *)
      if not (Sys.file_exists "/static/cmis/stdlib.cmi")
      then Worker.import_scripts [ cmis_base_url ^ "stdlib.cmis.js" ];
      Direct.initialize ();
      return_unit_success
  | Reset ->
      clear_fds ();
      (* Drop any open parsing sessions: they belong to the environment being
         wiped (their code fds are already gone with [clear_fds]). *)
      lexbufs := Lexbuf.Map.empty;
      (* Drop any scratch typing env before wiping the toplevel, so the
         saved-env snapshot is not left dangling across the reset. *)
      Wrapped.clear_check ();
      (* Reset via Direct so the cmi load path survives (plain
         [Toploop.initialize_toplevel_env] would drop it). *)
      Direct.reset_toplevel_env ();
      return_unit_success
  | Check { setenv; code } -> Wrapped.check () ~setenv code
  | Clear_check ->
      Wrapped.clear_check ();
      return_unit_success
  | Execute { code_fd; print_outcome; answer_fd; code } ->
      let ppf_code = Option.map wrap_fd code_fd in
      let ppf_answer = wrap_fd answer_fd in
      let result = Wrapped.execute () ?ppf_code ~print_outcome ~ppf_answer code in
      Option.iter close_fd code_fd;
      close_fd answer_fd;
      result
  | Use_string { filename; print_outcome; answer_fd; code } ->
      let ppf_answer = wrap_fd answer_fd in
      let result = Wrapped.use () ?filename ~print_outcome ~ppf_answer code in
      close_fd answer_fd;
      result
  | Use_mod_string { answer_fd; print_outcome; modname; sig_code; code } ->
      let ppf_answer = wrap_fd answer_fd in
      let result =
        Wrapped.use_mod_string () ~ppf_answer ~print_outcome ~modname ?sig_code code
      in
      close_fd answer_fd;
      result
  | Open_lexbuf { id; code; code_fd } ->
      (* Defensive against an id reused while a prior session under it is still
         open (host counter desync): release the old echo fd first. *)
      (match Lexbuf.Map.find_opt id !lexbufs with
      | Some (_, old_code_fd) -> Option.iter close_fd old_code_fd
      | None -> ());
      let ppf_code = Option.map wrap_fd code_fd in
      let lb = Wrapped.make_lexbuf ?ppf_code code in
      lexbufs := Lexbuf.Map.add id (lb, code_fd) !lexbufs;
      return_unit_success
  | Step { lexbuf; print_outcome; answer_fd } -> (
      match Lexbuf.Map.find_opt lexbuf !lexbufs with
      | None ->
          Wrapped.Error
            ({ Wrapped.msg = "Toplevel worker: unknown lexbuf"; locs = [] }, [])
      | Some (lb, _) ->
          let ppf_answer = wrap_fd answer_fd in
          let result = Wrapped.step () ~print_outcome ~ppf_answer lb in
          close_fd answer_fd;
          result)
  | Close_lexbuf { lexbuf } ->
      (match Lexbuf.Map.find_opt lexbuf !lexbufs with
      | None -> ()
      | Some (_lb, code_fd) -> Option.iter close_fd code_fd);
      lexbufs := Lexbuf.Map.remove lexbuf !lexbufs;
      return_unit_success
  | Import_scripts urls -> (
      try
        Worker.import_scripts urls;
        return_unit_success
      with exn -> return_exn exn)

(* Install the channel flushers and the [onmessage] handler that drives the
   toplevel. Call this once from the worker's entry point; nothing happens at
   module load. *)
let start () =
  let stdout_ppf = wrap_fd Fd.stdout in
  let stderr_ppf = wrap_fd Fd.stderr in
  Sys_js.set_channel_flusher stdout (fun s ->
      Format.pp_print_string stdout_ppf s;
      Format.pp_print_flush stdout_ppf ());
  Sys_js.set_channel_flusher stderr (fun s ->
      Format.pp_print_string stderr_ppf s;
      Format.pp_print_flush stderr_ppf ());
  let dispatch (type t) data =
    let (id, data) : Message_id.t * t host_msg = Json.unsafe_input data in
    (* Once [id] is decoded, never let an exception escape: the host has
       queued a wakener under [id] and will hang forever if no
       [ReturnSuccess] or [ReturnError] ever arrives. Both the dispatch
       ([handler]) and the reply ([ty_of_host_msg], [post_message]) run
       under this guard, so any failure is reported back as a
       [ReturnError] under [id]. Decoding [id] itself is necessarily
       outside the guard; a failure there is caught by the host's worker
       [onerror] handler instead. *)
    try
      let ty = ty_of_host_msg data in
      let result = try handler data with exn -> return_exn exn in
      match result with
      | Wrapped.Success (v, w) -> post_message (Worker_msg.ReturnSuccess (id, ty, v, w))
      | Wrapped.Error (res, w) -> post_message (Worker_msg.ReturnError (id, res, w))
    with exn ->
      (* [ty_of_host_msg], or the success [post_message] (e.g. a reply
         value [Json.output] cannot marshal), raised after [handler] had
         already returned; surface it under [id] rather than hang. The
         [error_of_exn] payload is plain data, so this reply marshals. *)
      post_message (Worker_msg.ReturnError (id, Wrapped.error_of_exn exn, []))
  in
  Worker.set_onmessage (fun s -> dispatch s)
