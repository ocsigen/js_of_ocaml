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
open! Js_of_ocaml_toplevel_protocol
open Worker_msg

type 'a result = 'a Wrapped_intf.result Lwt.t

let ( >>= ) = Lwt.bind

let ( >>? ) o f =
  let open! Wrapped_intf in
  o
  >>= function
  | Error (err, w) -> Lwt.return (Error (err, w))
  | Success (x, w) -> (
      f x
      >>= function
      | Error (err, w') -> Lwt.return (Error (err, w @ w'))
      | Success (x, w') -> Lwt.return (Success (x, w @ w')))

let return_success e = Lwt.return (Wrapped_intf.Success (e, []))

let return_unit_success = return_success ()

type u =
  | U : 'a msg_ty * 'a Wrapped_intf.result Lwt.u * 'a Wrapped_intf.result Lwt.t -> u

type output = string -> unit

(* [pp_stdout] is stored but not read yet (FIXME below); disable the
   unused-field warning (69) for the record. A per-field attribute did not
   suppress it on every OCaml version, so it is set on the whole declaration. *)
type toplevel =
  { cmis_base_url : string
  ; js_file : string
  ; mutable imported : string list
  ; mutable pending_imports : (string * unit Wrapped_intf.result Lwt.t) list
  ; mutable worker : (Js.js_string Js.t, Js.js_string Js.t) Worker.worker Js.t
  ; mutable wakeners : u Message_id.Map.t
  ; mutable counter : Message_id.t
  ; mutable fds : output Fd.Map.t
  ; mutable fd_counter : Fd.t
  ; mutable lexbuf_counter : Lexbuf.t
  ; mutable reset_worker : toplevel -> unit Lwt.t
  ; mutable after_init : toplevel -> unit Lwt.t
  ; pp_stdout : output (* FIXME: kept for API symmetry; not yet routed *)
  ; pp_stderr : output
  }
[@@warning "-69"]

(* Host-side handle for a worker-side parsing session: [lb_id] is what crosses
   the wire; [lb_code_fd] is the echo descriptor to release when it is closed. *)
type lexbuf =
  { lb_id : Lexbuf.t
  ; lb_code_fd : Fd.t option
  }

exception Not_equal

let check_equal : type t1 t2. t1 msg_ty -> t2 msg_ty -> (t1, t2) eq =
 fun ty1 ty2 ->
  match ty1, ty2 with
  | Unit, Unit -> Eq
  | Bool, Bool -> Eq
  | Int, Int -> Eq
  | String, String -> Eq
  | Step_result, Step_result -> Eq
  | Unit, _ -> raise Not_equal
  | Bool, _ -> raise Not_equal
  | Int, _ -> raise Not_equal
  | String, _ -> raise Not_equal
  | Step_result, _ -> raise Not_equal

let onmessage worker (ev : (_, _) Dom_html.messageEvent Js.t) =
  match Json.unsafe_input ev##.data with
  | Write (fd, s) -> (
      try
        Fd.Map.find fd worker.fds s;
        Js._false
      with Not_found ->
        Console.console##warn
          (Js.string (Printf.sprintf "Missing channels (%d)" (Fd.to_int fd)));
        Js._false)
  | ReturnSuccess (id, ty_v, v, w) -> (
      match Message_id.Map.find_opt id worker.wakeners with
      | None ->
          Console.console##warn
            (Js.string (Printf.sprintf "Missing wakeners (%d)" (Message_id.to_int id)));
          Js._false
      | Some (U (ty_u, u, t)) ->
          worker.wakeners <- Message_id.Map.remove id worker.wakeners;
          (* The thread may already have been cancelled by the caller; waking a
             resolved [u] would raise. Drop the late reply in that case. *)
          (if Lwt.is_sleeping t
           then
             match check_equal ty_u ty_v with
             | Eq -> Lwt.wakeup u (Wrapped_intf.Success (v, w))
             | exception Not_equal ->
                 Console.console##warn
                   (Js.string
                      (Printf.sprintf "Unexpected wakeners (%d)" (Message_id.to_int id)));
                 let err =
                   { Wrapped_intf.msg =
                       Printf.sprintf
                         "Worker returned a value of unexpected type for request %d"
                         (Message_id.to_int id)
                   ; locs = []
                   }
                 in
                 Lwt.wakeup u (Wrapped_intf.Error (err, w)));
          Js._false)
  | ReturnError (id, e, w) -> (
      match Message_id.Map.find_opt id worker.wakeners with
      | None ->
          Console.console##warn
            (Js.string (Printf.sprintf "Missing wakeners (%d)" (Message_id.to_int id)));
          Js._false
      | Some (U (_, u, t)) ->
          worker.wakeners <- Message_id.Map.remove id worker.wakeners;
          if Lwt.is_sleeping t then Lwt.wakeup u (Wrapped_intf.Error (e, w));
          Js._false)

(* A worker [error] event means the worker script failed to load (e.g.
   a missing or wrong [js_file]) or an uncaught error escaped it. Either
   way, any request currently in flight will never receive a reply, so we
   resolve every pending wakener with an [Error] instead of leaving the
   corresponding Lwt thread to hang forever. We deliberately do not
   respawn the worker here: a permanently failing [js_file] would
   otherwise loop forever, and a recoverable error (the worker keeps
   running) leaves it ready for the next request. Callers can {!reset} to
   get a fresh worker. *)
let fail_pending worker err =
  let wakeners = worker.wakeners in
  worker.wakeners <- Message_id.Map.empty;
  Message_id.Map.iter
    (fun _id (U (_, u, t)) ->
      (* Skip threads already resolved/cancelled: waking them would raise and
         abort the iteration, stranding the rest. *)
      if Lwt.is_sleeping t then Lwt.wakeup u (Wrapped_intf.Error (err, [])))
    wakeners

let onerror worker (ev : Worker.errorEvent Js.t) =
  let msg =
    Printf.sprintf
      "Toplevel worker error: %s (%s:%d)"
      (Js.to_string ev##.message)
      (Js.to_string ev##.filename)
      ev##.lineno
  in
  Console.console##error (Js.string msg);
  fail_pending worker { Wrapped_intf.msg; locs = [] };
  Js._false

let install_handlers worker =
  worker.worker##.onmessage := Dom.handler (onmessage worker);
  worker.worker##.onerror := Dom.handler (onerror worker)

let terminate worker =
  worker.worker##terminate;
  Message_id.Map.iter
    (fun id (U (_, _, t)) ->
      worker.wakeners <- Message_id.Map.remove id worker.wakeners;
      Lwt.cancel t)
    worker.wakeners

let never_ending () =
  (* and not cancellable. A fresh thread per call so it (and the [Lwt.choose]
     branch built on it) can be collected once the reset resolves, instead of
     piling waiters onto one shared, never-resolving thread. *)
  fst (Lwt.wait ())

(** Threads created with [post] will always be wake-uped by
    [onmessage] by calling [Lwt.wakeup]. They should never end with
    an exception, unless canceled. When canceled, the worker is
    killed and a new one is spawned. *)
let rec post : type a. toplevel -> a host_msg -> a Wrapped_intf.result Lwt.t =
 fun worker msg ->
  let msg_id = worker.counter in
  let msg_ty = ty_of_host_msg msg in
  let t, u = Lwt.task () in
  (* Capture [worker.reset_worker] eagerly: if this cancel is fired from
     inside [terminate] (itself called from the current reset), the field
     still points to the in-progress closure whose [running] flag is
     already [false], so the scheduled reset is a no-op. Reading the
     field lazily at async-fire time would see the fresh closure
     installed at the end of the reset and trigger a cascade. *)
  Lwt.on_cancel t (fun () ->
      let reset = worker.reset_worker in
      Lwt.async (fun () -> reset worker));
  worker.wakeners <- Message_id.Map.add msg_id (U (msg_ty, u, t)) worker.wakeners;
  worker.counter <- Message_id.next msg_id;
  worker.worker##postMessage (Json.output (msg_id, msg));
  t

and do_reset_worker () =
  let running = ref true in
  fun worker ->
    if !running
    then (
      running := false;
      terminate worker;
      worker.worker <- Worker.create worker.js_file;
      worker.fds <-
        Fd.Map.empty
        |> Fd.Map.add Fd.stdout (Fd.Map.find Fd.stdout worker.fds)
        |> Fd.Map.add Fd.stderr (Fd.Map.find Fd.stderr worker.fds);
      worker.fd_counter <- Fd.first_free;
      (* Keep [lexbuf_counter] monotonic across generations (like [counter]
         below). The respawned worker has no parsing sessions, so a stale handle
         held from before the reset must not alias a freshly opened one; with a
         monotonic counter a stale [step] hits "unknown lexbuf" instead. *)
      let imported = worker.imported in
      worker.imported <- [];
      worker.pending_imports <- [];
      worker.wakeners <- Message_id.Map.empty;
      (* Keep [counter] monotonic across worker generations: a stale reply
         still queued from the terminated worker must never share an id
         with a freshly issued request, or [onmessage] would resolve the
         new wakener with the dead worker's result. *)
      worker.reset_worker <- do_reset_worker ();
      install_handlers worker;
      Lwt_list.iter_p
        (fun name -> import_cmis_js worker name >>= fun _ -> Lwt.return_unit)
        imported
      >>= fun () ->
      post worker @@ Init { cmis_base_url = worker.cmis_base_url }
      >>= function
      | Wrapped_intf.Error (err, _) ->
          (* A respawned worker that cannot re-initialize: report it and
             skip [after_init] (its setup phrases would all fail) rather
             than pretend the reset succeeded. *)
          worker.pp_stderr err.Wrapped_intf.msg;
          Lwt.return_unit
      | Wrapped_intf.Success ((), _) ->
          worker.after_init worker >>= fun _ -> Lwt.return_unit)
    else Lwt.return_unit

and import_cmis_js worker name =
  if List.mem name worker.imported
  then return_unit_success
  else
    (* Concurrent imports of the same [name] share a single underlying
       [post] thread. Hand every caller a [Lwt.protected] view of it so
       that one caller cancelling its handle cannot cancel the shared
       thread (which, via [post]'s [on_cancel], would tear down the whole
       worker and fail the other importers). Cancellation still flows the
       other way: a real worker teardown cancels the shared thread, which
       propagates to all protected views. *)
    match List.assoc_opt name worker.pending_imports with
    | Some t -> Lwt.protected t
    | None ->
        let url = worker.cmis_base_url ^ name ^ ".cmis.js" in
        let t =
          post worker @@ Import_scripts [ url ]
          >>? fun () ->
          worker.imported <- name :: worker.imported;
          return_unit_success
        in
        worker.pending_imports <- (name, t) :: worker.pending_imports;
        let remove_pending () =
          worker.pending_imports <- List.remove_assoc name worker.pending_imports
        in
        Lwt.on_any t (fun _ -> remove_pending ()) (fun _ -> remove_pending ());
        Lwt.protected t

exception Init_failed of Wrapped_intf.error

let create
    ?(cmis_base_url = "")
    ?(after_init = fun _ -> Lwt.return_unit)
    ~pp_stdout
    ~pp_stderr
    ~js_file
    () =
  let worker = Worker.create js_file in
  let fds =
    Fd.Map.empty |> Fd.Map.add Fd.stdout pp_stdout |> Fd.Map.add Fd.stderr pp_stderr
  in
  let worker =
    { cmis_base_url
    ; imported = []
    ; pending_imports = []
    ; worker
    ; js_file
    ; wakeners = Message_id.Map.empty
    ; counter = Message_id.first
    ; fds
    ; fd_counter = Fd.first_free
    ; lexbuf_counter = Lexbuf.first
    ; reset_worker = do_reset_worker ()
    ; after_init
    ; pp_stdout
    ; pp_stderr
    }
  in
  install_handlers worker;
  (* Surface a failed [Init] (e.g. [stdlib.cmis.js] missing under
     [cmis_base_url], or the worker failing to load — reported via the
     [onerror] handler) by rejecting with [Init_failed] rather than
     handing back a worker whose toplevel env was never initialized. *)
  post worker @@ Init { cmis_base_url }
  >>= function
  | Wrapped_intf.Error (err, _) ->
      (* [create] never returns this worker, so the caller has no handle to
         tear it down: terminate it here so a worker that failed to init does
         not stay alive and idle. [wakeners] is already empty at this point. *)
      terminate worker;
      Lwt.fail (Init_failed err)
  | Wrapped_intf.Success ((), _) ->
      (* As on the [Error] path, tear the worker down if [after_init] fails:
         [create] never returns it, so the caller could not. *)
      Lwt.catch
        (fun () -> worker.after_init worker >>= fun () -> Lwt.return worker)
        (fun exn ->
          terminate worker;
          Lwt.fail exn)

let create_fd worker pp =
  worker.fds <- Fd.Map.add worker.fd_counter pp worker.fds;
  let fd = worker.fd_counter in
  worker.fd_counter <- Fd.next fd;
  fd

let close_fd worker fd = worker.fds <- Fd.Map.remove fd worker.fds

let reset worker ?(timeout = never_ending) () =
  let timeout = timeout () in
  Lwt.choose
    [ (post worker Reset >>= fun res -> Lwt.return (`Reset res))
    ; (timeout >>= fun () -> Lwt.return `Timeout)
    ]
  >>= function
  | `Reset (Wrapped_intf.Success ((), _)) ->
      Lwt.cancel timeout;
      worker.after_init worker
  | `Reset (Wrapped_intf.Error (err, _)) ->
      Lwt.cancel timeout;
      worker.pp_stderr err.Wrapped_intf.msg;
      worker.reset_worker worker
  | `Timeout ->
      (* Not canceling the Reset thread, but manually resetting. *)
      worker.reset_worker worker

(* Forcibly restart the worker. Unlike {!reset}, this does not try the
   cooperative [Reset] message first — a worker stuck in an infinite loop
   cannot process it — it goes straight to terminate + respawn. [reset_worker]
   cancels every in-flight request as part of [terminate], so their threads
   fail with [Lwt.Canceled]. *)
let interrupt worker = worker.reset_worker worker

let check worker ?(setenv = false) code = post worker @@ Check { setenv; code }

let clear_check worker = post worker Clear_check >>= fun _ -> Lwt.return_unit

let execute worker ?ppf_code ?(print_outcome = true) ~ppf_answer code =
  let ppf_code = Option.map (create_fd worker) ppf_code in
  let ppf_answer = create_fd worker ppf_answer in
  post worker
  @@ Execute { code_fd = ppf_code; print_outcome; answer_fd = ppf_answer; code }
  >>= fun result ->
  Option.iter (close_fd worker) ppf_code;
  close_fd worker ppf_answer;
  Lwt.return result

let use worker ?filename ?(print_outcome = false) ~ppf_answer code =
  let ppf_answer = create_fd worker ppf_answer in
  post worker @@ Use_string { filename; print_outcome; answer_fd = ppf_answer; code }
  >>= fun result ->
  close_fd worker ppf_answer;
  Lwt.return result

let open_lexbuf worker ?ppf_code code =
  let code_fd = Option.map (create_fd worker) ppf_code in
  let id = worker.lexbuf_counter in
  worker.lexbuf_counter <- Lexbuf.next id;
  post worker @@ Open_lexbuf { id; code; code_fd }
  >>= function
  | Wrapped_intf.Success ((), _) -> Lwt.return { lb_id = id; lb_code_fd = code_fd }
  | Wrapped_intf.Error (err, _) ->
      (* The session was not created; release the echo fd and surface the error
         rather than handing back a handle to a non-existent session. *)
      Option.iter (close_fd worker) code_fd;
      Lwt.fail (Failure err.Wrapped_intf.msg)

let step worker ?(print_outcome = false) ~ppf_answer lexbuf =
  let answer_fd = create_fd worker ppf_answer in
  post worker @@ Step { lexbuf = lexbuf.lb_id; print_outcome; answer_fd }
  >>= fun result ->
  close_fd worker answer_fd;
  Lwt.return result

let close_lexbuf worker lexbuf =
  post worker @@ Close_lexbuf { lexbuf = lexbuf.lb_id }
  >>= fun (_ : unit Wrapped_intf.result) ->
  Option.iter (close_fd worker) lexbuf.lb_code_fd;
  Lwt.return_unit

let use_mod_string worker ?(print_outcome = true) ~ppf_answer ~modname ?sig_code impl_code
    =
  let ppf_answer = create_fd worker ppf_answer in
  post worker
  @@ Use_mod_string
       { answer_fd = ppf_answer; print_outcome; modname; sig_code; code = impl_code }
  >>= fun result ->
  close_fd worker ppf_answer;
  Lwt.return result

let set_after_init w after_init = w.after_init <- after_init
