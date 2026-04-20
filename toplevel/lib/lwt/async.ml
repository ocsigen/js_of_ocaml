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
open Worker_msg

type 'a result = 'a Wrapped.result Lwt.t

let ( >>= ) = Lwt.bind

let ( >>? ) o f =
  let open! Wrapped in
  o
  >>= function
  | Error (err, w) -> Lwt.return (Error (err, w))
  | Success (x, w) -> (
      f x
      >>= function
      | Error (err, w') -> Lwt.return (Error (err, w @ w'))
      | Success (x, w') -> Lwt.return (Success (x, w @ w')))

let return_success e = Lwt.return (Wrapped.Success (e, []))

let return_unit_success = return_success ()

module IntMap = Map.Make (Int)

type u =
  | U : 'a msg_ty * 'a Wrapped.result Lwt.u * 'a Wrapped.result Lwt.t -> u

type output = string -> unit

type toplevel =
  { cmis_prefix : string
  ; js_file : string
  ; mutable imported : string list
  ; mutable pending_imports : (string * unit Wrapped.result Lwt.t) list
  ; mutable worker : (Js.js_string Js.t, Js.js_string Js.t) Worker.worker Js.t
  ; mutable wakeners : u IntMap.t
  ; mutable counter : int
  ; mutable fds : output IntMap.t
  ; mutable fd_counter : int
  ; mutable reset_worker : toplevel -> unit Lwt.t
  ; mutable after_init : toplevel -> unit Lwt.t
  ; pp_stdout : output [@ocaml.warning "-69"] (* FIXME *)
  ; pp_stderr : output
  }

exception Not_equal

let check_equal : type t1 t2. t1 msg_ty -> t2 msg_ty -> (t1, t2) eq =
 fun ty1 ty2 ->
  match ty1, ty2 with
  | Unit, Unit -> Eq
  | Bool, Bool -> Eq
  | Int, Int -> Eq
  | String, String -> Eq
  | Unit, _ -> raise Not_equal
  | Bool, _ -> raise Not_equal
  | Int, _ -> raise Not_equal
  | String, _ -> raise Not_equal

let onmessage worker (ev : _ Worker.messageEvent Js.t) =
  match Json.unsafe_input ev##.data with
  | Write (fd, s) -> (
      try
        IntMap.find fd worker.fds s;
        Js._false
      with Not_found ->
        Console.console##warn (Js.string (Printf.sprintf "Missing channels (%d)" fd));
        Js._false)
  | ReturnSuccess (id, ty_v, v, w) -> (
      match IntMap.find_opt id worker.wakeners with
      | None ->
          Console.console##warn (Js.string (Printf.sprintf "Missing wakeners (%d)" id));
          Js._false
      | Some (U (ty_u, u, _)) ->
          worker.wakeners <- IntMap.remove id worker.wakeners;
          (match check_equal ty_u ty_v with
          | Eq -> Lwt.wakeup u (Wrapped.Success (v, w))
          | exception Not_equal ->
              Console.console##warn
                (Js.string (Printf.sprintf "Unexpected wakeners (%d)" id));
              let err =
                { Wrapped.msg =
                    Printf.sprintf "Worker returned a value of unexpected type for request %d" id
                ; locs = []
                }
              in
              Lwt.wakeup u (Wrapped.Error (err, w)));
          Js._false)
  | ReturnError (id, e, w) -> (
      try
        let (U (_, u, _)) = IntMap.find id worker.wakeners in
        worker.wakeners <- IntMap.remove id worker.wakeners;
        Lwt.wakeup u (Wrapped.Error (e, w));
        Js._false
      with Not_found ->
        Console.console##warn (Js.string (Printf.sprintf "Missing wakeners (%d)" id));
        Js._false)

let terminate worker =
  worker.worker##terminate;
  IntMap.iter
    (fun id (U (_, _, t)) ->
      worker.wakeners <- IntMap.remove id worker.wakeners;
      Lwt.cancel t)
    worker.wakeners

let never_ending =
  (* and not cancellable. *)
  fst (Lwt.wait ())

(** Threads created with [post] will always be wake-uped by
    [onmessage] by calling [Lwt.wakeup]. They should never end with
    an exception, unless canceled. When canceled, the worker is
    killed and a new one is spawned. *)
let rec post : type a. toplevel -> a host_msg -> a Wrapped.result Lwt.t =
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
  worker.wakeners <- IntMap.add msg_id (U (msg_ty, u, t)) worker.wakeners;
  worker.counter <- msg_id + 1;
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
        IntMap.empty
        |> IntMap.add 0 (IntMap.find 0 worker.fds)
        |> IntMap.add 1 (IntMap.find 1 worker.fds);
      worker.fd_counter <- 2;
      let imported = worker.imported in
      worker.imported <- [];
      worker.pending_imports <- [];
      worker.wakeners <- IntMap.empty;
      worker.counter <- 0;
      worker.reset_worker <- do_reset_worker ();
      worker.worker##.onmessage := Dom.handler (onmessage worker);
      Lwt_list.iter_p
        (fun name -> import_cmis_js worker name >>= fun _ -> Lwt.return_unit)
        imported
      >>= fun () ->
      post worker @@ Init worker.cmis_prefix
      >>= fun _ -> worker.after_init worker >>= fun _ -> Lwt.return_unit)
    else Lwt.return_unit

and import_cmis_js worker name =
  if List.mem name worker.imported
  then return_unit_success
  else
    match List.assoc_opt name worker.pending_imports with
    | Some t -> t
    | None ->
        let url = worker.cmis_prefix ^ name ^ ".cmis.js" in
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
        t

let create
    ?(cmis_prefix = "")
    ?(after_init = fun _ -> Lwt.return_unit)
    ~pp_stdout
    ~pp_stderr
    ~js_file
    () =
  let worker = Worker.create js_file in
  let fds = IntMap.empty |> IntMap.add 0 pp_stdout |> IntMap.add 1 pp_stderr in
  let worker =
    { cmis_prefix
    ; imported = []
    ; pending_imports = []
    ; worker
    ; js_file
    ; wakeners = IntMap.empty
    ; counter = 0
    ; fds
    ; fd_counter = 2
    ; reset_worker = do_reset_worker ()
    ; after_init
    ; pp_stdout
    ; pp_stderr
    }
  in
  worker.worker##.onmessage := Dom.handler (onmessage worker);
  post worker @@ Init cmis_prefix
  >>= fun _ -> worker.after_init worker >>= fun () -> Lwt.return worker

let create_fd worker pp =
  worker.fds <- IntMap.add worker.fd_counter pp worker.fds;
  let fd = worker.fd_counter in
  worker.fd_counter <- fd + 1;
  fd

let close_fd worker fd = worker.fds <- IntMap.remove fd worker.fds

let reset worker ?(timeout = fun () -> never_ending) () =
  let timeout = timeout () in
  Lwt.choose
    [ (post worker Reset >>= fun res -> Lwt.return (`Reset res))
    ; (timeout >>= fun () -> Lwt.return `Timeout)
    ]
  >>= function
  | `Reset (Wrapped.Success ((), _)) ->
      Lwt.cancel timeout;
      worker.after_init worker
  | `Reset (Wrapped.Error (err, _)) ->
      Lwt.cancel timeout;
      worker.pp_stderr err.Wrapped.msg;
      worker.reset_worker worker
  | `Timeout ->
      (* Not canceling the Reset thread, but manually resetting. *)
      worker.reset_worker worker

let check worker ?(setenv = false) code = post worker @@ Check (setenv, code)

let execute worker ?ppf_code ?(print_outcome = true) ~ppf_answer code =
  let ppf_code = Option.map (create_fd worker) ppf_code in
  let ppf_answer = create_fd worker ppf_answer in
  post worker @@ Execute (ppf_code, print_outcome, ppf_answer, code)
  >>= fun result ->
  Option.iter (close_fd worker) ppf_code;
  close_fd worker ppf_answer;
  Lwt.return result

let use_string worker ?filename ?(print_outcome = true) ~ppf_answer code =
  let ppf_answer = create_fd worker ppf_answer in
  post worker @@ Use_string (filename, print_outcome, ppf_answer, code)
  >>= fun result ->
  close_fd worker ppf_answer;
  Lwt.return result

let use_mod_string worker ?(print_outcome = true) ~ppf_answer ~modname ?sig_code impl_code
    =
  let ppf_answer = create_fd worker ppf_answer in
  post worker @@ Use_mod_string (ppf_answer, print_outcome, modname, sig_code, impl_code)
  >>= fun result ->
  close_fd worker ppf_answer;
  Lwt.return result

let set_after_init w after_init = w.after_init <- after_init
