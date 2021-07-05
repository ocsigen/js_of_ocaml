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

type 'a result = 'a JsooTopWrapped.result Lwt.t

let ( >>= ) = Lwt.bind

let ( >>? ) o f =
  let open! JsooTopWrapped in
  o
  >>= function
  | Error (err, w) -> Lwt.return (Error (err, w))
  | Success (x, w) -> (
      f x
      >>= function
      | Error (err, w') -> Lwt.return (Error (err, w @ w'))
      | Success (x, w') -> Lwt.return (Success (x, w @ w')))

let return_success e = Lwt.return (JsooTopWrapped.Success (e, []))

let return_unit_success = return_success ()

(* let return_error e = Lwt.return (JsooTopWrapped.Error (e, [])) *)
(* let return_exn exn = return_error (JsooTopWrapped.error_of_exn exn) *)

(* let wrap pp = *)
(* let buf = Buffer.create 503 in *)
(* let flush () = *)
(* let s = Buffer.contents buf in *)
(* if s <> "" then begin *)
(* Buffer.reset buf; *)
(* pp s *)
(* end in *)
(* Format.make_formatter (Buffer.add_substring buf) flush *)

(* let () = *)
(* Location.register_error_of_exn *)
(* (function *)
(* | Js.Error e -> *)
(* Firebug.console##log(e##stack); *)
(* let msg = Js.to_string e##message in *)
(* Some { Location.msg; if_highlight = msg; sub = []; loc = Location.none } *)
(* | _ -> None) *)

module IntMap = Map.Make (Int)

let map_option f o =
  match o with
  | None -> None
  | Some o -> Some (f o)

let iter_option f o =
  match o with
  | None -> ()
  | Some o -> f o

type u =
  | U : 'a msg_ty * 'a JsooTopWrapped.result Lwt.u * 'a JsooTopWrapped.result Lwt.t -> u

type output = string -> unit

type toplevel =
  { cmis_prefix : string
  ; js_file : string
  ; mutable imported : string list
  ; mutable worker : (Js.js_string Js.t, Js.js_string Js.t) Worker.worker Js.t
  ; mutable wakeners : u IntMap.t
  ; mutable counter : int
  ; mutable fds : output IntMap.t
  ; mutable fd_counter : int
  ; mutable reset_worker : toplevel -> unit Lwt.t
  ; mutable after_init : toplevel -> unit Lwt.t
  ; pp_stdout : output
  ; pp_stderr : output }

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
      Firebug.console##warn (Js.string (Printf.sprintf "Missing channels (%d)" fd));
      Js._false)
  | ReturnSuccess (id, ty_v, v, w) -> (
    try
      let (U (ty_u, u, _)) = IntMap.find id worker.wakeners in
      let Eq = check_equal ty_u ty_v in
      worker.wakeners <- IntMap.remove id worker.wakeners;
      Lwt.wakeup u (JsooTopWrapped.Success (v, w));
      Js._false
    with
    | Not_found ->
        Firebug.console##warn (Js.string (Printf.sprintf "Missing wakeners (%d)" id));
        Js._false
    | Not_equal ->
        Firebug.console##warn (Js.string (Printf.sprintf "Unexpected wakeners (%d)" id));
        Js._false)
  | ReturnError (id, e, w) -> (
    try
      let (U (_, u, _)) = IntMap.find id worker.wakeners in
      worker.wakeners <- IntMap.remove id worker.wakeners;
      Lwt.wakeup u (JsooTopWrapped.Error (e, w));
      Js._false
    with Not_found ->
      Firebug.console##warn (Js.string (Printf.sprintf "Missing wakeners (%d)" id));
      Js._false)

let terminate worker =
  (worker.worker)##terminate;
  IntMap.iter
    (fun id (U (_, _, t)) ->
      worker.wakeners <- IntMap.remove id worker.wakeners;
      Lwt.cancel t)
    worker.wakeners

let never_ending =
  (* and not cancellable. *)
  fst (Lwt.wait ())

let ty_of_host_msg : type t. t host_msg -> t msg_ty = function
  | Init _ -> Unit
  | Reset -> Unit
  | Check _ -> Unit
  | Execute _ -> Bool
  | Use_string _ -> Bool
  | Use_mod_string _ -> Bool
  | Import_scripts _ -> Unit

(** Threads created with [post] will always be wake-uped by
    [onmessage] by calling [Lwt.wakeup]. They should never end with
    an exception, unless canceled. When canceled, the worker is
    killed and a new one is spawned. *)
let rec post : type a. toplevel -> a host_msg -> a JsooTopWrapped.result Lwt.t =
 fun worker msg ->
  let msg_id = worker.counter in
  let msg_ty = ty_of_host_msg msg in
  let t, u = Lwt.task () in
  Lwt.on_cancel t (fun () -> Lwt.async (fun () -> worker.reset_worker worker));
  worker.wakeners <- IntMap.add msg_id (U (msg_ty, u, t)) worker.wakeners;
  worker.counter <- msg_id + 1;
  (worker.worker)##postMessage (Json.output (msg_id, msg));
  t

and do_reset_worker () =
  let running = ref true in
  fun worker ->
    if !running
    then (
      running := false;
      terminate worker;
      IntMap.iter
        (* GRGR: Peut-on 'cancel' directement le Lwt.u ? *)
          (fun _ (U (_, _, t)) -> Lwt.cancel t)
        worker.wakeners;
      worker.worker <- Worker.create worker.js_file;
      worker.fds <-
        IntMap.empty
        |> IntMap.add 0 (IntMap.find 0 worker.fds)
        |> IntMap.add 1 (IntMap.find 1 worker.fds);
      worker.fd_counter <- 2;
      let imported = worker.imported in
      worker.imported <- [];
      worker.wakeners <- IntMap.empty;
      worker.counter <- 0;
      worker.reset_worker <- do_reset_worker ();
      (Obj.magic worker.worker)##.onmessage := Js.wrap_callback (onmessage worker);
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
    let url = worker.cmis_prefix ^ name ^ ".cmis.js" in
    post worker @@ Import_scripts [url]
    >>? fun () ->
    worker.imported <- name :: worker.imported;
    return_unit_success

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
    ; worker
    ; js_file
    ; wakeners = IntMap.empty
    ; counter = 0
    ; fds
    ; fd_counter = 2
    ; reset_worker = do_reset_worker ()
    ; after_init
    ; pp_stdout
    ; pp_stderr }
  in
  (Obj.magic worker.worker)##.onmessage := Js.wrap_callback (onmessage worker);
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
    ; (timeout >>= fun () -> Lwt.return `Timeout) ]
  >>= function
  | `Reset (JsooTopWrapped.Success ((), _)) ->
      Lwt.cancel timeout;
      worker.after_init worker
  | `Reset (JsooTopWrapped.Error (err, _)) ->
      Lwt.cancel timeout;
      worker.pp_stderr err.JsooTopWrapped.msg;
      worker.reset_worker worker
  | `Timeout ->
      (* Not canceling the Reset thread, but manually resetting. *)
      worker.reset_worker worker

let check worker ?(setenv = false) code = post worker @@ Check (setenv, code)

let execute worker ?ppf_code ?(print_outcome = false) ~ppf_answer code =
  let ppf_code = map_option (create_fd worker) ppf_code in
  let ppf_answer = create_fd worker ppf_answer in
  post worker @@ Execute (ppf_code, print_outcome, ppf_answer, code)
  >>= fun result ->
  iter_option (close_fd worker) ppf_code;
  close_fd worker ppf_answer;
  Lwt.return result

let use_string worker ?filename ?(print_outcome = false) ~ppf_answer code =
  let ppf_answer = create_fd worker ppf_answer in
  post worker @@ Use_string (filename, print_outcome, ppf_answer, code)
  >>= fun result ->
  close_fd worker ppf_answer;
  Lwt.return result

let use_mod_string
    worker
    ?(print_outcome = false)
    ~ppf_answer
    ~modname
    ?sig_code
    impl_code =
  let ppf_answer = create_fd worker ppf_answer in
  post worker @@ Use_mod_string (ppf_answer, print_outcome, modname, sig_code, impl_code)
  >>= fun result ->
  close_fd worker ppf_answer;
  Lwt.return result

let set_after_init w after_init = w.after_init <- after_init
