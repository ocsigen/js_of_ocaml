(* Js_of_ocaml toplevel
 * http://www.ocsigen.org/js_of_ocaml/
 * Copyright (C) 2016 OCamlPro, Grégoire Henry
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

open Toplevel_ui
open Lwt

type t = {
  top: JsooTopAsynchronous.toplevel ;
  pp_stdout: string -> unit ;
  pp_stderr: string -> unit ;
  pp_code: string -> unit ;
  pp_answer: string -> unit ;
  output: Dom_html.element Js.t ;
}

let execute_phrases t print_outcome content =
  let ppf_code = if print_outcome then Some t.pp_code else None in
  JsooTopAsynchronous.execute t.top
    ~print_outcome ?ppf_code ~ppf_answer:t.pp_answer content >>=
  Toplevel_ui.display_result t.output

let exec t s =
  execute_phrases t false s >>= fun res ->
  if not res then Format.eprintf "error while evaluating %s@." s;
  Lwt.return_unit

let init output =
  let pp_stdout = append Colorize.text  output "stdout" in
  let pp_stderr = append Colorize.text  output "stderr" in
  let pp_code = append Colorize.ocaml output "sharp" in
  let pp_answer = append Colorize.ocaml output "caml" in

  let after_init top =
    JsooTopAsynchronous.import_cmis_js top "js_of_ocaml" >>= fun _ ->
    JsooTopAsynchronous.import_cmis_js top "js_of_ocaml.toplevel" >>= fun _ ->
    JsooTopAsynchronous.import_cmis_js top "js_of_ocaml.deriving" >>= fun _ ->
    JsooTopAsynchronous.import_cmis_js top "js_of_ocaml.tyxml" >>= fun _ ->
    JsooTopAsynchronous.import_cmis_js top "js_of_ocaml.graphics" >>= fun _ ->
    JsooTopAsynchronous.import_cmis_js top "lwt" >>= fun _ ->
    JsooTopAsynchronous.import_cmis_js top "react" >>= fun _ ->
    JsooTopAsynchronous.import_cmis_js top "reactiveData" >>= fun _ ->
    JsooTopAsynchronous.import_cmis_js top "dynlink" >>= fun _ ->
    Toplevel_ui.setup_toplevel
      (exec { top ; pp_stdout ; pp_stderr ;
              pp_code ; pp_answer ; output }) >>= fun () ->
    Lwt.return_unit in
  JsooTopAsynchronous.create
    ~after_init
    ~pp_stdout ~pp_stderr
    ~js_file:"toplevel_worker.js"
    () >>= fun top ->
  Lwt.return { top ; pp_stdout ; pp_stderr ; pp_code ; pp_answer ; output }

let reset_toplevel { top } () =
  JsooTopAsynchronous.reset top ()

let () =
  let run _ =
    Lwt.async (fun () -> Toplevel_ui.run init reset_toplevel execute_phrases) ;
    Js._false in
  Dom_html.window##onload <- Dom_html.handler run
