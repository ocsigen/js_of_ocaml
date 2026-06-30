(* Js_of_ocaml toplevel
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

(* Host page driving an OCaml toplevel that runs in a separate Web Worker.

   The heavy lifting (parsing, type-checking, byte-compilation) happens in
   [worker.js]; this page only sends source phrases over and renders the
   results, so the UI thread never blocks. See [worker.ml] for the worker
   side and {!Js_of_ocaml_toplevel_lwt.Async} for the message protocol. *)

open Js_of_ocaml
module Async = Js_of_ocaml_toplevel_lwt.Async
(* The host only needs the result type, from the dependency-free msg library —
   not the toplevel runtime. *)
module Wrapped = Js_of_ocaml_toplevel_msg.Wrapped_intf

let ( >>= ) = Lwt.bind

let () =
  let onload _ =
    let output = Dom_html.getElementById "output" in
    let append s =
      Dom.appendChild output (Dom_html.document##createTextNode (Js.string s))
    in
    let run () =
      append "Spawning the toplevel Web Worker...\n";
      (* [stdlib.cmis.js] is fetched by the worker (see [Async.create]'s
         [cmis_base_url]) so the toplevel can type-check against the stdlib. *)
      Async.create ~pp_stdout:append ~pp_stderr:append ~js_file:"worker.js" ()
      >>= fun top ->
      append "Worker ready.\n\n";
      let phrase code =
        append (Printf.sprintf "# %s\n" code);
        Async.execute top ~print_outcome:true ~ppf_answer:append code
        >>= fun result ->
        (match result with
        | Wrapped.Success _ -> ()
        | Wrapped.Error (err, _) -> append (Printf.sprintf "Error: %s\n" err.Wrapped.msg));
        append "\n";
        Lwt.return_unit
      in
      (* [Async.execute] normalizes the source, so a trailing [;;] is optional. *)
      phrase {|print_string "Hello from the worker toplevel!\n"|}
      >>= fun () ->
      phrase "let x = 6 * 7"
      >>= fun () ->
      phrase "List.map (fun n -> n * n) [ 1; 2; 3; 4 ]"
      >>= fun () -> phrase "let oops = undefined_value"
    in
    Lwt.async run;
    Js._false
  in
  Dom_html.window##.onload := Dom_html.handler onload
