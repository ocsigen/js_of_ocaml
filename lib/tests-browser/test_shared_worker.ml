(* Js_of_ocaml tests
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
 *
 * You should have received a copy of the GNU Lesser General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
 *)

(* SharedWorker and BroadcastChannel demo (page side).

   Open this page in several tabs: all tabs talk to the same shared worker
   (see test_shared_worker_worker.ml), and messages broadcast on the
   "jsoo-demo" channel are delivered to every other tab. *)

open Js_of_ocaml

let log msg =
  let el = Dom_html.document##getElementById (Js.string "log") in
  Js.Opt.iter el (fun el ->
      let line = Dom_html.document##createElement (Js.string "div") in
      line##.textContent := Js.some (Js.string msg);
      Dom.appendChild el line)

let on_click id f =
  Js.Opt.iter
    (Dom_html.document##getElementById (Js.string id))
    (fun el ->
      el##.onclick :=
        Dom_html.handler (fun _ ->
            f ();
            Js._false))

let is_wasm () =
  let search = Js.to_string Dom_html.window##.location##.search in
  match Regexp.search (Regexp.regexp "[?&]wasm\\b") search 0 with
  | Some _ -> true
  | None -> false

let message_data ev = Js.to_string (Js.Unsafe.coerce ev##.data)

let setup_shared_worker () =
  if not (SharedWorker.is_supported ())
  then log "SharedWorker is not supported in this browser"
  else begin
    let url =
      if is_wasm ()
      then "test_shared_worker_worker.bc.wasm.js"
      else "test_shared_worker_worker.bc.js"
    in
    let worker =
      new%js SharedWorker.sharedWorker_withName (Js.string url) (Js.string "jsoo-demo")
    in
    worker##.onerror :=
      Dom.handler (fun _ ->
          log "shared worker: error event (see the browser console)";
          Js._true);
    worker##.port##.onmessage
    := Dom.handler (fun ev ->
        log ("shared worker: " ^ message_data ev);
        Js._true);
    let count = ref 0 in
    on_click "send-worker" (fun () ->
        incr count;
        worker##.port##postMessage (Js.string (Printf.sprintf "ping %d" !count)))
  end

let setup_broadcast_channel () =
  if not (BroadcastChannel.is_supported ())
  then log "BroadcastChannel is not supported in this browser"
  else begin
    let channel = new%js BroadcastChannel.broadcastChannel (Js.string "jsoo-demo") in
    channel##.onmessage :=
      Dom.handler (fun ev ->
          log ("broadcast: " ^ message_data ev);
          Js._true);
    let count = ref 0 in
    on_click "send-broadcast" (fun () ->
        incr count;
        (* Not delivered to this tab: a channel does not receive its own
           messages *)
        channel##postMessage
          (Js.string (Printf.sprintf "hello from another tab (%d)" !count)))
  end

let () =
  setup_shared_worker ();
  setup_broadcast_channel ()
