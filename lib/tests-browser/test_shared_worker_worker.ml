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

(* SharedWorker and BroadcastChannel demo (worker side).

   A single instance of this worker serves every tab displaying
   test_shared_worker.html: it numbers connections, echoes messages with a
   shared counter, and announces new connections on the broadcast channel. *)

open Js_of_ocaml

let () =
  let connections = ref 0 in
  let messages = ref 0 in
  let broadcast =
    if BroadcastChannel.is_supported ()
    then Some (new%js BroadcastChannel.broadcastChannel (Js.string "jsoo-demo"))
    else None
  in
  SharedWorker.set_onconnect (fun port ->
      incr connections;
      let id = !connections in
      port##postMessage
        (Js.string (Printf.sprintf "you are connection #%d to this worker" id));
      (match broadcast with
      | Some channel ->
          channel##postMessage
            (Js.string (Printf.sprintf "connection #%d joined the shared worker" id))
      | None -> ());
      port##.onmessage :=
        Dom.handler (fun ev ->
            incr messages;
            let data = Js.to_string (Js.Unsafe.coerce ev##.data) in
            port##postMessage
              (Js.string
                 (Printf.sprintf
                    "connection #%d sent %S (%d messages received from all tabs)"
                    id
                    data
                    !messages));
            Js._true))
