(* Js_of_ocaml library
 * http://www.ocsigen.org/js_of_ocaml/
 * Copyright (C) 2010 Jérôme Vouillon
 * Laboratoire PPS - CNRS Université Paris Diderot
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

let sleep d =
  let (t, w) = Lwt.task () in
  let id = Dom_html.setTimeout (Lwt.wakeup w) (d *. 1000.) in
  Lwt.on_cancel t (fun () -> Dom_html.clearTimeout id);
  t

let yield () = sleep 0.

let wakeup = function
  | 1 -> ignore (Dom_html.window##setTimeout
		  (Js.wrap_callback Lwt.wakeup_paused , 0.))
  | _ -> ()

let () = Lwt.register_pause_notifier wakeup


let prerr_string s = Firebug.console##log(Js.string s)

let _ =
  Lwt.async_exception_hook := (fun exn ->
    prerr_string "Exception during Lwt.async: ";
    prerr_string (Printexc.to_string exn);
    Printexc.print_backtrace stderr)
