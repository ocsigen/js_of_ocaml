(* Js_of_ocaml toplevel
 * http://www.ocsigen.org/js_of_ocaml/
 * Copyright (C) 2011 Jérôme Vouillon
 * Laboratoire PPS - CNRS Université Paris Diderot
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

type t = {
  ppf_code: Format.formatter ;
  ppf_answer: Format.formatter ;
  output: Dom_html.element Js.t ;
}

let highlight_location cl loc =
  let _file1,line1,col1 = Location.get_pos_info (loc.Location.loc_start) in
  let _file2,line2,col2 = Location.get_pos_info (loc.Location.loc_end) in
  Toplevel_ui.highlight_location cl {
    JsooTop.Wrapped.loc_start = (line1,col1);
    loc_end = (line2,col2);
  }

let execute_phrases t print_outcome content =
  let ppf_code = if print_outcome then Some t.ppf_code else None in
  let res =
    JsooTop.Wrapped.execute ()
      ~print_outcome ?ppf_code ~ppf_answer:t.ppf_answer content in
  Toplevel_ui.display_result t.output res

let exec toplevel s =
  let open Lwt in
  execute_phrases toplevel false s >>= fun res ->
  if not res then Format.eprintf "error while evaluating %s@." s;
  Lwt.return_unit

let init output =

#ifdef graphics
  Graphics_js.open_canvas (by_id_coerce "test-canvas" Dom_html.CoerceTo.canvas);
#endif

  let sharp_chan = open_out "/dev/null0" in
  let sharp_ppf = Format.formatter_of_out_channel sharp_chan in

  let caml_chan = open_out "/dev/null1" in
  let caml_ppf = Format.formatter_of_out_channel caml_chan in

  Sys_js.set_channel_flusher caml_chan  (append Colorize.ocaml output "caml");
  Sys_js.set_channel_flusher sharp_chan (append Colorize.ocaml output "sharp");
  Sys_js.set_channel_flusher stdout     (append Colorize.text  output "stdout");
  Sys_js.set_channel_flusher stderr     (append Colorize.text  output "stderr");

  let readline () =
    Js.Opt.case
      (Dom_html.window##prompt
         (Js.string "The toplevel expects inputs:", Js.string ""))
      (fun () -> "")
      (fun s -> Js.to_string s ^ "\n") in
  Sys_js.set_channel_filler stdin readline;

  JsooTop.initialize ();
  let open Lwt in
  let toplevel = { output ; ppf_answer = caml_ppf; ppf_code = sharp_ppf } in
  Toplevel_ui.setup_toplevel (exec toplevel) >>= fun () ->
  Lwt.return toplevel

let reset_toplevel top () =
  JsooTop.initialize ();
  Toplevel_ui.setup_toplevel (exec top)

let () =
  let run _ =
    Lwt.async (fun () ->
      Toplevel_ui.run init reset_toplevel execute_phrases) ;
    Js._false in
  Dom_html.window##onload <- Dom_html.handler run
