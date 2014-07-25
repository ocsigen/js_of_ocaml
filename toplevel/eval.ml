(* Js_of_ocaml toplevel
 * http://www.ocsigen.org/js_of_ocaml/
 * Copyright (C) 2011 Jérôme Vouillon
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

open Compiler

module Top : sig
  val exec : string -> unit
  val initialize : unit -> unit
end = struct
  let split_primitives p =
    let len = String.length p in
    let rec split beg cur =
      if cur >= len then []
      else if p.[cur] = '\000' then
        String.sub p beg (cur - beg) :: split (cur + 1) (cur + 1)
      else
        split beg (cur + 1) in
    Array.of_list(split 0 0)

  let initialize () =
    Topdirs.dir_directory "/cmis";
    let initial_primitive_count =
      Array.length (split_primitives (Symtable.data_primitive_names ())) in

    let compile s =
      let prims =
        split_primitives (Symtable.data_primitive_names ()) in
      let unbound_primitive p =
        try ignore (Js.Unsafe.eval_string p); false with _ -> true in
      let stubs = ref [] in
      Array.iteri
        (fun i p ->
           if i >= initial_primitive_count && unbound_primitive p then
             stubs :=
               Format.sprintf
                 "function %s(){caml_failwith(\"%s not implemented\")}" p p
               :: !stubs)
        prims;
      let output_program = Driver.from_string prims s in
      let b = Buffer.create 100 in
      output_program (Pretty_print.to_buffer b);
      let res = Buffer.contents b in
      let res = String.concat "" !stubs ^ res in
      res
    in
    Js.Unsafe.global##toplevelCompile <- compile; (*XXX HACK!*)
    Toploop.initialize_toplevel_env ();
    Toploop.input_name := "//toplevel//"

  let exec s =
    let lb = Lexing.from_string s in
    try
      List.iter
        (fun phr ->
           if not (Toploop.execute_phrase true Format.std_formatter phr) then raise Exit)
        (!Toploop.parse_use_file lb)
    with
    | Exit -> ()
    | x    -> Errors.report_error Format.err_formatter x

end
let _ = Firebug.console##warn(Js.string "one")

let append_string output cl s =
  let d = Dom_html.window##document in
  let span = Dom_html.createDiv d in
  span##classList##add(Js.string cl);
  Dom.appendChild span (d##createTextNode (Js.string s));
  Dom.appendChild output span

let configure o chan attr default =
  try
    let v = o##getAttribute(Js.string attr) in
    match Js.Opt.to_option v with
    | None -> raise Not_found
    | Some id ->
      let dom = Dom_html.getElementById (Js.to_string id) in
      Sys_js.set_channel_flusher chan (append_string dom attr)
  with Not_found -> Sys_js.set_channel_flusher chan default


let _ = Lwt.bind (Lwt_js_events.domContentLoaded ()) (fun () ->
    let _ = Firebug.console##warn(Js.string "two") in
    let toploop_ = open_out "/dev/null" in
    let toploop_ppf = Format.formatter_of_out_channel toploop_ in
    let print = !Toploop.print_out_phrase in
    Toploop.print_out_phrase:= (fun fmt outcome -> print toploop_ppf outcome);
    Lwt.async_exception_hook:= (fun exc -> Format.eprintf "exc during Lwt.async: %s@." (Printexc.to_string exc));
    Top.initialize ();
    let scripts = Dom_html.window##document##getElementsByTagName(Js.string "script") in
    let default_stdout = Format.printf  "%s@." in
    let default_stderr = Format.eprintf "%s@." in
    let default_toploop x = () in
    for i = 0 to scripts##length - 1 do
      let item_opt = scripts##item(i) in
      let elt_opt = Js.Opt.bind item_opt Dom_html.CoerceTo.element in
      match Dom_html.opt_tagged elt_opt with
      | Some (Dom_html.Script script) ->
        if script##_type = Js.string "text/ocaml"
        then begin
          let txt = Js.to_string script##text in
          configure script stdout "stdout" default_stdout;
          configure script stderr "stderr" default_stderr;
          configure script toploop_ "toploop" default_toploop;
          Top.exec txt
        end
        else ()
      | _ -> ()
    done;
    Lwt.return_unit)
