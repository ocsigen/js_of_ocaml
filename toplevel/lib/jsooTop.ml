(* Js_of_ocaml library
 * http://www.ocsigen.org/js_of_ocaml/
 * Copyright (C) 2014 Hugo Heuzard
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
open Js_of_ocaml_compiler
open Js_of_ocaml_compiler.Stdlib

let new_directive name k = Hashtbl.add Toploop.directive_table name k
  [@@alert "-deprecated"]

let setup =
  lazy
    (Topdirs.dir_directory "/static/cmis";
     new_directive "enable" (Toploop.Directive_string Config.Flag.enable);
     new_directive "disable" (Toploop.Directive_string Config.Flag.disable);
     new_directive "debug_on" (Toploop.Directive_string Debug.enable);
     new_directive "debug_off" (Toploop.Directive_string Debug.disable);
     new_directive "tailcall" (Toploop.Directive_string (Config.Param.set "tc")))

let refill_lexbuf s p ppf buffer len =
  if !p = String.length s
  then 0
  else
    let len', nl =
      try String.index_from s !p '\n' - !p + 1, false
      with _ -> String.length s - !p, true
    in
    let len'' = min len len' in
    String.blit ~src:s ~src_pos:!p ~dst:buffer ~dst_pos:0 ~len:len'';
    (match ppf with
    | Some ppf ->
        Format.fprintf ppf "%s" (Bytes.sub_string buffer ~pos:0 ~len:len'');
        if nl then Format.pp_print_newline ppf ();
        Format.pp_print_flush ppf ()
    | None -> ());
    p := !p + len'';
    len''

let toploop_use_silently ffp name = Toploop.use_silently ffp name
  [@@ocaml.warning "-32"] [@@if ocaml_version < (4, 14, 0)]

let toploop_use_silently ffp name = Toploop.use_silently ffp (File name)
  [@@ocaml.warning "-32"] [@@if ocaml_version >= (4, 14, 0)]

let use ffp content =
  let name = "/dev/fake_stdin" in
  if Sys.file_exists name then Sys.remove name;
  Sys_js.create_file ~name ~content;
  toploop_use_silently ffp name

let execute printval ?pp_code ?highlight_location pp_answer s =
  let lb = Lexing.from_function (refill_lexbuf s (ref 0) pp_code) in
  (try
     while true do
       try
         let phr = !Toploop.parse_toplevel_phrase lb in
         let phr = JsooTopPpx.preprocess_phrase phr in
         ignore (Toploop.execute_phrase printval pp_answer phr : bool)
       with
       | End_of_file -> raise End_of_file
       | x ->
           (match highlight_location with
           | None -> ()
           | Some f -> (
               match JsooTopError.loc x with
               | None -> ()
               | Some loc -> f loc));
           Errors.report_error Format.err_formatter x
     done
   with End_of_file -> ());
  flush_all ()

let initialize () =
  Sys.interactive := false;
  Lazy.force setup;
  Toploop.initialize_toplevel_env ();
  Toploop.input_name := "//toplevel//";
  Sys.interactive := true
