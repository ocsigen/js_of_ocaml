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

open Js_of_ocaml_compiler
open Js_of_ocaml_compiler.Stdlib

let setup =
  lazy
    (Topdirs.dir_directory "/static/cmis";
     Toploop.add_directive
       "enable"
       (Toploop.Directive_string Config.Flag.enable)
       { section = "js_of_ocaml"; doc = "Enable the given flag" };
     Toploop.add_directive
       "disable"
       (Toploop.Directive_string Config.Flag.disable)
       { section = "js_of_ocaml"; doc = "Disable the given flag" };
     Toploop.add_directive
       "debug_on"
       (Toploop.Directive_string Debug.enable)
       { section = "js_of_ocaml"; doc = "Enable debug for the given section" };
     Toploop.add_directive
       "debug_off"
       (Toploop.Directive_string Debug.disable)
       { section = "js_of_ocaml"; doc = "Disable debug for the given section" };
     Toploop.add_directive
       "tailcall"
       (Toploop.Directive_string (Config.Param.set "tc"))
       { section = "js_of_ocaml"
       ; doc = "Set the depth of tail calls before going through a trampoline"
       })

let init_loc lb filename =
  Location.init lb filename;
  (* [Location.init] points [input_lexbuf] at [lb]; clear it so rendered errors
     do not inline a source excerpt (locations are reported on their own). *)
  Location.input_lexbuf := None

(* Reimplements [Toploop.use_silently] so that the toplevel's in-process ppx
   rewriters are applied (via [Ppx.preprocess_phrase]); [use_silently] only
   knows about external [-ppx] executables, which cannot run in the browser. *)
let use ?(print_outcome = false) ffp content =
  let lb = Lexing.from_string content in
  init_loc lb "//toplevel//";
  try
    List.iter
      ~f:(fun phr ->
        if not (Toploop.execute_phrase print_outcome ffp phr)
        then raise Exit
        else Format.pp_print_flush ffp ())
      (List.map ~f:Ppx.preprocess_phrase (!Toploop.parse_use_file lb));
    flush_all ();
    true
  with
  | Exit ->
      flush_all ();
      false
  | x ->
      flush_all ();
      Errors.report_error ffp x;
      false

let execute printval ?pp_code ?highlight_location pp_answer s =
  let lb = Lexing.from_function (Refill.lexbuf s (ref 0) pp_code) in
  init_loc lb "//toplevel//";
  (try
     while true do
       try
         let phr = !Toploop.parse_toplevel_phrase lb in
         let phr = Ppx.preprocess_phrase phr in
         ignore (Toploop.execute_phrase printval pp_answer phr : bool)
       with
       | End_of_file -> raise End_of_file
       | x ->
           (match highlight_location with
           | None -> ()
           | Some f -> (
               match Error_loc.loc x with
               | None -> ()
               | Some loc -> f loc));
           Errors.report_error Format.err_formatter x
     done
   with End_of_file -> ());
  flush_all ()

let initialized = ref false

let initialize () =
  if not !initialized
  then (
    initialized := true;
    Sys.interactive := false;
    Lazy.force setup;
    Toploop.initialize_toplevel_env ();
    Toploop.input_name := "//toplevel//";
    Sys.interactive := true)
