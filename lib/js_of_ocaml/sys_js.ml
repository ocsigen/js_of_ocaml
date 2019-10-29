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
open! Import

external create_file : name:string -> content:string -> unit = "caml_create_file"

external read_file : name:string -> string = "caml_read_file_content"

let update_file ~name ~content =
  let oc = open_out name in
  output_string oc content;
  close_out oc

external set_channel_output' :
  out_channel -> (Js.js_string Js.t -> unit) Js.callback -> unit
  = "caml_ml_set_channel_output"

external set_channel_input' : in_channel -> (unit -> string) Js.callback -> unit
  = "caml_ml_set_channel_refill"

let set_channel_flusher (out_channel : out_channel) (f : string -> unit) =
  let f' : (Js.js_string Js.t -> unit) Js.callback =
    Js.wrap_callback (fun s -> f (Js.to_bytestring s))
  in
  set_channel_output' out_channel f'

let set_channel_filler (in_channel : in_channel) (f : unit -> string) =
  let f' : (unit -> string) Js.callback = Js.wrap_callback f in
  set_channel_input' in_channel f'

external mount_point : unit -> string list = "caml_list_mount_point"

external mount_autoload :
  string -> (string -> string -> string option) Js.callback -> unit
  = "caml_mount_autoload"

external unmount : string -> unit = "caml_unmount"

let mount ~path f =
  mount_autoload path (Js.wrap_callback (fun prefix path -> f ~prefix ~path))

let unmount ~path = unmount path

let js_of_ocaml_version =
  if String.equal Lib_version.git_version ""
  then Lib_version.s
  else Lib_version.s ^ "+" ^ Lib_version.git_version
