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

(** Javascript specific Sys functions. *)

(** {2 Io.} *)

val set_channel_flusher : out_channel -> (string -> unit) -> unit
(** Set a callback to be called when an out_channel flush its buffer.
      [set_channel_flusher chan cb] install the callback [cb] for [chan] out_channel.
      [cb] will be called with the string to flush. *)

val set_channel_filler : in_channel -> (unit -> string) -> unit
(** Set a callback to be called when an in_channel wants to fill its
      buffer. [set_channel_filler chan cb] install the called [cb] for
      [chan] in_channel. The string returned by [cb] will be appended
      to the channel buffer. *)

(** {2 Pseudo filesystem.} *)

val mount_point : unit -> string list

val unmount : path:string -> unit

val mount : path:string -> (prefix:string -> path:string -> string option) -> unit
(** Register a callback to the [path] to dynamically load missing files.
      Whenever a file is missing in [path], the callback is used to optionally
      get the content of the file.
      [mount ~path f] register the callback [f] to the path [path].
      The callback [f] receives [(prefix,suffix)] where:
       - [prefix] is the path the function has been registered to.
       - [Filename.contact prefix suffix] is the absolute filename .*)

val read_file : name:string -> string
(** [read_file name] returns the content of the file [name].
      Raise [Sys_error] if the file does not exist. *)

val create_file : name:string -> content:string -> unit
(** Register a file to a Pseudo Filesystem.
      [create_file ~name ~content] register the file [name] with content [content]
      so it can be opened with [open_in name] *)

val update_file : name:string -> content:string -> unit
(** Update a file in the Pseudo Filesystem.
      [update_file ~name ~content] update the file [name] with content [content] *)

(** {2 Information.} *)

val js_of_ocaml_version : string
(** [js_of_ocaml_version] is the version of Js_of_ocaml.
      It is a string of the form ["major.minor[.patchlevel][+additional-info]"],
      where [major], [minor], and [patchlevel] are integers, and
      [additional-info] is an arbitrary string. The [[.patchlevel]] and
      [[+additional-info]] parts may be absent. *)
