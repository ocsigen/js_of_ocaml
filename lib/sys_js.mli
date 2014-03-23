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

val register_file: name:string -> content:string -> unit
  (** Register a file to a Pseudo Filesystem.
      [register_file ~name ~content] register the file [name] with content [content]
      so it can be be opened with [Pervasives.open_in name] *)

val register_autoload : path:string -> (string -> string option) -> unit
  (** Register a callback to the [path] to dynamicly load missing files.
      Whenever a file is missing in [path], the callback is used to optionally
      get the content of the file.
      [register_autoload ~path f] register the callback [f] to the path [path].
      The callback [f] receives the absolute filename as arguement.*)

val set_channel_flusher : out_channel -> (string -> unit) -> unit
  (** Set a callback to be called when an out_channel flush its buffer.
      [set_channel_flusher chan cb] install the callback [cb] for [chan] out_channel.
      [cb] will be called with the string to flush. *)
