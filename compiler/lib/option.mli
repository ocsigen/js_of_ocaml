(* Js_of_ocaml compiler
 * http://www.ocsigen.org/js_of_ocaml/
 * Copyright (C) 2013 Hugo Heuzard
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

val stop_profiling : unit -> unit
val start_profiling : string -> unit
module Debug : sig
  val available : unit -> string list
  val find : string -> unit -> bool
  val enable : string -> unit
  val disable : string -> unit
end

module Optim : sig
  val available : unit -> string list
  val deadcode : unit -> bool
  val optcall : unit -> bool
  val shortvar : unit -> bool
  val compact : unit -> bool
  val inline : unit -> bool
  val share_constant : unit -> bool
  val staticeval : unit -> bool
  val genprim : unit -> bool
  val strictmode : unit -> bool
  val compact_vardecl : unit -> bool
  val debugger : unit -> bool
  val pretty : unit -> bool
  val stable_var : unit -> bool
  val debuginfo : unit -> bool
  val excwrap : unit -> bool
  val include_cmis: unit -> bool
  val improved_stacktrace : unit -> bool

  val warn_unused : unit -> bool
  val inline_callgen : unit -> bool

  val check_magic : unit -> bool

  val enable : string -> unit
  val disable : string -> unit
end

module Param : sig
  val set : string -> string -> unit
  val all : unit -> (string * string) list
  val switch_max_case : unit -> int
  val tailcall_max_depth : unit -> int
  val constant_max_depth : unit -> int

  type tc =
     | TcNone
     | TcTrampoline
     (* | TcWhile *)
  val tailcall_optim : unit -> tc
end

val global_object : string
val extra_js_files : string list
