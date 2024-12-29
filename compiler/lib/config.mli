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

module Flag : sig
  val available : unit -> string list

  val find : string -> bool

  val set : string -> bool -> unit

  val deadcode : unit -> bool

  val globaldeadcode : unit -> bool

  val optcall : unit -> bool

  val shortvar : unit -> bool

  val compact : unit -> bool

  val inline : unit -> bool

  val share_constant : unit -> bool

  val staticeval : unit -> bool

  val effects : unit -> bool

  val genprim : unit -> bool

  val strictmode : unit -> bool

  val compact_vardecl : unit -> bool

  val debugger : unit -> bool

  val pretty : unit -> bool

  val stable_var : unit -> bool

  val debuginfo : unit -> bool

  val excwrap : unit -> bool

  val improved_stacktrace : unit -> bool

  val warn_unused : unit -> bool

  val inline_callgen : unit -> bool

  val safe_string : unit -> bool

  val use_js_string : unit -> bool

  val check_magic : unit -> bool

  val header : unit -> bool

  val auto_link : unit -> bool

  val es6 : unit -> bool

  val enable : string -> unit

  val disable : string -> unit
end

(** This module contains parameters that may be modified through command-line flags. *)
module Param : sig
  val set : string -> string -> unit

  val all : unit -> (string * string) list

  val switch_max_case : unit -> int

  val inlining_limit : unit -> int

  val tailcall_max_depth : unit -> int

  val constant_max_depth : unit -> int

  type tc =
    | TcNone
    | TcTrampoline

  (* | TcWhile *)
  val tailcall_optim : unit -> tc

  val lambda_lifting_threshold : unit -> int

  val lambda_lifting_baseline : unit -> int
end

(****)

(** {2 Parameters that are constant across a program run} *)

(** These parameters should be set at most once at the beginning of the program. *)

val target : unit -> [ `JavaScript | `Wasm ]

val set_target : [ `JavaScript | `Wasm ] -> unit

type effects_backend =
  [ `Disabled
  | `Cps
  | `Double_translation
  | `Jspi
  ]

val effects : unit -> effects_backend

val set_effects_backend : effects_backend -> unit
