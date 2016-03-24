(* Js_of_ocaml library
 * http://www.ocsigen.org/js_of_ocaml/
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

(** This is the javascript equivalent of Async_unix.Scheduler, ie a reimplementation of
    the async scheduler for javascript (at least the part of it that makes sense). *)

open Async_kernel.Std

(** [sleep d] is a deferred which becomes determined in [d] seconds. *)
val sleep : float -> unit Deferred.t

(** [yield ()] returns a deferred that becomes determined after the current cycle
    completes. *)
val yield : unit -> unit Deferred.t

(** Initialize the async scheduler *)
val init : unit -> unit
