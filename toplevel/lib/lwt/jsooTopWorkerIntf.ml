(* Js_of_ocaml library
 * http://www.ocsigen.org/js_of_ocaml/
 * Copyright (C) 2016 OCamlPro
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU Library General Public License as published by
 * the Free Software Foundation, with linking exception;
 * either version 2.1 of the License, or (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Library General Public License for more details.
 *
 * You should have received a copy of the GNU Library General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
 *)
open! Js_of_ocaml_toplevel
open Js_of_ocaml

(** Types of the messages exchanged with a toplevel in a Web Worker. *)

type _ host_msg =
  | Init : string -> unit host_msg
  | Reset : unit host_msg
  | Check : bool * string -> unit host_msg
  | Execute : int option * bool * int * string -> bool host_msg
  | Use_string : string option * bool * int * string -> bool host_msg
  | Use_mod_string : int * bool * string * string option * string -> bool host_msg
  | Import_scripts : string list -> unit host_msg

type _ msg_ty =
  | Unit : unit msg_ty
  | Bool : bool msg_ty
  | Int : int msg_ty
  | String : Js.js_string Js.t msg_ty

type (_, _) eq = Eq : ('a, 'a) eq

type toploop_msg =
  | Write : int * string -> toploop_msg (* pseudo file descriptor * content *)
  | ReturnSuccess : int * 'a msg_ty * 'a * JsooTopWrapped.warning list -> toploop_msg
  | ReturnError : int * JsooTopWrapped.error * JsooTopWrapped.warning list -> toploop_msg
