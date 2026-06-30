(* Js_of_ocaml library
 * http://www.ocsigen.org/js_of_ocaml/
 * Copyright (C) 2016 OCamlPro
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
(** Types of the messages exchanged with a toplevel in a Web Worker. *)

(* Identifier correlating a request with its reply. Abstract so it cannot be
   confused with an [Fd.t] (both are plain integers underneath). *)
module Message_id : sig
  type t

  val first : t

  val next : t -> t

  val to_int : t -> int
  (** For logging only. *)

  module Map : Map.S with type key = t
end = struct
  type t = int

  let first = 0

  let next t = t + 1

  let to_int t = t

  module Map = Map.Make (Int)
end

(* Pseudo file descriptor naming an output channel (stdout, stderr, or a
   per-call answer/code formatter). Abstract so it cannot be confused with a
   [Message_id.t]. *)
module Fd : sig
  type t

  val stdout : t

  val stderr : t

  val first_free : t
  (** First descriptor available to [next]; [stdout] and [stderr] are reserved
      below it and persist across resets. *)

  val next : t -> t

  val is_reserved : t -> bool

  val to_int : t -> int
  (** For logging only. *)

  module Map : Map.S with type key = t
end = struct
  type t = int

  let stdout = 0

  let stderr = 1

  let first_free = 2

  let next t = t + 1

  let is_reserved t = t = stdout || t = stderr

  let to_int t = t

  module Map = Map.Make (Int)
end

(* Identifier of a worker-side parsing session (a [Lexing.lexbuf] held in the
   worker, stepped one phrase at a time). Like [Fd.t], the state stays in the
   worker and only this id crosses the boundary. *)
module Lexbuf : sig
  type t

  val first : t

  val next : t -> t

  module Map : Map.S with type key = t
end = struct
  type t = int

  let first = 0

  let next t = t + 1

  module Map = Map.Make (Int)
end

(* Result of stepping one phrase: [`Phrase ok] ran a phrase (with [ok] false
   if it raised at evaluation), [`Eof] once the buffer is exhausted. *)
type step_result =
  [ `Phrase of bool
  | `Eof
  ]

type _ host_msg =
  | Init : { cmis_base_url : string } -> unit host_msg
  | Reset : unit host_msg
  | Check : { setenv : bool; code : string } -> unit host_msg
  | Clear_check : unit host_msg
  | Execute :
      { code_fd : Fd.t option
      ; print_outcome : bool
      ; answer_fd : Fd.t
      ; code : string
      }
      -> bool host_msg
  | Use_string :
      { filename : string option
      ; print_outcome : bool
      ; answer_fd : Fd.t
      ; code : string
      }
      -> bool host_msg
  | Use_mod_string :
      { answer_fd : Fd.t
      ; print_outcome : bool
      ; modname : string
      ; sig_code : string option
      ; code : string
      }
      -> bool host_msg
  | Import_scripts : string list -> unit host_msg
  | Open_lexbuf : { id : Lexbuf.t; code : string; code_fd : Fd.t option } -> unit host_msg
  | Step : { lexbuf : Lexbuf.t; print_outcome : bool; answer_fd : Fd.t } -> step_result host_msg
  | Close_lexbuf : { lexbuf : Lexbuf.t } -> unit host_msg

type _ msg_ty =
  | Unit : unit msg_ty
  | Bool : bool msg_ty
  | Int : int msg_ty
  | String : string msg_ty
  | Step_result : step_result msg_ty

type (_, _) eq = Eq : ('a, 'a) eq

type toploop_msg =
  | Write : Fd.t * string -> toploop_msg (* pseudo file descriptor * content *)
  | ReturnSuccess : Message_id.t * 'a msg_ty * 'a * Wrapped_intf.warning list -> toploop_msg
  | ReturnError : Message_id.t * Wrapped_intf.error * Wrapped_intf.warning list -> toploop_msg

let ty_of_host_msg : type t. t host_msg -> t msg_ty = function
  | Init _ -> Unit
  | Reset -> Unit
  | Check _ -> Unit
  | Clear_check -> Unit
  | Execute _ -> Bool
  | Use_string _ -> Bool
  | Use_mod_string _ -> Bool
  | Import_scripts _ -> Unit
  | Open_lexbuf _ -> Unit
  | Step _ -> Step_result
  | Close_lexbuf _ -> Unit
