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


include module type of Lwt_log_core
  with type level = Lwt_log_core.level
   and type logger = Lwt_log_core.logger
   and type section = Lwt_log_core.section
   and type template = Lwt_log_core.template
   and module Section = Lwt_log_core.Section

(** Lwt logger for js_of_ocaml *)

(** {2 Predefined logger} *)

val console : Lwt_log.logger
(** Logger that use the javascript console object. *)


(** {2 Logging functions} *)

val log : ?inspect : 'v -> ?exn : exn -> ?section : section -> ?location : (string * int * int) -> ?logger : logger -> level : level -> string -> unit Lwt.t
  (** [log ?section ?logger ~level message] logs a message.

      [section] defaults to {!Section.main}. If [logger] is not
      specified, then the default one is used instead (see
      {!default}).

      If [exn] is provided, then its string representation
      (= [Printexc.to_string exn]) will be append to the message, and if
      possible the backtrace will also be logged.

      If [inspect] is provided, it will be append to the message.

      [location] contains the location of the logging directive, it is
      of the form [(file_name, line, column)]. *)

val log_f : ?inspect : 'v -> ?exn : exn -> ?section : section -> ?location : (string * int * int) -> ?logger : logger -> level : level -> ('a, unit, string, unit Lwt.t) format4 -> 'a
  (** [log_f] is the same as [log] except that it takes a format
      string *)

val ign_log : ?inspect : 'v -> ?exn : exn -> ?section : section -> ?location : (string * int * int) -> ?logger : logger -> level : level -> string -> unit
  (** Same as {!log} but ignore the resulting thread. *)

val ign_log_f : ?inspect : 'v -> ?exn : exn -> ?section : section -> ?location : (string * int * int) -> ?logger : logger -> level : level -> ('a, unit, string, unit) format4 -> 'a
  (** Same as {!log_f} but ignore the resulting thread. *)

(** The following functions are the same as {!log} except that their
    name determines which level is used.

    For example {!info msg} is the same as {!log ~level:Info msg}.
*)

val debug : ?inspect : 'v -> ?exn : exn -> ?section : section -> ?location : (string * int * int) -> ?logger : logger -> string -> unit Lwt.t
val debug_f : ?inspect : 'v -> ?exn : exn ->  ?section : section -> ?location : (string * int * int) -> ?logger : logger -> ('a, unit, string, unit Lwt.t) format4 -> 'a
val ign_debug : ?inspect : 'v -> ?exn : exn -> ?section : section -> ?location : (string * int * int) -> ?logger : logger -> string -> unit
val ign_debug_f : ?inspect : 'v -> ?exn : exn ->  ?section : section -> ?location : (string * int * int) -> ?logger : logger -> ('a, unit, string, unit) format4 -> 'a

val info : ?inspect : 'v -> ?exn : exn -> ?section : section -> ?location : (string * int * int) -> ?logger : logger -> string -> unit Lwt.t
val info_f : ?inspect : 'v -> ?exn : exn ->  ?section : section -> ?location : (string * int * int) -> ?logger : logger -> ('a, unit, string, unit Lwt.t) format4 -> 'a
val ign_info : ?inspect : 'v -> ?exn : exn -> ?section : section -> ?location : (string * int * int) -> ?logger : logger -> string -> unit
val ign_info_f : ?inspect : 'v -> ?exn : exn ->  ?section : section -> ?location : (string * int * int) -> ?logger : logger -> ('a, unit, string, unit) format4 -> 'a

val notice : ?inspect : 'v -> ?exn : exn -> ?section : section -> ?location : (string * int * int) -> ?logger : logger -> string -> unit Lwt.t
val notice_f : ?inspect : 'v -> ?exn : exn ->  ?section : section -> ?location : (string * int * int) -> ?logger : logger -> ('a, unit, string, unit Lwt.t) format4 -> 'a
val ign_notice : ?inspect : 'v -> ?exn : exn -> ?section : section -> ?location : (string * int * int) -> ?logger : logger -> string -> unit
val ign_notice_f : ?inspect : 'v -> ?exn : exn ->  ?section : section -> ?location : (string * int * int) -> ?logger : logger -> ('a, unit, string, unit) format4 -> 'a

val warning : ?inspect : 'v -> ?exn : exn -> ?section : section -> ?location : (string * int * int) -> ?logger : logger -> string -> unit Lwt.t
val warning_f : ?inspect : 'v -> ?exn : exn ->  ?section : section -> ?location : (string * int * int) -> ?logger : logger -> ('a, unit, string, unit Lwt.t) format4 -> 'a
val ign_warning : ?inspect : 'v -> ?exn : exn -> ?section : section -> ?location : (string * int * int) -> ?logger : logger -> string -> unit
val ign_warning_f : ?inspect : 'v -> ?exn : exn ->  ?section : section -> ?location : (string * int * int) -> ?logger : logger -> ('a, unit, string, unit) format4 -> 'a

val error : ?inspect : 'v -> ?exn : exn -> ?section : section -> ?location : (string * int * int) -> ?logger : logger -> string -> unit Lwt.t
val error_f : ?inspect : 'v -> ?exn : exn ->  ?section : section -> ?location : (string * int * int) -> ?logger : logger -> ('a, unit, string, unit Lwt.t) format4 -> 'a
val ign_error : ?inspect : 'v -> ?exn : exn -> ?section : section -> ?location : (string * int * int) -> ?logger : logger -> string -> unit
val ign_error_f : ?inspect : 'v -> ?exn : exn ->  ?section : section -> ?location : (string * int * int) -> ?logger : logger -> ('a, unit, string, unit) format4 -> 'a

val fatal : ?inspect : 'v -> ?exn : exn -> ?section : section -> ?location : (string * int * int) -> ?logger : logger -> string -> unit Lwt.t
val fatal_f : ?inspect : 'v -> ?exn : exn ->  ?section : section -> ?location : (string * int * int) -> ?logger : logger -> ('a, unit, string, unit Lwt.t) format4 -> 'a
val ign_fatal : ?inspect : 'v -> ?exn : exn -> ?section : section -> ?location : (string * int * int) -> ?logger : logger -> string -> unit
val ign_fatal_f : ?inspect : 'v -> ?exn : exn ->  ?section : section -> ?location : (string * int * int) -> ?logger : logger -> ('a, unit, string, unit) format4 -> 'a
