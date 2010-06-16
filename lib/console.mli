(* Js_of_ocaml library
 * http://www.ocsigen.org/js_of_ocaml/
 * Copyright (C) 2010 Jérôme Vouillon
 * Laboratoire PPS - CNRS Université Paris Diderot
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

open Js

class type console = object
  (* Writes a message to the console. You may pass as many arguments
     as you'd like, and they will be joined together in a space-
     delimited line.  The first argument to log may be a string
     containing printf-like string substitution patterns.  If objects
     are logged, they will be written not as static text, but as
     interactive hyperlinks that can be clicked to inspect the
     object. *)
  method log : _ -> unit meth
  method log_2 : _ -> _ -> unit meth
  method log_3 : _ -> _ -> _ -> unit meth
  method log_4 : _ -> _ -> _ -> _ -> unit meth
  method log_5 : _ -> _ -> _ -> _ -> _ -> unit meth
  (* Writes a message to the console, including a hyperlink to the
     line where it was called. *)
  method debug : _ -> unit meth
  method debug_2 : _ -> _ -> unit meth
  method debug_3 : _ -> _ -> _ -> unit meth
  method debug_4 : _ -> _ -> _ -> _ -> unit meth
  method debug_5 : _ -> _ -> _ -> _ -> _ -> unit meth
  (* Writes a message to the console with the visual "info" icon and
     color coding and a hyperlink to the line where it was called. *)
  method info : _ -> unit meth
  method info_2 : _ -> _ -> unit meth
  method info_3 : _ -> _ -> _ -> unit meth
  method info_4 : _ -> _ -> _ -> _ -> unit meth
  method info_5 : _ -> _ -> _ -> _ -> _ -> unit meth
  (* Writes a message to the console with the visual "warning" icon
     and color coding and a hyperlink to the line where it was called. *)
  method warn : _ -> unit meth
  method warn_2 : _ -> _ -> unit meth
  method warn_3 : _ -> _ -> _ -> unit meth
  method warn_4 : _ -> _ -> _ -> _ -> unit meth
  method warn_5 : _ -> _ -> _ -> _ -> _ -> unit meth
  (* Writes a message to the console with the visual "error" icon and
     color coding and a hyperlink to the line where it was called. *)
  method error : _ -> unit meth
  method error_2 : _ -> _ -> unit meth
  method error_3 : _ -> _ -> _ -> unit meth
  method error_4 : _ -> _ -> _ -> _ -> unit meth
  method error_5 : _ -> _ -> _ -> _ -> _ -> unit meth
  (* Tests that an expression is true. If not, it will write a message
     to the console and throw an exception. *)
  method assert_ : bool t -> unit meth
  method assert_1 : bool t -> _ -> unit meth
  method assert_2 : bool t -> _ -> _ -> unit meth
  method assert_3 : bool t -> _ -> _ -> _ -> unit meth
  method assert_4 : bool t -> _ -> _ -> _ -> _ -> unit meth
  method assert_5 : bool t -> _ -> _ -> _ -> _ -> _ -> unit meth
  (* Prints an interactive listing of all properties of the object. *)
  method dir : _ -> unit meth
  (* Prints the XML source tree of an HTML or XML element. This looks
     identical to the view that you would see in the HTML tab. You can
     click on any node to inspect it in the HTML tab. *)
  method dirxml : #Dom.node t -> unit meth
  (* Prints an interactive stack trace of JavaScript execution at the
     point where it is called. *)
  method trace : unit meth
  (* Writes a message to the console and opens a nested block to
     indent all future messages sent to the console. Call
     console.groupEnd() to close the block. *)
  method group : _ -> unit meth
  method group_2 : _ -> _ -> unit meth
  method group_3 : _ -> _ -> _ -> unit meth
  method group_4 : _ -> _ -> _ -> _ -> unit meth
  method group_5 : _ -> _ -> _ -> _ -> _ -> unit meth
  (* Like console.group(), but the block is initially collapsed. *)
  method groupCollapsed : _ -> unit meth
  method groupCollapsed_2 : _ -> _ -> unit meth
  method groupCollapsed_3 : _ -> _ -> _ -> unit meth
  method groupCollapsed_4 : _ -> _ -> _ -> _ -> unit meth
  method groupCollapsed_5 : _ -> _ -> _ -> _ -> _ -> unit meth
  (* Closes the most recently opened block created by a call to
     console.group() or console.groupCollapsed() *)
  method groupEnd : unit meth
  (* Creates a new timer under the given name. Call
     console.timeEnd(name) with the same name to stop the timer and
     print the time elapsed. *)
  method time : js_string t -> unit meth
  (* Stops a timer created by a call to console.time(name) and writes
     the time elapsed. *)
  method timeEnd : js_string t -> unit meth
end

val console : console t
