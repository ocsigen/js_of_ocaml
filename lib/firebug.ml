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

type any = Js.Unsafe.any

class type console = object
  method log : any -> unit meth
  method log_2 : any -> any -> unit meth
  method log_3 : any -> any -> any -> unit meth
  method log_4 : any -> any -> any -> any -> unit meth
  method log_5 : any -> any -> any -> any -> any -> unit meth
  method debug : any -> unit meth
  method debug_2 : any -> any -> unit meth
  method debug_3 : any -> any -> any -> unit meth
  method debug_4 : any -> any -> any -> any -> unit meth
  method debug_5 : any -> any -> any -> any -> any -> unit meth
  method info : any -> unit meth
  method info_2 : any -> any -> unit meth
  method info_3 : any -> any -> any -> unit meth
  method info_4 : any -> any -> any -> any -> unit meth
  method info_5 : any -> any -> any -> any -> any -> unit meth
  method warn : any -> unit meth
  method warn_2 : any -> any -> unit meth
  method warn_3 : any -> any -> any -> unit meth
  method warn_4 : any -> any -> any -> any -> unit meth
  method warn_5 : any -> any -> any -> any -> any -> unit meth
  method error : any -> unit meth
  method error_2 : any -> any -> unit meth
  method error_3 : any -> any -> any -> unit meth
  method error_4 : any -> any -> any -> any -> unit meth
  method error_5 : any -> any -> any -> any -> any -> unit meth
  method assert_ : bool t -> unit meth
  method assert_1 : bool t -> any -> unit meth
  method assert_2 : bool t -> any -> any -> unit meth
  method assert_3 : bool t -> any -> any -> any -> unit meth
  method assert_4 : bool t -> any -> any -> any -> any -> unit meth
  method assert_5 : bool t -> any -> any -> any -> any -> any -> unit meth
  method dir : any -> unit meth
  method dirxml : Dom.node t -> unit meth
  method trace : unit meth
  method group : any -> unit meth
  method group_2 : any -> any -> unit meth
  method group_3 : any -> any -> any -> unit meth
  method group_4 : any -> any -> any -> any -> unit meth
  method group_5 : any -> any -> any -> any -> any -> unit meth
  method groupCollapsed : any -> unit meth
  method groupCollapsed_2 : any -> any -> unit meth
  method groupCollapsed_3 : any -> any -> any -> unit meth
  method groupCollapsed_4 : any -> any -> any -> any -> unit meth
  method groupCollapsed_5 : any -> any -> any -> any -> any -> unit meth
  method groupEnd : unit meth
  method time : js_string t -> unit meth
  method timeEnd : js_string t -> unit meth
end

external get_console : unit -> console t = "caml_js_get_console"

let console = get_console ()
