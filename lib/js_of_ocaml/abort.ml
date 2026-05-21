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

open! Import

class type signal = object ('self)
  method aborted : bool Js.t Js.readonly_prop

  method reason : Js.Unsafe.any Js.readonly_prop

  method onabort : ('self Js.t, 'self Dom.event Js.t) Dom.event_listener Js.writeonly_prop

  method throwIfAborted : unit Js.meth
end

class type controller = object
  method signal : signal Js.t Js.readonly_prop

  method abort : unit Js.meth

  method abort_reason : Js.Unsafe.any -> unit Js.meth
end

let controller : controller Js.t Js.constr = Js.Unsafe.global##._AbortController
