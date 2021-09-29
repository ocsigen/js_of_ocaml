(* Js_of_ocaml library
   * http://www.ocsigen.org/js_of_ocaml/
   * Copyright (C) 2015 Stéphane Legrand
   *
   * This program is free software; you can redistribute it and/or modifiy
   * it under the terms of the GNU Lesser General Public License as published by
   * the Free Software Foundation, with linking exception;
   * either version 2.1 of the License, or (at your option) any later version.
   *
   * This program is distributed in the hope that it will be useful,
   * but WITHOUT ANY WARRANTY; without even the implied warranty of
   * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
   * GNU Lesser General Public License for more details.
   *
   * You should have received a copy of the GNU Lesser General Public License
   * along with this program; if not, write to the Free Software
   * Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
*)

class type weakMap =
  object
    method get : 'a -> 'b Js.meth

    method set : 'a -> 'b -> unit Js.meth

    method delete : 'a -> unit Js.meth

    method has : 'a -> bool Js.meth
  end

let weakM = Js.Unsafe.global ##. WeakMap

let is_supported () = Js.Optdef.test Js.Unsafe.global ##. WeakMap

let new_weakMap : (?init:('a -> 'b) array -> unit -> weakMap Js.t) Js.constr = weakM
