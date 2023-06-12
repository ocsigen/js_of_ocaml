(* Js_of_ocaml library
 * http://www.ocsigen.org/js_of_ocaml/
 * Copyright (C) 2015 Stéphane Legrand
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

type positionErrorCode

type watchId

class type coordinates =
  object
    method latitude : float Js.t Js.readonly_prop

    method longitude : float Js.t Js.readonly_prop

    method altitude : float Js.t Js.opt Js.readonly_prop

    method accuracy : float Js.t Js.readonly_prop

    method altitudeAccuracy : float Js.t Js.opt Js.readonly_prop

    method heading : float Js.t Js.opt Js.readonly_prop

    method speed : float Js.t Js.opt Js.readonly_prop
  end

class type position =
  object
    method coords : coordinates Js.t Js.readonly_prop

    method timestamp : Js.date Js.readonly_prop
  end

class type positionOptions =
  object
    method enableHighAccuracy : bool Js.writeonly_prop

    method timeout : int Js.writeonly_prop

    method maximumAge : int Js.writeonly_prop
  end

class type positionError =
  object
    method _PERMISSION_DENIED_ : positionErrorCode Js.readonly_prop

    method _POSITION_UNAVAILABLE_ : positionErrorCode Js.readonly_prop

    method _TIMEOUT : positionErrorCode Js.readonly_prop

    method code : positionErrorCode Js.readonly_prop

    method message : Js.js_string Js.t Js.readonly_prop
  end

class type geolocation =
  object
    method getCurrentPosition :
         (position Js.t -> unit) Js.callback
      -> (positionError Js.t -> unit) Js.callback
      -> positionOptions Js.t
      -> unit Js.meth

    method watchPosition :
         (position Js.t -> unit) Js.callback
      -> (positionError Js.t -> unit) Js.callback
      -> positionOptions Js.t
      -> watchId Js.meth

    method clearWatch : watchId -> unit Js.meth
  end

let empty_position_options () = Js.Unsafe.obj [||]

let geolocation =
  let x = Js.Unsafe.global##.navigator in
  if Js.Optdef.test x then x##.geolocation else x

(* undefined *)

let is_supported () = Js.Optdef.test geolocation
