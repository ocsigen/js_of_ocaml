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

(** Geolocation API
  @see <https://developer.mozilla.org/en-US/docs/Web/API/Geolocation> for API documentation.
  @see <http://www.w3.org/TR/geolocation-API/> for the W3C Recommendation.

  A code example:

  if (Geolocation.is_supported()) then
    let geo = Geolocation.geolocation in
    let options = Geolocation.empty_position_options() in
    let () = options##enableHighAccuracy <- true in
    let f_success pos =
      let coords = pos##coords in
      let latitude = coords##latitude in
      Firebug.console##debug(latitude) ;
    in
    let f_error err =
      let code = err##code in
      let msg = err##message in
      match code with
      | Geolocation.TIMEOUT -> Firebug.console##debug(msg)
      | _ -> ()
    in
    geo##getCurrentPosition(f_success, f_error, options) *)

type positionErrorCode =
    PERMISSION_DENIED
  | POSITION_UNAVAILABLE
  | TIMEOUT

class type coordinates = object
  method latitude : float Js.readonly_prop
  method longitude : float Js.readonly_prop
  method altitude : float Js.opt Js.readonly_prop
  method accuracy : float Js.readonly_prop
  method altitudeAccuracy : float Js.opt Js.readonly_prop
  method heading : float Js.opt Js.readonly_prop
  method speed : float Js.opt Js.readonly_prop
end

class type position = object
  method coords : coordinates Js.t Js.readonly_prop
  method timestamp : Js.date Js.readonly_prop
end

class type positionOptions = object
  method enableHighAccuracy : bool Js.prop
  method timeout : int Js.prop
  method maximumAge : int Js.prop
end

class type positionError = object
  method code : positionErrorCode Js.readonly_prop
  method message : Js.js_string Js.t Js.readonly_prop
end

class type geolocation = object
  method getCurrentPosition : (position Js.t -> unit)
    -> (positionError Js.t -> unit)
    -> positionOptions Js.t
    -> unit Js.meth
  method watchPosition : (position Js.t -> unit)
    -> (positionError Js.t -> unit)
    -> positionOptions Js.t
    -> int Js.meth
  method clearWatch : int -> unit Js.meth
end

val empty_position_options : unit -> positionOptions Js.t

val geolocation : geolocation Js.t

val is_supported : unit -> bool
