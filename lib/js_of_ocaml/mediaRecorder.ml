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

class type blobEvent = object
  inherit Dom_html.event

  method data : File.blob Js.t Js.readonly_prop

  method timecode : Js.number_t Js.optdef Js.readonly_prop
end

class type mediaRecorderErrorEvent = object
  inherit Dom_html.event

  method error : Js.error Js.t Js.readonly_prop
end

class type mediaRecorderOptions = object
  method mimeType : Js.js_string Js.t Js.writeonly_prop

  method audioBitsPerSecond : int Js.writeonly_prop

  method videoBitsPerSecond : int Js.writeonly_prop

  method bitsPerSecond : int Js.writeonly_prop
end

let empty_recorder_options () : mediaRecorderOptions Js.t = Js.Unsafe.obj [||]

class type mediaRecorder = object ('self)
  inherit Dom_html.eventTarget

  method stream : MediaCapture.mediaStream Js.t Js.readonly_prop

  method mimeType : Js.js_string Js.t Js.readonly_prop

  method state : Js.js_string Js.t Js.readonly_prop

  method audioBitsPerSecond : int Js.readonly_prop

  method videoBitsPerSecond : int Js.readonly_prop

  method audioBitrateMode : Js.js_string Js.t Js.readonly_prop

  method start : unit Js.meth

  method start_withTimeslice : int -> unit Js.meth

  method stop : unit Js.meth

  method pause : unit Js.meth

  method resume : unit Js.meth

  method requestData : unit Js.meth

  method ondataavailable :
    ('self Js.t, blobEvent Js.t) Dom.event_listener Js.writeonly_prop

  method onstart : ('self Js.t, Dom_html.event Js.t) Dom.event_listener Js.writeonly_prop

  method onstop : ('self Js.t, Dom_html.event Js.t) Dom.event_listener Js.writeonly_prop

  method onpause : ('self Js.t, Dom_html.event Js.t) Dom.event_listener Js.writeonly_prop

  method onresume : ('self Js.t, Dom_html.event Js.t) Dom.event_listener Js.writeonly_prop

  method onerror :
    ('self Js.t, mediaRecorderErrorEvent Js.t) Dom.event_listener Js.writeonly_prop
end

let mediaRecorder : (MediaCapture.mediaStream Js.t -> mediaRecorder Js.t) Js.constr =
  Js.Unsafe.global##._MediaRecorder

let mediaRecorder_withOptions :
    (MediaCapture.mediaStream Js.t -> mediaRecorderOptions Js.t -> mediaRecorder Js.t)
    Js.constr =
  Js.Unsafe.global##._MediaRecorder

let is_supported () = Js.Optdef.test Js.Unsafe.global##._MediaRecorder

let is_type_supported ty =
  is_supported ()
  && Js.to_bool (Js.Unsafe.global##._MediaRecorder##isTypeSupported (Js.string ty))
