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

class type mediaTrackSettings = object
  method deviceId : Js.js_string Js.t Js.optdef Js.readonly_prop

  method groupId : Js.js_string Js.t Js.optdef Js.readonly_prop

  method width : int Js.optdef Js.readonly_prop

  method height : int Js.optdef Js.readonly_prop

  method aspectRatio : Js.number_t Js.optdef Js.readonly_prop

  method frameRate : Js.number_t Js.optdef Js.readonly_prop

  method facingMode : Js.js_string Js.t Js.optdef Js.readonly_prop

  method resizeMode : Js.js_string Js.t Js.optdef Js.readonly_prop

  method sampleRate : int Js.optdef Js.readonly_prop

  method sampleSize : int Js.optdef Js.readonly_prop

  method channelCount : int Js.optdef Js.readonly_prop

  method echoCancellation : bool Js.t Js.optdef Js.readonly_prop

  method noiseSuppression : bool Js.t Js.optdef Js.readonly_prop

  method autoGainControl : bool Js.t Js.optdef Js.readonly_prop
end

class type mediaTrackConstraints = object
  method deviceId : Js.js_string Js.t Js.writeonly_prop

  method groupId : Js.js_string Js.t Js.writeonly_prop

  method facingMode : Js.js_string Js.t Js.writeonly_prop

  method width : int Js.writeonly_prop

  method height : int Js.writeonly_prop

  method aspectRatio : Js.number_t Js.writeonly_prop

  method frameRate : Js.number_t Js.writeonly_prop

  method resizeMode : Js.js_string Js.t Js.writeonly_prop

  method sampleRate : int Js.writeonly_prop

  method sampleSize : int Js.writeonly_prop

  method channelCount : int Js.writeonly_prop

  method echoCancellation : bool Js.t Js.writeonly_prop

  method noiseSuppression : bool Js.t Js.writeonly_prop

  method autoGainControl : bool Js.t Js.writeonly_prop
end

let empty_track_constraints () : mediaTrackConstraints Js.t = Js.Unsafe.obj [||]

class type mediaStreamConstraints = object
  method audio : bool Js.t Js.writeonly_prop

  method audio_constr : mediaTrackConstraints Js.t Js.writeonly_prop

  method video : bool Js.t Js.writeonly_prop

  method video_constr : mediaTrackConstraints Js.t Js.writeonly_prop
end

let empty_stream_constraints () : mediaStreamConstraints Js.t = Js.Unsafe.obj [||]

class type mediaStreamTrack = object ('self)
  inherit Dom_html.eventTarget

  method kind : Js.js_string Js.t Js.readonly_prop

  method id : Js.js_string Js.t Js.readonly_prop

  method label : Js.js_string Js.t Js.readonly_prop

  method enabled : bool Js.t Js.prop

  method muted : bool Js.t Js.readonly_prop

  method readyState : Js.js_string Js.t Js.readonly_prop

  method contentHint : Js.js_string Js.t Js.prop

  method clone : mediaStreamTrack Js.t Js.meth

  method stop : unit Js.meth

  method getSettings : mediaTrackSettings Js.t Js.meth

  method getConstraints : mediaTrackConstraints Js.t Js.meth

  method getCapabilities : Js.Unsafe.any Js.meth

  method applyConstraints : mediaTrackConstraints Js.t -> unit Promise.t Js.meth

  method onended : ('self Js.t, 'self Dom.event Js.t) Dom.event_listener Js.writeonly_prop

  method onmute : ('self Js.t, 'self Dom.event Js.t) Dom.event_listener Js.writeonly_prop

  method onunmute :
    ('self Js.t, 'self Dom.event Js.t) Dom.event_listener Js.writeonly_prop
end

class type trackEvent = object
  inherit Dom_html.event

  method track : mediaStreamTrack Js.t Js.readonly_prop
end

class type mediaStream = object ('self)
  inherit Dom_html.eventTarget

  method id : Js.js_string Js.t Js.readonly_prop

  method active : bool Js.t Js.readonly_prop

  method getTracks : mediaStreamTrack Js.t Js.js_array Js.t Js.meth

  method getAudioTracks : mediaStreamTrack Js.t Js.js_array Js.t Js.meth

  method getVideoTracks : mediaStreamTrack Js.t Js.js_array Js.t Js.meth

  method getTrackById : Js.js_string Js.t -> mediaStreamTrack Js.t Js.opt Js.meth

  method addTrack : mediaStreamTrack Js.t -> unit Js.meth

  method removeTrack : mediaStreamTrack Js.t -> unit Js.meth

  method clone : mediaStream Js.t Js.meth

  method onaddtrack : ('self Js.t, trackEvent Js.t) Dom.event_listener Js.writeonly_prop

  method onremovetrack :
    ('self Js.t, trackEvent Js.t) Dom.event_listener Js.writeonly_prop
end

let mediaStream : mediaStream Js.t Js.constr = Js.Unsafe.global##._MediaStream

let mediaStream_fromStream : (mediaStream Js.t -> mediaStream Js.t) Js.constr =
  Js.Unsafe.global##._MediaStream

let mediaStream_fromTracks :
    (mediaStreamTrack Js.t Js.js_array Js.t -> mediaStream Js.t) Js.constr =
  Js.Unsafe.global##._MediaStream

class type mediaDeviceInfo = object
  method deviceId : Js.js_string Js.t Js.readonly_prop

  method groupId : Js.js_string Js.t Js.readonly_prop

  method kind : Js.js_string Js.t Js.readonly_prop

  method label : Js.js_string Js.t Js.readonly_prop
end

class type mediaTrackSupportedConstraints = object
  method deviceId : bool Js.t Js.optdef Js.readonly_prop

  method groupId : bool Js.t Js.optdef Js.readonly_prop

  method facingMode : bool Js.t Js.optdef Js.readonly_prop

  method width : bool Js.t Js.optdef Js.readonly_prop

  method height : bool Js.t Js.optdef Js.readonly_prop

  method aspectRatio : bool Js.t Js.optdef Js.readonly_prop

  method frameRate : bool Js.t Js.optdef Js.readonly_prop

  method resizeMode : bool Js.t Js.optdef Js.readonly_prop

  method sampleRate : bool Js.t Js.optdef Js.readonly_prop

  method sampleSize : bool Js.t Js.optdef Js.readonly_prop

  method channelCount : bool Js.t Js.optdef Js.readonly_prop

  method echoCancellation : bool Js.t Js.optdef Js.readonly_prop

  method noiseSuppression : bool Js.t Js.optdef Js.readonly_prop

  method autoGainControl : bool Js.t Js.optdef Js.readonly_prop
end

class type mediaDevices = object ('self)
  inherit Dom_html.eventTarget

  method enumerateDevices : mediaDeviceInfo Js.t Js.js_array Js.t Promise.t Js.meth

  method getSupportedConstraints : mediaTrackSupportedConstraints Js.t Js.meth

  method getUserMedia : mediaStreamConstraints Js.t -> mediaStream Js.t Promise.t Js.meth

  method getDisplayMedia : mediaStream Js.t Promise.t Js.meth

  method getDisplayMedia_withConstraints :
    mediaStreamConstraints Js.t -> mediaStream Js.t Promise.t Js.meth

  method ondevicechange :
    ('self Js.t, 'self Dom.event Js.t) Dom.event_listener Js.writeonly_prop
end

let mediaDevices () : mediaDevices Js.t = Js.Unsafe.global##.navigator##.mediaDevices

let is_supported () =
  Js.Optdef.test Js.Unsafe.global##.navigator
  && Js.Optdef.test Js.Unsafe.global##.navigator##.mediaDevices
