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

(** Media Capture and Streams: [MediaStream], [MediaStreamTrack] and
    [navigator.mediaDevices] ([getUserMedia], [getDisplayMedia],
    [enumerateDevices]).

    To render a captured stream, assign it to the [srcObject] property of a
    {!Dom_html.mediaElement}:
    {[
      el##.srcObject := Js.some (Js.Unsafe.inject stream)
    ]}

    @see <https://developer.mozilla.org/en-US/docs/Web/API/Media_Capture_and_Streams_API>
    @see <https://developer.mozilla.org/en-US/docs/Web/API/MediaDevices>
    @see <https://www.w3.org/TR/mediacapture-streams/> *)

open Js

(** {2 Track settings and constraints} *)

(** The values in effect for a track, as returned by
    [track##getSettings]. Fields not applicable to the track's kind are
    [undefined]. *)
class type mediaTrackSettings = object
  method deviceId : js_string t optdef readonly_prop

  method groupId : js_string t optdef readonly_prop

  (** {3 Video tracks} *)

  method width : int optdef readonly_prop

  method height : int optdef readonly_prop

  method aspectRatio : number_t optdef readonly_prop

  method frameRate : number_t optdef readonly_prop

  method facingMode : js_string t optdef readonly_prop
  (** One of ["user"], ["environment"], ["left"] or ["right"]. *)

  method resizeMode : js_string t optdef readonly_prop

  (** {3 Audio tracks} *)

  method sampleRate : int optdef readonly_prop

  method sampleSize : int optdef readonly_prop

  method channelCount : int optdef readonly_prop

  method echoCancellation : bool t optdef readonly_prop

  method noiseSuppression : bool t optdef readonly_prop

  method autoGainControl : bool t optdef readonly_prop
end

(** Constraints on a single track. All fields are optional; create an empty
    record with {!empty_track_constraints} and populate the ones you need.
    Each field is typed with its plain ("ideal") value; to pass an advanced
    [ConstrainRange]/[ConstrainDOMString] record such as
    [{exact: ...; min: ...}], build it with {!Js.Unsafe.obj} and assign it with
    {!Js.Unsafe.set}. *)
class type mediaTrackConstraints = object
  method deviceId : js_string t writeonly_prop

  method groupId : js_string t writeonly_prop

  method facingMode : js_string t writeonly_prop

  method width : int writeonly_prop

  method height : int writeonly_prop

  method aspectRatio : number_t writeonly_prop

  method frameRate : number_t writeonly_prop

  method resizeMode : js_string t writeonly_prop

  method sampleRate : int writeonly_prop

  method sampleSize : int writeonly_prop

  method channelCount : int writeonly_prop

  method echoCancellation : bool t writeonly_prop

  method noiseSuppression : bool t writeonly_prop

  method autoGainControl : bool t writeonly_prop
end

val empty_track_constraints : unit -> mediaTrackConstraints t

(** Argument of [getUserMedia]/[getDisplayMedia]: which kinds of tracks are
    requested. Each kind is either enabled with a plain boolean ([audio],
    [video]) or requested with per-track constraints ([audio_constr],
    [video_constr] — both write the same underlying field). Create an empty
    record with {!empty_stream_constraints}. *)
class type mediaStreamConstraints = object
  method audio : bool t writeonly_prop

  method audio_constr : mediaTrackConstraints t writeonly_prop

  method video : bool t writeonly_prop

  method video_constr : mediaTrackConstraints t writeonly_prop
end

val empty_stream_constraints : unit -> mediaStreamConstraints t

(** {2 Tracks and streams} *)

(** A single media track (an audio or video source). *)
class type mediaStreamTrack = object ('self)
  inherit Dom_html.eventTarget

  method kind : js_string t readonly_prop
  (** Either ["audio"] or ["video"]. *)

  method id : js_string t readonly_prop

  method label : js_string t readonly_prop

  method enabled : bool t prop
  (** Disabled tracks render silence / black frames. *)

  method muted : bool t readonly_prop

  method readyState : js_string t readonly_prop
  (** Either ["live"] or ["ended"]. *)

  method contentHint : js_string t prop

  method clone : mediaStreamTrack t meth

  method stop : unit meth

  method getSettings : mediaTrackSettings t meth

  method getConstraints : mediaTrackConstraints t meth

  method getCapabilities : Unsafe.any meth
  (** The [MediaTrackCapabilities] record: same field names as
      {!mediaTrackSettings} but with range values. *)

  method applyConstraints : mediaTrackConstraints t -> unit Promise.t meth

  method onended : ('self t, 'self Dom.event t) Dom.event_listener writeonly_prop

  method onmute : ('self t, 'self Dom.event t) Dom.event_listener writeonly_prop

  method onunmute : ('self t, 'self Dom.event t) Dom.event_listener writeonly_prop
end

(** The event delivered to [addtrack] / [removetrack] listeners. *)
class type trackEvent = object
  inherit Dom_html.event

  method track : mediaStreamTrack t readonly_prop
end

class type mediaStream = object ('self)
  inherit Dom_html.eventTarget

  method id : js_string t readonly_prop

  method active : bool t readonly_prop

  method getTracks : mediaStreamTrack t js_array t meth

  method getAudioTracks : mediaStreamTrack t js_array t meth

  method getVideoTracks : mediaStreamTrack t js_array t meth

  method getTrackById : js_string t -> mediaStreamTrack t opt meth

  method addTrack : mediaStreamTrack t -> unit meth

  method removeTrack : mediaStreamTrack t -> unit meth

  method clone : mediaStream t meth

  method onaddtrack : ('self t, trackEvent t) Dom.event_listener writeonly_prop

  method onremovetrack : ('self t, trackEvent t) Dom.event_listener writeonly_prop
end

val mediaStream : mediaStream t constr
(** [new%js mediaStream] creates an empty stream. *)

val mediaStream_fromStream : (mediaStream t -> mediaStream t) constr
(** Creates a stream sharing the tracks of an existing stream. *)

val mediaStream_fromTracks : (mediaStreamTrack t js_array t -> mediaStream t) constr

(** {2 Devices} *)

class type mediaDeviceInfo = object
  method deviceId : js_string t readonly_prop

  method groupId : js_string t readonly_prop

  method kind : js_string t readonly_prop
  (** One of ["audioinput"], ["audiooutput"] or ["videoinput"]. *)

  method label : js_string t readonly_prop
  (** The empty string when no capture permission has been granted. *)
end

(** Which constraints the user agent understands: one boolean field per
    {!mediaTrackConstraints} field name. *)
class type mediaTrackSupportedConstraints = object
  method deviceId : bool t optdef readonly_prop

  method groupId : bool t optdef readonly_prop

  method facingMode : bool t optdef readonly_prop

  method width : bool t optdef readonly_prop

  method height : bool t optdef readonly_prop

  method aspectRatio : bool t optdef readonly_prop

  method frameRate : bool t optdef readonly_prop

  method resizeMode : bool t optdef readonly_prop

  method sampleRate : bool t optdef readonly_prop

  method sampleSize : bool t optdef readonly_prop

  method channelCount : bool t optdef readonly_prop

  method echoCancellation : bool t optdef readonly_prop

  method noiseSuppression : bool t optdef readonly_prop

  method autoGainControl : bool t optdef readonly_prop
end

(** The [navigator.mediaDevices] singleton. *)
class type mediaDevices = object ('self)
  inherit Dom_html.eventTarget

  method enumerateDevices : mediaDeviceInfo t js_array t Promise.t meth

  method getSupportedConstraints : mediaTrackSupportedConstraints t meth

  method getUserMedia : mediaStreamConstraints t -> mediaStream t Promise.t meth
  (** Prompts the user for camera / microphone access. Rejects with
      [NotAllowedError] when denied and [NotFoundError] when no device
      matches. *)

  method getDisplayMedia : mediaStream t Promise.t meth
  (** Prompts the user to pick a screen / window / tab to capture. *)

  method getDisplayMedia_withConstraints :
    mediaStreamConstraints t -> mediaStream t Promise.t meth

  method ondevicechange : ('self t, 'self Dom.event t) Dom.event_listener writeonly_prop
end

val mediaDevices : unit -> mediaDevices t
(** The [navigator.mediaDevices] object. Only meaningful when {!is_supported}
    returns [true]. *)

val is_supported : unit -> bool
(** Whether [navigator.mediaDevices] is available in the current
    environment. *)
