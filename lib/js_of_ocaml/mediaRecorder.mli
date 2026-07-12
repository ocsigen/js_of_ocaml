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

(** MediaStream Recording: record a {!MediaCapture.mediaStream} into
    {!File.blob} chunks.

    Without a timeslice, [start] records into a single chunk delivered to
    [dataavailable] when the recording stops; pass a timeslice (in
    milliseconds) to receive periodic chunks instead. Feed the blobs to
    [window##._URL##createObjectURL] to replay them in a
    {!Dom_html.mediaElement}.

    @see <https://developer.mozilla.org/en-US/docs/Web/API/MediaStream_Recording_API>
    @see <https://www.w3.org/TR/mediastream-recording/> *)

open Js

(** The event delivered to [dataavailable] listeners. *)
class type blobEvent = object
  inherit Dom_html.event

  method data : File.blob t readonly_prop

  method timecode : number_t optdef readonly_prop
end

(** The event delivered to [error] listeners; [error] is the [DOMException]
    that ended the recording. *)
class type mediaRecorderErrorEvent = object
  inherit Dom_html.event

  method error : Js.error t readonly_prop
end

(** Options for the {!mediaRecorder_withOptions} constructor. All fields are
    optional; create an empty record with {!empty_recorder_options}. *)
class type mediaRecorderOptions = object
  method mimeType : js_string t writeonly_prop
  (** E.g. ["audio/webm"] or ["video/webm;codecs=vp9"]; probe support with
      {!is_type_supported}. *)

  method audioBitsPerSecond : int writeonly_prop

  method videoBitsPerSecond : int writeonly_prop

  method bitsPerSecond : int writeonly_prop
end

val empty_recorder_options : unit -> mediaRecorderOptions t

class type mediaRecorder = object ('self)
  inherit Dom_html.eventTarget

  method stream : MediaCapture.mediaStream t readonly_prop

  method mimeType : js_string t readonly_prop

  method state : js_string t readonly_prop
  (** One of ["inactive"], ["recording"] or ["paused"]. *)

  method audioBitsPerSecond : int readonly_prop
  (** The bit rate actually in use; it can differ from the one requested in
      the constructor options. *)

  method videoBitsPerSecond : int readonly_prop

  method audioBitrateMode : js_string t readonly_prop
  (** Either ["constant"] or ["variable"]. *)

  method start : unit meth

  method start_withTimeslice : int -> unit meth
  (** [r##start_withTimeslice ms] delivers a [dataavailable] chunk every [ms]
      milliseconds. *)

  method stop : unit meth
  (** Fires a final [dataavailable] with the remaining data, then [stop]. *)

  method pause : unit meth

  method resume : unit meth

  method requestData : unit meth
  (** Delivers the data recorded so far as a [dataavailable] event without
      stopping. *)

  method ondataavailable : ('self t, blobEvent t) Dom.event_listener writeonly_prop

  method onstart : ('self t, Dom_html.event t) Dom.event_listener writeonly_prop

  method onstop : ('self t, Dom_html.event t) Dom.event_listener writeonly_prop

  method onpause : ('self t, Dom_html.event t) Dom.event_listener writeonly_prop

  method onresume : ('self t, Dom_html.event t) Dom.event_listener writeonly_prop

  method onerror : ('self t, mediaRecorderErrorEvent t) Dom.event_listener writeonly_prop
end

val mediaRecorder : (MediaCapture.mediaStream t -> mediaRecorder t) constr
(** [new%js mediaRecorder stream] records [stream] with the user agent's
    default container and codecs. *)

val mediaRecorder_withOptions :
  (MediaCapture.mediaStream t -> mediaRecorderOptions t -> mediaRecorder t) constr

val is_type_supported : string -> bool
(** Whether the given MIME type can be recorded; [false] when the
    [MediaRecorder] global itself is missing. *)

val is_supported : unit -> bool
(** Whether the [MediaRecorder] global is available in the current
    environment. *)
