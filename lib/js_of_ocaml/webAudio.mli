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

(** Web Audio API.

    Audio processing as a graph of {!class-type:audioNode}s inside an
    {!class-type:audioContext}: source nodes (buffers, oscillators, media
    elements, media streams) are connected through processing nodes (gain,
    filters, effects, panning, analysis) to the context's [destination].

    Typical usage:
    {[
      let ctx = new%js WebAudio.audioContext in
      let osc = ctx##createOscillator in
      let gain = ctx##createGain in
      gain##.gain##.value := Js.number_of_float 0.1;
      ignore (osc##connect (gain :> WebAudio.audioNode Js.t));
      ignore (gain##connect (ctx##.destination :> WebAudio.audioNode Js.t));
      osc##start
    ]}

    The [AudioWorklet] and deprecated [ScriptProcessorNode] interfaces are not
    bound.

    @see <https://developer.mozilla.org/en-US/docs/Web/API/Web_Audio_API>
    @see <https://webaudio.github.io/web-audio-api/> *)

open Js

(** {2 Parameters, buffers and helper objects} *)

(** An audio parameter: a value that can be set directly or scheduled to
    change over time. Times are in seconds on the owning context's
    [currentTime] clock. The scheduling methods return the parameter itself,
    allowing call chaining; [ignore] the result when not needed. *)
class type audioParam = object ('self)
  method value : number_t prop

  method defaultValue : number_t readonly_prop

  method minValue : number_t readonly_prop

  method maxValue : number_t readonly_prop

  method automationRate : js_string t prop
  (** Either ["a-rate"] or ["k-rate"]. *)

  method setValueAtTime : number_t -> number_t -> 'self t meth

  method linearRampToValueAtTime : number_t -> number_t -> 'self t meth

  method exponentialRampToValueAtTime : number_t -> number_t -> 'self t meth

  method setTargetAtTime : number_t -> number_t -> number_t -> 'self t meth
  (** [p##setTargetAtTime target startTime timeConstant]. *)

  method setValueCurveAtTime :
    Typed_array.float32Array t -> number_t -> number_t -> 'self t meth
  (** [p##setValueCurveAtTime values startTime duration]. *)

  method cancelScheduledValues : number_t -> 'self t meth

  method cancelAndHoldAtTime : number_t -> 'self t meth
end

(** A short audio asset resident in memory, one [Float32Array] of samples in
    [-1., 1.] per channel. Created with [createBuffer] or [decodeAudioData]. *)
class type audioBuffer = object
  method sampleRate : number_t readonly_prop

  method length : int readonly_prop
  (** In sample-frames. *)

  method duration : number_t readonly_prop
  (** In seconds. *)

  method numberOfChannels : int readonly_prop

  method getChannelData : int -> Typed_array.float32Array t meth

  method copyFromChannel : Typed_array.float32Array t -> int -> unit meth

  method copyFromChannel_withOffset :
    Typed_array.float32Array t -> int -> int -> unit meth

  method copyToChannel : Typed_array.float32Array t -> int -> unit meth

  method copyToChannel_withOffset : Typed_array.float32Array t -> int -> int -> unit meth
end

(** The position and orientation of the listener, for spatialization with
    {!class-type:pannerNode}. Accessed as [ctx##.listener]. *)
class type audioListener = object
  method positionX : audioParam t readonly_prop

  method positionY : audioParam t readonly_prop

  method positionZ : audioParam t readonly_prop

  method forwardX : audioParam t readonly_prop

  method forwardY : audioParam t readonly_prop

  method forwardZ : audioParam t readonly_prop

  method upX : audioParam t readonly_prop

  method upY : audioParam t readonly_prop

  method upZ : audioParam t readonly_prop

  method setPosition : number_t -> number_t -> number_t -> unit meth

  method setOrientation :
    number_t -> number_t -> number_t -> number_t -> number_t -> number_t -> unit meth
end

type periodicWave
(** A custom periodic waveform for {!class-type:oscillatorNode}, created with
    [createPeriodicWave]. *)

(** {2 The node graph} *)

(** A node in the audio graph. [connect] returns the destination node to allow
    call chaining; [ignore] the result when not needed. Connect to a subtype by
    coercing it: [node##connect (gain :> audioNode Js.t)]. *)
class type audioNode = object
  inherit Dom_html.eventTarget

  method context : baseAudioContext t readonly_prop

  method numberOfInputs : int readonly_prop

  method numberOfOutputs : int readonly_prop

  method channelCount : int prop

  method channelCountMode : js_string t prop
  (** One of ["max"], ["clamped-max"] or ["explicit"]. *)

  method channelInterpretation : js_string t prop
  (** Either ["speakers"] or ["discrete"]. *)

  method connect : audioNode t -> audioNode t meth

  method connect_param : audioParam t -> unit meth
  (** Connects the node's output to an {!class-type:audioParam}, modulating it
      with the audio signal. *)

  method disconnect : unit meth

  method disconnect_node : audioNode t -> unit meth

  method disconnect_param : audioParam t -> unit meth
end

(** A source node that can be scheduled to start and stop ([start]/[stop]
    without argument act immediately). A source can be started only once. *)
and audioScheduledSourceNode = object ('self)
  inherit audioNode

  method start : unit meth

  method start_atTime : number_t -> unit meth

  method stop : unit meth

  method stop_atTime : number_t -> unit meth

  method onended : ('self t, Dom_html.event t) Dom.event_listener writeonly_prop
end

and analyserNode = object
  inherit audioNode

  method fftSize : int prop

  method frequencyBinCount : int readonly_prop

  method minDecibels : number_t prop

  method maxDecibels : number_t prop

  method smoothingTimeConstant : number_t prop

  method getFloatFrequencyData : Typed_array.float32Array t -> unit meth

  method getByteFrequencyData : Typed_array.uint8Array t -> unit meth

  method getFloatTimeDomainData : Typed_array.float32Array t -> unit meth

  method getByteTimeDomainData : Typed_array.uint8Array t -> unit meth
end

and audioBufferSourceNode = object
  inherit audioScheduledSourceNode

  method buffer : audioBuffer t opt prop

  method playbackRate : audioParam t readonly_prop

  method detune : audioParam t readonly_prop

  method loop : bool t prop

  method loopStart : number_t prop

  method loopEnd : number_t prop

  method start_withOffset : number_t -> number_t -> unit meth
  (** [src##start_withOffset when offset]. *)

  method start_withOffsetAndDuration : number_t -> number_t -> number_t -> unit meth
end

and biquadFilterNode = object
  inherit audioNode

  method _type : js_string t prop
  (** One of ["lowpass"], ["highpass"], ["bandpass"], ["lowshelf"],
      ["highshelf"], ["peaking"], ["notch"] or ["allpass"]. *)

  method frequency : audioParam t readonly_prop

  method detune : audioParam t readonly_prop

  method _Q : audioParam t readonly_prop

  method gain : audioParam t readonly_prop

  method getFrequencyResponse :
       Typed_array.float32Array t
    -> Typed_array.float32Array t
    -> Typed_array.float32Array t
    -> unit meth
  (** [f##getFrequencyResponse frequencies magResponse phaseResponse]. *)
end

and channelMergerNode = object
  inherit audioNode
end

and channelSplitterNode = object
  inherit audioNode
end

and constantSourceNode = object
  inherit audioScheduledSourceNode

  method offset : audioParam t readonly_prop
end

and convolverNode = object
  inherit audioNode

  method buffer : audioBuffer t opt prop

  method normalize : bool t prop
end

and delayNode = object
  inherit audioNode

  method delayTime : audioParam t readonly_prop
end

and dynamicsCompressorNode = object
  inherit audioNode

  method threshold : audioParam t readonly_prop

  method knee : audioParam t readonly_prop

  method ratio : audioParam t readonly_prop

  method attack : audioParam t readonly_prop

  method release : audioParam t readonly_prop

  method reduction : number_t readonly_prop
end

and gainNode = object
  inherit audioNode

  method gain : audioParam t readonly_prop
end

and iirFilterNode = object
  inherit audioNode

  method getFrequencyResponse :
       Typed_array.float32Array t
    -> Typed_array.float32Array t
    -> Typed_array.float32Array t
    -> unit meth
end

and oscillatorNode = object
  inherit audioScheduledSourceNode

  method _type : js_string t prop
  (** One of ["sine"], ["square"], ["sawtooth"], ["triangle"] or ["custom"]
      (set implicitly by [setPeriodicWave]). *)

  method frequency : audioParam t readonly_prop

  method detune : audioParam t readonly_prop

  method setPeriodicWave : periodicWave t -> unit meth
end

and pannerNode = object
  inherit audioNode

  method panningModel : js_string t prop
  (** Either ["equalpower"] or ["HRTF"]. *)

  method distanceModel : js_string t prop
  (** One of ["linear"], ["inverse"] or ["exponential"]. *)

  method positionX : audioParam t readonly_prop

  method positionY : audioParam t readonly_prop

  method positionZ : audioParam t readonly_prop

  method orientationX : audioParam t readonly_prop

  method orientationY : audioParam t readonly_prop

  method orientationZ : audioParam t readonly_prop

  method refDistance : number_t prop

  method maxDistance : number_t prop

  method rolloffFactor : number_t prop

  method coneInnerAngle : number_t prop

  method coneOuterAngle : number_t prop

  method coneOuterGain : number_t prop
end

and stereoPannerNode = object
  inherit audioNode

  method pan : audioParam t readonly_prop
end

and waveShaperNode = object
  inherit audioNode

  method curve : Typed_array.float32Array t opt prop

  method oversample : js_string t prop
  (** One of ["none"], ["2x"] or ["4x"]. *)
end

and mediaElementAudioSourceNode = object
  inherit audioNode

  method mediaElement : Dom_html.mediaElement t readonly_prop
end

and mediaStreamAudioSourceNode = object
  inherit audioNode

  method mediaStream : MediaCapture.mediaStream t readonly_prop
end

(** An output node whose audio is exposed as a {!MediaCapture.mediaStream}
    (e.g. to feed a [MediaRecorder] or a WebRTC connection). *)
and mediaStreamAudioDestinationNode = object
  inherit audioNode

  method stream : MediaCapture.mediaStream t readonly_prop
end

and audioDestinationNode = object
  inherit audioNode

  method maxChannelCount : int readonly_prop
end

(** {2 Contexts} *)

and baseAudioContext = object ('self)
  inherit Dom_html.eventTarget

  method destination : audioDestinationNode t readonly_prop

  method sampleRate : number_t readonly_prop

  method currentTime : number_t readonly_prop

  method listener : audioListener t readonly_prop

  method state : js_string t readonly_prop
  (** One of ["suspended"], ["running"] or ["closed"]. *)

  method onstatechange : ('self t, Dom_html.event t) Dom.event_listener writeonly_prop

  method createAnalyser : analyserNode t meth

  method createBiquadFilter : biquadFilterNode t meth

  method createBuffer : int -> int -> number_t -> audioBuffer t meth
  (** [ctx##createBuffer numberOfChannels length sampleRate]. *)

  method createBufferSource : audioBufferSourceNode t meth

  method createChannelMerger : channelMergerNode t meth

  method createChannelMerger_withCount : int -> channelMergerNode t meth

  method createChannelSplitter : channelSplitterNode t meth

  method createChannelSplitter_withCount : int -> channelSplitterNode t meth

  method createConstantSource : constantSourceNode t meth

  method createConvolver : convolverNode t meth

  method createDelay : delayNode t meth

  method createDelay_withMaxDelay : number_t -> delayNode t meth

  method createDynamicsCompressor : dynamicsCompressorNode t meth

  method createGain : gainNode t meth

  method createIIRFilter :
    number_t js_array t -> number_t js_array t -> iirFilterNode t meth
  (** [ctx##createIIRFilter feedforward feedback]. *)

  method createOscillator : oscillatorNode t meth

  method createPanner : pannerNode t meth

  method createPeriodicWave :
    Typed_array.float32Array t -> Typed_array.float32Array t -> periodicWave t meth
  (** [ctx##createPeriodicWave real imag]. *)

  method createStereoPanner : stereoPannerNode t meth

  method createWaveShaper : waveShaperNode t meth

  method decodeAudioData : Typed_array.arrayBuffer t -> audioBuffer t Promise.t meth

  method resume : unit Promise.t meth
end

and audioContext = object
  inherit baseAudioContext

  method baseLatency : number_t readonly_prop

  method outputLatency : number_t readonly_prop

  method suspend : unit Promise.t meth

  method close : unit Promise.t meth

  method createMediaElementSource :
    Dom_html.mediaElement t -> mediaElementAudioSourceNode t meth

  method createMediaStreamSource :
    MediaCapture.mediaStream t -> mediaStreamAudioSourceNode t meth

  method createMediaStreamDestination : mediaStreamAudioDestinationNode t meth
end

(** The event delivered to [complete] listeners of an
    {!class-type:offlineAudioContext}. *)
and offlineAudioCompletionEvent = object
  inherit Dom_html.event

  method renderedBuffer : audioBuffer t readonly_prop
end

(** A context that renders its graph as fast as possible into an
    {!class-type:audioBuffer} instead of playing it. *)
and offlineAudioContext = object ('self)
  inherit baseAudioContext

  method length : int readonly_prop

  method startRendering : audioBuffer t Promise.t meth

  method suspend_atTime : number_t -> unit Promise.t meth

  method oncomplete :
    ('self t, offlineAudioCompletionEvent t) Dom.event_listener writeonly_prop
end

(** {2 Constructors} *)

(** Options for {!audioContext_withOptions}. All fields are optional; create
    an empty record with {!empty_audio_context_options}. *)
class type audioContextOptions = object
  method latencyHint : js_string t writeonly_prop
  (** One of ["balanced"], ["interactive"] (default) or ["playback"]; use
      [latencyHint_seconds] instead to request an explicit latency. *)

  method latencyHint_seconds : number_t writeonly_prop

  method sampleRate : number_t writeonly_prop
end

val empty_audio_context_options : unit -> audioContextOptions t

val audioContext : audioContext t constr

val audioContext_withOptions : (audioContextOptions t -> audioContext t) constr

val offlineAudioContext : (int -> int -> number_t -> offlineAudioContext t) constr
(** [new%js offlineAudioContext numberOfChannels length sampleRate]. *)

val is_supported : unit -> bool
(** Whether the [AudioContext] global is available in the current
    environment. *)
