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

class type audioParam = object ('self)
  method value : Js.number_t Js.prop

  method defaultValue : Js.number_t Js.readonly_prop

  method minValue : Js.number_t Js.readonly_prop

  method maxValue : Js.number_t Js.readonly_prop

  method automationRate : Js.js_string Js.t Js.prop

  method setValueAtTime : Js.number_t -> Js.number_t -> 'self Js.t Js.meth

  method linearRampToValueAtTime : Js.number_t -> Js.number_t -> 'self Js.t Js.meth

  method exponentialRampToValueAtTime : Js.number_t -> Js.number_t -> 'self Js.t Js.meth

  method setTargetAtTime : Js.number_t -> Js.number_t -> Js.number_t -> 'self Js.t Js.meth

  method setValueCurveAtTime :
    Typed_array.float32Array Js.t -> Js.number_t -> Js.number_t -> 'self Js.t Js.meth

  method cancelScheduledValues : Js.number_t -> 'self Js.t Js.meth

  method cancelAndHoldAtTime : Js.number_t -> 'self Js.t Js.meth
end

class type audioBuffer = object
  method sampleRate : Js.number_t Js.readonly_prop

  method length : int Js.readonly_prop

  method duration : Js.number_t Js.readonly_prop

  method numberOfChannels : int Js.readonly_prop

  method getChannelData : int -> Typed_array.float32Array Js.t Js.meth

  method copyFromChannel : Typed_array.float32Array Js.t -> int -> unit Js.meth

  method copyFromChannel_withOffset :
    Typed_array.float32Array Js.t -> int -> int -> unit Js.meth

  method copyToChannel : Typed_array.float32Array Js.t -> int -> unit Js.meth

  method copyToChannel_withOffset :
    Typed_array.float32Array Js.t -> int -> int -> unit Js.meth
end

class type audioListener = object
  method positionX : audioParam Js.t Js.readonly_prop

  method positionY : audioParam Js.t Js.readonly_prop

  method positionZ : audioParam Js.t Js.readonly_prop

  method forwardX : audioParam Js.t Js.readonly_prop

  method forwardY : audioParam Js.t Js.readonly_prop

  method forwardZ : audioParam Js.t Js.readonly_prop

  method upX : audioParam Js.t Js.readonly_prop

  method upY : audioParam Js.t Js.readonly_prop

  method upZ : audioParam Js.t Js.readonly_prop

  method setPosition : Js.number_t -> Js.number_t -> Js.number_t -> unit Js.meth

  method setOrientation :
       Js.number_t
    -> Js.number_t
    -> Js.number_t
    -> Js.number_t
    -> Js.number_t
    -> Js.number_t
    -> unit Js.meth
end

type periodicWave

class type audioNode = object
  inherit Dom_html.eventTarget

  method context : baseAudioContext Js.t Js.readonly_prop

  method numberOfInputs : int Js.readonly_prop

  method numberOfOutputs : int Js.readonly_prop

  method channelCount : int Js.prop

  method channelCountMode : Js.js_string Js.t Js.prop

  method channelInterpretation : Js.js_string Js.t Js.prop

  method connect : audioNode Js.t -> audioNode Js.t Js.meth

  method connect_param : audioParam Js.t -> unit Js.meth

  method disconnect : unit Js.meth

  method disconnect_node : audioNode Js.t -> unit Js.meth

  method disconnect_param : audioParam Js.t -> unit Js.meth
end

and audioScheduledSourceNode = object ('self)
  inherit audioNode

  method start : unit Js.meth

  method start_atTime : Js.number_t -> unit Js.meth

  method stop : unit Js.meth

  method stop_atTime : Js.number_t -> unit Js.meth

  method onended : ('self Js.t, Dom_html.event Js.t) Dom.event_listener Js.writeonly_prop
end

and analyserNode = object
  inherit audioNode

  method fftSize : int Js.prop

  method frequencyBinCount : int Js.readonly_prop

  method minDecibels : Js.number_t Js.prop

  method maxDecibels : Js.number_t Js.prop

  method smoothingTimeConstant : Js.number_t Js.prop

  method getFloatFrequencyData : Typed_array.float32Array Js.t -> unit Js.meth

  method getByteFrequencyData : Typed_array.uint8Array Js.t -> unit Js.meth

  method getFloatTimeDomainData : Typed_array.float32Array Js.t -> unit Js.meth

  method getByteTimeDomainData : Typed_array.uint8Array Js.t -> unit Js.meth
end

and audioBufferSourceNode = object
  inherit audioScheduledSourceNode

  method buffer : audioBuffer Js.t Js.opt Js.prop

  method playbackRate : audioParam Js.t Js.readonly_prop

  method detune : audioParam Js.t Js.readonly_prop

  method loop : bool Js.t Js.prop

  method loopStart : Js.number_t Js.prop

  method loopEnd : Js.number_t Js.prop

  method start_withOffset : Js.number_t -> Js.number_t -> unit Js.meth

  method start_withOffsetAndDuration :
    Js.number_t -> Js.number_t -> Js.number_t -> unit Js.meth
end

and biquadFilterNode = object
  inherit audioNode

  method _type : Js.js_string Js.t Js.prop

  method frequency : audioParam Js.t Js.readonly_prop

  method detune : audioParam Js.t Js.readonly_prop

  method _Q : audioParam Js.t Js.readonly_prop

  method gain : audioParam Js.t Js.readonly_prop

  method getFrequencyResponse :
       Typed_array.float32Array Js.t
    -> Typed_array.float32Array Js.t
    -> Typed_array.float32Array Js.t
    -> unit Js.meth
end

and channelMergerNode = object
  inherit audioNode
end

and channelSplitterNode = object
  inherit audioNode
end

and constantSourceNode = object
  inherit audioScheduledSourceNode

  method offset : audioParam Js.t Js.readonly_prop
end

and convolverNode = object
  inherit audioNode

  method buffer : audioBuffer Js.t Js.opt Js.prop

  method normalize : bool Js.t Js.prop
end

and delayNode = object
  inherit audioNode

  method delayTime : audioParam Js.t Js.readonly_prop
end

and dynamicsCompressorNode = object
  inherit audioNode

  method threshold : audioParam Js.t Js.readonly_prop

  method knee : audioParam Js.t Js.readonly_prop

  method ratio : audioParam Js.t Js.readonly_prop

  method attack : audioParam Js.t Js.readonly_prop

  method release : audioParam Js.t Js.readonly_prop

  method reduction : Js.number_t Js.readonly_prop
end

and gainNode = object
  inherit audioNode

  method gain : audioParam Js.t Js.readonly_prop
end

and iirFilterNode = object
  inherit audioNode

  method getFrequencyResponse :
       Typed_array.float32Array Js.t
    -> Typed_array.float32Array Js.t
    -> Typed_array.float32Array Js.t
    -> unit Js.meth
end

and oscillatorNode = object
  inherit audioScheduledSourceNode

  method _type : Js.js_string Js.t Js.prop

  method frequency : audioParam Js.t Js.readonly_prop

  method detune : audioParam Js.t Js.readonly_prop

  method setPeriodicWave : periodicWave Js.t -> unit Js.meth
end

and pannerNode = object
  inherit audioNode

  method panningModel : Js.js_string Js.t Js.prop

  method distanceModel : Js.js_string Js.t Js.prop

  method positionX : audioParam Js.t Js.readonly_prop

  method positionY : audioParam Js.t Js.readonly_prop

  method positionZ : audioParam Js.t Js.readonly_prop

  method orientationX : audioParam Js.t Js.readonly_prop

  method orientationY : audioParam Js.t Js.readonly_prop

  method orientationZ : audioParam Js.t Js.readonly_prop

  method refDistance : Js.number_t Js.prop

  method maxDistance : Js.number_t Js.prop

  method rolloffFactor : Js.number_t Js.prop

  method coneInnerAngle : Js.number_t Js.prop

  method coneOuterAngle : Js.number_t Js.prop

  method coneOuterGain : Js.number_t Js.prop
end

and stereoPannerNode = object
  inherit audioNode

  method pan : audioParam Js.t Js.readonly_prop
end

and waveShaperNode = object
  inherit audioNode

  method curve : Typed_array.float32Array Js.t Js.opt Js.prop

  method oversample : Js.js_string Js.t Js.prop
end

and mediaElementAudioSourceNode = object
  inherit audioNode

  method mediaElement : Dom_html.mediaElement Js.t Js.readonly_prop
end

and mediaStreamAudioSourceNode = object
  inherit audioNode

  method mediaStream : MediaCapture.mediaStream Js.t Js.readonly_prop
end

and mediaStreamAudioDestinationNode = object
  inherit audioNode

  method stream : MediaCapture.mediaStream Js.t Js.readonly_prop
end

and audioDestinationNode = object
  inherit audioNode

  method maxChannelCount : int Js.readonly_prop
end

and baseAudioContext = object ('self)
  inherit Dom_html.eventTarget

  method destination : audioDestinationNode Js.t Js.readonly_prop

  method sampleRate : Js.number_t Js.readonly_prop

  method currentTime : Js.number_t Js.readonly_prop

  method listener : audioListener Js.t Js.readonly_prop

  method state : Js.js_string Js.t Js.readonly_prop

  method onstatechange :
    ('self Js.t, Dom_html.event Js.t) Dom.event_listener Js.writeonly_prop

  method createAnalyser : analyserNode Js.t Js.meth

  method createBiquadFilter : biquadFilterNode Js.t Js.meth

  method createBuffer : int -> int -> Js.number_t -> audioBuffer Js.t Js.meth

  method createBufferSource : audioBufferSourceNode Js.t Js.meth

  method createChannelMerger : channelMergerNode Js.t Js.meth

  method createChannelMerger_withCount : int -> channelMergerNode Js.t Js.meth

  method createChannelSplitter : channelSplitterNode Js.t Js.meth

  method createChannelSplitter_withCount : int -> channelSplitterNode Js.t Js.meth

  method createConstantSource : constantSourceNode Js.t Js.meth

  method createConvolver : convolverNode Js.t Js.meth

  method createDelay : delayNode Js.t Js.meth

  method createDelay_withMaxDelay : Js.number_t -> delayNode Js.t Js.meth

  method createDynamicsCompressor : dynamicsCompressorNode Js.t Js.meth

  method createGain : gainNode Js.t Js.meth

  method createIIRFilter :
       Js.number_t Js.js_array Js.t
    -> Js.number_t Js.js_array Js.t
    -> iirFilterNode Js.t Js.meth

  method createOscillator : oscillatorNode Js.t Js.meth

  method createPanner : pannerNode Js.t Js.meth

  method createPeriodicWave :
       Typed_array.float32Array Js.t
    -> Typed_array.float32Array Js.t
    -> periodicWave Js.t Js.meth

  method createStereoPanner : stereoPannerNode Js.t Js.meth

  method createWaveShaper : waveShaperNode Js.t Js.meth

  method decodeAudioData :
    Typed_array.arrayBuffer Js.t -> audioBuffer Js.t Promise.t Js.meth

  method resume : unit Promise.t Js.meth
end

and audioContext = object
  inherit baseAudioContext

  method baseLatency : Js.number_t Js.readonly_prop

  method outputLatency : Js.number_t Js.readonly_prop

  method suspend : unit Promise.t Js.meth

  method close : unit Promise.t Js.meth

  method createMediaElementSource :
    Dom_html.mediaElement Js.t -> mediaElementAudioSourceNode Js.t Js.meth

  method createMediaStreamSource :
    MediaCapture.mediaStream Js.t -> mediaStreamAudioSourceNode Js.t Js.meth

  method createMediaStreamDestination : mediaStreamAudioDestinationNode Js.t Js.meth
end

and offlineAudioCompletionEvent = object
  inherit Dom_html.event

  method renderedBuffer : audioBuffer Js.t Js.readonly_prop
end

and offlineAudioContext = object ('self)
  inherit baseAudioContext

  method length : int Js.readonly_prop

  method startRendering : audioBuffer Js.t Promise.t Js.meth

  method suspend_atTime : Js.number_t -> unit Promise.t Js.meth

  method oncomplete :
    ('self Js.t, offlineAudioCompletionEvent Js.t) Dom.event_listener Js.writeonly_prop
end

class type audioContextOptions = object
  method latencyHint : Js.js_string Js.t Js.writeonly_prop

  method latencyHint_seconds : Js.number_t Js.writeonly_prop

  method sampleRate : Js.number_t Js.writeonly_prop
end

let empty_audio_context_options () : audioContextOptions Js.t = Js.Unsafe.obj [||]

let audioContext : audioContext Js.t Js.constr = Js.Unsafe.global##._AudioContext

let audioContext_withOptions : (audioContextOptions Js.t -> audioContext Js.t) Js.constr =
  Js.Unsafe.global##._AudioContext

let offlineAudioContext :
    (int -> int -> Js.number_t -> offlineAudioContext Js.t) Js.constr =
  Js.Unsafe.global##._OfflineAudioContext

let is_supported () = Js.Optdef.test Js.Unsafe.global##._AudioContext
