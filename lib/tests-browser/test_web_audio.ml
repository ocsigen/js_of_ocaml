(* Js_of_ocaml tests
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

(* Web Audio / Media Capture / mediaElement demo.

   The automatic part renders an oscillator through a gain with an
   OfflineAudioContext, routes a real-time context into a MediaStream
   (exercising the MediaCapture types without any permission prompt) and
   checks the new mediaElement members. The two buttons need a user gesture:
   one plays a short beep, the other asks for microphone access and prints
   the captured track's settings. *)

open Js_of_ocaml

let pass_count = ref 0

let fail_count = ref 0

let log_row status label detail =
  let el = Dom_html.document##getElementById (Js.string "log") in
  Js.Opt.iter el (fun el ->
      let row = Dom_html.document##createElement (Js.string "div") in
      row##.className := Js.string ("row " ^ status);
      let mark = Dom_html.document##createElement (Js.string "span") in
      mark##.className := Js.string ("mark " ^ status);
      mark##.textContent :=
        Js.some
          (Js.string
             (if String.equal status "pass" then "\xe2\x9c\x93" else "\xe2\x9c\x97"));
      let txt = Dom_html.document##createElement (Js.string "span") in
      txt##.className := Js.string "label";
      txt##.textContent := Js.some (Js.string label);
      let det = Dom_html.document##createElement (Js.string "span") in
      det##.className := Js.string "detail";
      det##.textContent := Js.some (Js.string detail);
      Dom.appendChild row mark;
      Dom.appendChild row txt;
      Dom.appendChild row det;
      Dom.appendChild el row)

let check label cond detail =
  if cond
  then (
    incr pass_count;
    log_row "pass" label detail)
  else (
    incr fail_count;
    log_row "fail" label detail)

let info label detail = log_row "pass" label detail

let summarize () =
  let el = Dom_html.document##getElementById (Js.string "summary") in
  Js.Opt.iter el (fun el ->
      let status = if !fail_count = 0 then "pass" else "fail" in
      el##.className := Js.string ("summary " ^ status);
      el##.textContent :=
        Js.some
          (Js.string (Printf.sprintf "%d passed, %d failed" !pass_count !fail_count)))

let ( >>= ) p f = Promise.then_ f p

let return = Promise.resolve

let stringify any = Js.to_string (Js.Unsafe.fun_call Js.Unsafe.global##._String [| any |])

let num = Js.number_of_float

let on_click id f =
  Js.Opt.iter
    (Dom_html.document##getElementById (Js.string id))
    (fun el ->
      el##.onclick :=
        Dom_html.handler (fun _ ->
            f ();
            Js._false))

(* Render one second of a 440 Hz oscillator through a gain of 0.5 and check
   the resulting buffer. *)
let test_offline_render () =
  let ctx = new%js WebAudio.offlineAudioContext 1 44100 (num 44100.) in
  let osc = ctx##createOscillator in
  osc##.frequency##.value := num 440.;
  let gain = ctx##createGain in
  let param = gain##.gain##setValueAtTime (num 0.5) (num 0.) in
  check "AudioParam scheduling returns the param" (param == gain##.gain) "";
  ignore (osc##connect (gain :> WebAudio.audioNode Js.t) : WebAudio.audioNode Js.t);
  ignore
    (gain##connect (ctx##.destination :> WebAudio.audioNode Js.t)
      : WebAudio.audioNode Js.t);
  osc##start;
  ctx##startRendering
  >>= fun buffer ->
  check
    "rendered buffer has the requested shape"
    (buffer##.length = 44100 && buffer##.numberOfChannels = 1)
    (Printf.sprintf
       "length=%d channels=%d duration=%.2fs"
       buffer##.length
       buffer##.numberOfChannels
       (Js.float_of_number buffer##.duration));
  let data = buffer##getChannelData 0 in
  let peak = ref 0. in
  for i = 0 to buffer##.length - 1 do
    let v = abs_float (Js.float_of_number (Typed_array.unsafe_get data i)) in
    if v > !peak then peak := v
  done;
  check
    "oscillator went through the 0.5 gain"
    (!peak > 0.45 && !peak < 0.55)
    (Printf.sprintf "peak=%.3f" !peak);
  return ()

(* Route a real-time context into a MediaStream: this exercises the
   MediaCapture types without any permission prompt. *)
let test_stream_destination (ctx : WebAudio.audioContext Js.t) =
  let dest = ctx##createMediaStreamDestination in
  let stream = dest##.stream in
  check "destination stream is active" (Js.to_bool stream##.active) "";
  let tracks = stream##getAudioTracks in
  check "stream has one audio track" (tracks##.length = 1) "";
  Js.Optdef.iter (Js.array_get tracks 0) (fun track ->
      check
        "track kind and state"
        (Js.to_string track##.kind = "audio" && Js.to_string track##.readyState = "live")
        (Js.to_string track##.kind ^ ", " ^ Js.to_string track##.readyState);
      let copy = track##clone in
      check
        "cloned track gets its own id"
        (Js.to_string copy##.id <> Js.to_string track##.id)
        "";
      copy##.enabled := Js._false;
      check "track can be disabled" (not (Js.to_bool copy##.enabled)) "";
      let bundle = new%js MediaCapture.mediaStream_fromTracks (Js.array [| copy |]) in
      check "MediaStream can be built from tracks" (bundle##getTracks##.length = 1) "";
      copy##stop;
      check
        "stopped track reports ended"
        (Js.to_string copy##.readyState = "ended")
        (Js.to_string copy##.readyState));
  return stream

let test_media_element (stream : MediaCapture.mediaStream Js.t) =
  let el = Dom_html.createAudio Dom_html.document in
  check "mediaElement error is initially null" (not (Js.Opt.test el##.error)) "";
  (* Rate properties are checked before [srcObject] is set: with a MediaStream
     source the element pins the playback rate to 1. *)
  el##.defaultPlaybackRate := num 1.5;
  check
    "defaultPlaybackRate is writable"
    (Js.float_of_number el##.defaultPlaybackRate = 1.5)
    "";
  el##.defaultPlaybackRate := num 1.;
  el##.preservesPitch := Js._false;
  check "preservesPitch is writable" (not (Js.to_bool el##.preservesPitch)) "";
  el##.srcObject := Js.some (Js.Unsafe.inject stream);
  check
    "srcObject round-trips"
    (Js.Opt.case el##.srcObject (fun () -> false) (fun s -> s == Js.Unsafe.inject stream))
    "";
  let track = el##addTextTrack (Js.string "subtitles") in
  check
    "addTextTrack yields a subtitles track"
    (Js.to_string track##.kind = "subtitles")
    (Js.to_string track##.kind ^ ", mode=" ^ Js.to_string track##.mode);
  check "textTracks lists the new track" (el##.textTracks##.length = 1) "";
  info "canPlayType audio/wav" (Js.to_string (el##canPlayType (Js.string "audio/wav")));
  return ()

let test_devices () =
  if not (MediaCapture.is_supported ())
  then check "navigator.mediaDevices is available" false ""
  else begin
    let devices = MediaCapture.mediaDevices () in
    let supported = devices##getSupportedConstraints in
    check
      "echoCancellation is a supported constraint"
      (Js.Optdef.test supported##.echoCancellation)
      "";
    (* Logged when it resolves, without holding the summary back: some
       environments (e.g. headless browsers without audio devices) never
       settle this promise. *)
    let _ : unit Promise.t =
      devices##enumerateDevices
      >>= fun infos ->
      info "enumerateDevices" (Printf.sprintf "%d device(s)" infos##.length);
      return ()
    in
    ()
  end;
  return ()

let test_realtime () =
  let ctx = new%js WebAudio.audioContext in
  let state = Js.to_string ctx##.state in
  check
    "fresh context is suspended or running"
    (state = "suspended" || state = "running")
    state;
  test_stream_destination ctx
  >>= fun stream ->
  test_media_element stream
  >>= fun () ->
  ctx##close
  >>= fun () ->
  check "closed context reports closed" (Js.to_string ctx##.state = "closed") "";
  return ()

(* Needs a user gesture (autoplay policy): play a short beep. *)
let beep () =
  let ctx = new%js WebAudio.audioContext in
  let _ : unit Promise.t =
    ctx##resume
    >>= fun () ->
    let osc = ctx##createOscillator in
    let gain = ctx##createGain in
    gain##.gain##.value := num 0.1;
    ignore (osc##connect (gain :> WebAudio.audioNode Js.t) : WebAudio.audioNode Js.t);
    ignore
      (gain##connect (ctx##.destination :> WebAudio.audioNode Js.t)
        : WebAudio.audioNode Js.t);
    osc##start;
    osc##stop_atTime (Js.number_of_float (Js.float_of_number ctx##.currentTime +. 0.3));
    info "beep" "played 440 Hz for 0.3 s";
    return ()
  in
  ()

(* Needs a user gesture and permission: capture the microphone and print the
   track settings. *)
let capture_microphone () =
  let constraints = MediaCapture.empty_stream_constraints () in
  constraints##.audio := Js._true;
  let _ : unit Promise.t =
    Promise.catch
      (fun e -> return (info "getUserMedia" (stringify (Promise.error_to_any e))))
      ((MediaCapture.mediaDevices ())##getUserMedia constraints
      >>= fun stream ->
      let tracks = stream##getAudioTracks in
      Js.Optdef.iter (Js.array_get tracks 0) (fun track ->
          let settings = track##getSettings in
          let dims label v =
            Js.Optdef.case v (fun () -> "") (fun v -> Printf.sprintf " %s=%d" label v)
          in
          info
            "getUserMedia"
            (Printf.sprintf
               "label=%S%s%s"
               (Js.to_string track##.label)
               (dims "sampleRate" settings##.sampleRate)
               (dims "channelCount" settings##.channelCount));
          track##stop);
      return ())
  in
  ()

let run () =
  check "WebAudio.is_supported" (WebAudio.is_supported ()) "";
  let tests = test_offline_render () >>= test_realtime >>= test_devices in
  let _ : unit Promise.t =
    Promise.catch
      (fun e ->
        check
          "test suite ran to completion without an unexpected rejection"
          false
          (stringify (Promise.error_to_any e));
        return ())
      tests
    >>= fun () ->
    summarize ();
    return ()
  in
  on_click "beep" beep;
  on_click "mic" capture_microphone

let () = run ()
