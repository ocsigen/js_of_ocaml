(* Js_of_ocaml example
 * http://www.ocsigen.org/js_of_ocaml/
 * Copyright (C) 2026 Ocsigen team
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2 of the License, or
 * (at your option) any later version.
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

(* GPU particle system simulated entirely with WebGL2 transform feedback:
   positions and velocities live in GPU buffers; an update program integrates
   them with rasterization discarded, capturing its varyings into a second
   buffer (ping-pong), which a render program then draws as points. *)

open Js_of_ocaml
open Js_of_ocaml_lwt
open Lwt
open Js

(* buffers are allocated for [max_particles]; the slider chooses how many are
   simulated and drawn *)
let max_particles = 200_000

let default_particles = 50_000

let error f =
  Printf.ksprintf
    (fun s ->
      Console.console##error (Js.string s);
      failwith s)
    f

let debug f = Printf.ksprintf (fun s -> Console.console##log (Js.string s)) f

let alert f =
  Printf.ksprintf
    (fun s ->
      Dom_html.window##alert (Js.string s);
      failwith s)
    f

let check_error gl = if gl##getError <> gl##._NO_ERROR_ then error "WebGL error"

let init_canvas canvas_id =
  let canvas =
    Opt.get
      (Opt.bind
         (Dom_html.document##getElementById (string canvas_id))
         Dom_html.CoerceTo.canvas)
      (fun () -> error "can't find canvas element %s" canvas_id)
  in
  let gl =
    Opt.get
      (try WebGL2.getContext canvas with _ -> null)
      (fun () -> alert "can't initialise webgl2 context")
  in
  canvas, gl

let load_shader (gl : WebGL2.renderingContext t) shader text =
  gl##shaderSource shader text;
  gl##compileShader shader;
  if not (to_bool (gl##getShaderParameter shader gl##._COMPILE_STATUS_))
  then
    error
      "An error occurred compiling the shaders: \n%s\n%s"
      (to_string text)
      (to_string (gl##getShaderInfoLog shader))

let create_program
    (gl : WebGL2.renderingContext t)
    ?(pre_link = fun _ -> ())
    vert_src
    frag_src =
  let vertexShader = gl##createShader gl##._VERTEX_SHADER_ in
  let fragmentShader = gl##createShader gl##._FRAGMENT_SHADER_ in
  load_shader gl vertexShader vert_src;
  load_shader gl fragmentShader frag_src;
  let prog = gl##createProgram in
  gl##attachShader prog vertexShader;
  gl##attachShader prog fragmentShader;
  pre_link prog;
  gl##linkProgram prog;
  if not (to_bool (gl##getProgramParameter prog gl##._LINK_STATUS_))
  then error "Unable to link the shader program.";
  prog

let get_source src_id =
  let script =
    Opt.get
      (Opt.bind
         (Dom_html.document##getElementById (string src_id))
         Dom_html.CoerceTo.script)
      (fun () -> error "can't find script element %s" src_id)
  in
  (* GLSL ES 3.00 requires [#version] to be on the very first line, so strip
     the newline following the opening script tag *)
  string (String.trim (to_string script##.text))

(* wire a range input to a label; [on_input] receives the slider value and
   returns the label text *)
let setup_slider slider_id label_id on_input =
  Opt.iter
    (Opt.bind
       (Dom_html.document##getElementById (string slider_id))
       Dom_html.CoerceTo.input)
    (fun slider ->
      let label_text = Dom_html.document##createTextNode (Js.string "") in
      Opt.iter
        (Opt.bind
           (Dom_html.document##getElementById (string label_id))
           Dom_html.CoerceTo.element)
        (fun span -> Dom.appendChild span label_text);
      let refresh () =
        label_text##.data := string (on_input (to_string slider##.value))
      in
      refresh ();
      slider##.oninput :=
        Dom_html.handler (fun _ ->
            refresh ();
            Js._true))

let start () =
  let fps_text = Dom_html.document##createTextNode (Js.string "-") in
  Opt.iter
    (Opt.bind
       (Dom_html.document##getElementById (string "fps"))
       Dom_html.CoerceTo.element)
    (fun span -> Dom.appendChild span fps_text);
  debug "init canvas";
  let canvas, gl = init_canvas "canvas" in
  debug "create programs";
  let update_prog =
    (* the transform feedback layout must be declared before linking *)
    create_program
      gl
      ~pre_link:(fun prog ->
        let varyings = array [| string "v_position"; string "v_velocity" |] in
        gl##transformFeedbackVaryings prog varyings gl##._INTERLEAVED_ATTRIBS_)
      (get_source "update-vertex-shader")
      (get_source "update-fragment-shader")
  in
  let render_prog =
    create_program
      gl
      (get_source "render-vertex-shader")
      (get_source "render-fragment-shader")
  in
  (* interleaved particle state: position.xy, velocity.xy *)
  let initial_data =
    let data = new%js Typed_array.float32Array (max_particles * 4) in
    for i = 0 to max_particles - 1 do
      let x = Random.float 1.8 -. 0.9 and y = Random.float 1.8 -. 0.9 in
      (* mostly tangential initial velocity, for orbital motion *)
      let s = 0.4 +. Random.float 0.3 in
      Typed_array.set data ((i * 4) + 0) (Js.float x);
      Typed_array.set data ((i * 4) + 1) (Js.float y);
      Typed_array.set data ((i * 4) + 2) (Js.float ((-.y *. s) +. Random.float 0.05));
      Typed_array.set data ((i * 4) + 3) (Js.float ((x *. s) +. Random.float 0.05))
    done;
    data
  in
  let make_buffer () =
    let buf = gl##createBuffer in
    gl##bindBuffer gl##._ARRAY_BUFFER_ buf;
    gl##bufferData gl##._ARRAY_BUFFER_ initial_data gl##._DYNAMIC_COPY_;
    buf
  in
  (* attribute locations are pinned with [layout(location = ...)] in the
     shaders, so the same VAO layout works for both programs *)
  let stride = 4 * 4 in
  let make_vao buf =
    let vao = gl##createVertexArray in
    gl##bindVertexArray (Opt.return vao);
    gl##bindBuffer gl##._ARRAY_BUFFER_ buf;
    gl##enableVertexAttribArray 0;
    gl##vertexAttribPointer 0 2 gl##._FLOAT _false stride 0;
    gl##enableVertexAttribArray 1;
    gl##vertexAttribPointer 1 2 gl##._FLOAT _false stride 8;
    gl##bindVertexArray Opt.empty;
    vao
  in
  let make_tf buf =
    let tf = gl##createTransformFeedback in
    gl##bindTransformFeedback gl##._TRANSFORM_FEEDBACK_ (Opt.return tf);
    gl##bindBufferBase gl##._TRANSFORM_FEEDBACK_BUFFER_ 0 buf;
    gl##bindTransformFeedback gl##._TRANSFORM_FEEDBACK_ Opt.empty;
    tf
  in
  let buf_a = make_buffer () in
  let buf_b = make_buffer () in
  let vao_a = make_vao buf_a in
  let vao_b = make_vao buf_b in
  let tf_a = make_tf buf_a in
  let tf_b = make_tf buf_b in
  (* a buffer being written through transform feedback must not be left bound
     to the generic ARRAY_BUFFER binding point (which is not part of the VAO
     state) *)
  gl##bindBuffer_ gl##._ARRAY_BUFFER_ Opt.empty;
  let dt_loc = gl##getUniformLocation update_prog (string "u_dt") in
  let attractor_loc = gl##getUniformLocation update_prog (string "u_attractor") in
  gl##enable gl##._BLEND;
  gl##blendFunc gl##._SRC_ALPHA_ gl##._ONE;
  gl##clearColor (Js.float 0.02) (Js.float 0.02) (Js.float 0.05) (Js.float 1.);
  check_error gl;
  debug "ready";
  (* the sliders choose how many particles take part and how fast time runs *)
  let nparticles = ref default_particles in
  setup_slider "count" "count-label" (fun v ->
      (match int_of_string_opt v with
      | Some n -> nparticles := max 1 (min max_particles n)
      | None -> ());
      Printf.sprintf "%d" !nparticles);
  let speed = ref 1.0 in
  setup_slider "speed" "speed-label" (fun v ->
      (match float_of_string_opt v with
      | Some s -> speed := max 0. (min 4. s)
      | None -> ());
      Printf.sprintf "%.2f" !speed);
  (* the attractor follows the mouse, or orbits while unattended *)
  let attractor = ref None in
  canvas##.onmousemove :=
    Dom_html.handler (fun (ev : Dom_html.mouseEvent t) ->
        let rect = canvas##getBoundingClientRect in
        let x =
          (Js.to_float ev##.clientX -. Js.to_float rect##.left)
          /. float_of_int canvas##.width
        in
        let y =
          (Js.to_float ev##.clientY -. Js.to_float rect##.top)
          /. float_of_int canvas##.height
        in
        attractor := Some ((x *. 2.) -. 1., 1. -. (y *. 2.));
        Js._true);
  canvas##.onmouseout :=
    Dom_html.handler (fun _ ->
        attractor := None;
        Js._true);
  let get_time () = Js.to_float (new%js date_now)##getTime in
  let last_draw = ref (get_time ()) in
  let draw_times = Queue.create () in
  let flip = ref true in
  let rec f () =
    let read_vao, write_tf, draw_vao =
      if !flip then vao_a, tf_b, vao_b else vao_b, tf_a, vao_a
    in
    flip := not !flip;
    let t = get_time () /. 1000. in
    let ax, ay =
      match !attractor with
      | Some p -> p
      | None -> 0.6 *. cos (0.7 *. t), 0.6 *. sin (1.1 *. t)
    in
    (* simulation pass: no fragments, varyings captured into the other buffer *)
    gl##useProgram update_prog;
    gl##uniform1f dt_loc (Js.number_of_float (0.016 *. !speed));
    gl##uniform2f attractor_loc (Js.number_of_float ax) (Js.number_of_float ay);
    gl##bindVertexArray (Opt.return read_vao);
    gl##bindTransformFeedback gl##._TRANSFORM_FEEDBACK_ (Opt.return write_tf);
    gl##enable gl##._RASTERIZER_DISCARD_;
    gl##beginTransformFeedback gl##._POINTS;
    gl##drawArrays gl##._POINTS 0 !nparticles;
    gl##endTransformFeedback;
    gl##disable gl##._RASTERIZER_DISCARD_;
    gl##bindTransformFeedback gl##._TRANSFORM_FEEDBACK_ Opt.empty;
    (* render pass: draw the freshly written buffer *)
    gl##useProgram render_prog;
    gl##clear gl##._COLOR_BUFFER_BIT_;
    gl##bindVertexArray (Opt.return draw_vao);
    gl##drawArrays gl##._POINTS 0 !nparticles;
    check_error gl;
    let now = get_time () in
    Queue.push (now -. !last_draw) draw_times;
    last_draw := now;
    if Queue.length draw_times > 50 then ignore (Queue.pop draw_times);
    let fps =
      1.
      /. Queue.fold ( +. ) 0. draw_times
      *. float_of_int (Queue.length draw_times)
      *. 1000.
    in
    fps_text##.data := string (Printf.sprintf "%.1f" fps);
    Lwt_js.sleep 0.016 >>= f
  in
  f ()

let () =
  ignore
    (catch
       (fun () -> start ())
       (fun exn -> error "uncaught exception: %s" (Printexc.to_string exn)))
