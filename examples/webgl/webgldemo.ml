(* Js_of_ocaml example
 * http://www.ocsigen.org/js_of_ocaml/
 * Copyright (C) 2012 Pierre Chambart
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
open Js_of_ocaml
open Js_of_ocaml_lwt
open Lwt
open Js

let error f =
  Printf.ksprintf
    (fun s ->
      Firebug.console##error (Js.string s);
      failwith s)
    f

let debug f = Printf.ksprintf (fun s -> Firebug.console##log (Js.string s)) f

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
      (try WebGL.getContext canvas with _ -> null)
      (fun () -> alert "can't initialise webgl context")
  in
  canvas, gl

let load_shader (gl : WebGL.renderingContext t) shader text =
  gl##shaderSource shader text;
  gl##compileShader shader;
  if not (to_bool (gl##getShaderParameter shader gl##._COMPILE_STATUS_))
  then
    error
      "An error occurred compiling the shaders: \n%s\n%s"
      (to_string text)
      (to_string (gl##getShaderInfoLog shader))

let create_program (gl : WebGL.renderingContext t) vert_src frag_src =
  let vertexShader = gl##createShader gl##._VERTEX_SHADER_ in
  let fragmentShader = gl##createShader gl##._FRAGMENT_SHADER_ in
  load_shader gl vertexShader vert_src;
  load_shader gl fragmentShader frag_src;
  let prog = gl##createProgram in
  gl##attachShader prog vertexShader;
  gl##attachShader prog fragmentShader;
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
  script##.text

let float32array a =
  let array = new%js Typed_array.float32Array (Array.length a) in
  Array.iteri (fun i v -> Typed_array.set array i (Js.float v)) a;
  array

module Proj3D = struct
  type t = float array

  let scale x y z : t = [| x; 0.; 0.; 0.; 0.; y; 0.; 0.; 0.; 0.; z; 0.; 0.; 0.; 0.; 1. |]

  let translate x y z : t =
    [| 1.; 0.; 0.; 0.; 0.; 1.; 0.; 0.; 0.; 0.; 1.; 0.; x; y; z; 1. |]

  let rotate_x t : t =
    [| 1.; 0.; 0.; 0.; 0.; cos t; sin t; 0.; 0.; -.sin t; cos t; 0.; 0.; 0.; 0.; 1. |]

  let rotate_y t : t =
    [| cos t; 0.; -.sin t; 0.; 0.; 1.; 0.; 0.; sin t; 0.; cos t; 0.; 0.; 0.; 0.; 1. |]

  let c i j = (i * 4) + j

  let o i = i / 4, i mod 4

  let mult m1 m2 =
    let v p =
      let i, j = o p in
      (m1.(c i 0) *. m2.(c 0 j))
      +. (m1.(c i 1) *. m2.(c 1 j))
      +. (m1.(c i 2) *. m2.(c 2 j))
      +. (m1.(c i 3) *. m2.(c 3 j))
    in
    Array.init 16 v

  let array m = float32array m
end

type line =
  | V of (float * float * float)
  | VN of (float * float * float)
  | F of ((int * int) * (int * int) * (int * int))

let line_regexp = Regexp.regexp "(v|vn|f)\\ ([^\\ ]+)\\ ([^\\ ]+)\\ ([^\\ ]+)"

let couple_regexp = Regexp.regexp "([0-9]+)//([0-9]+)"

let read_coord_couple c =
  match Regexp.string_match couple_regexp c 0 with
  | None -> None
  | Some res -> (
      match List.map (Regexp.matched_group res) [ 1; 2 ] with
      | [ Some v; Some vn ] -> Some (int_of_string v, int_of_string vn)
      | _ -> None)

let read_line l =
  match Regexp.string_match line_regexp l 0 with
  | None -> None
  | Some res -> (
      match List.map (Regexp.matched_group res) [ 1; 2; 3; 4 ] with
      | [ Some "v"; Some x; Some y; Some z ] ->
          Some (V (float_of_string x, float_of_string y, float_of_string z))
      | [ Some "vn"; Some x; Some y; Some z ] ->
          Some (VN (float_of_string x, float_of_string y, float_of_string z))
      | [ Some "f"; Some x; Some y; Some z ] -> (
          match List.map read_coord_couple [ x; y; z ] with
          | [ Some x; Some y; Some z ] -> Some (F (x, y, z))
          | _ -> None)
      | _ -> None)

let concat a =
  let length = Array.fold_left (fun len l -> len + List.length l) 0 a in
  let next =
    let pos = ref (-1) in
    let l = ref [] in
    let rec aux _ =
      match !l with
      | t :: q ->
          l := q;
          t
      | [] ->
          incr pos;
          l := a.(!pos);
          aux 0
    in
    aux
  in
  Array.init length next

let make_model vertex norm face =
  let vertex' =
    Array.init (Array.length face) (fun i ->
        let (av, _an), (bv, _bn), (cv, _cn) = face.(i) in
        let a1, a2, a3 = vertex.(av - 1) in
        let b1, b2, b3 = vertex.(bv - 1) in
        let c1, c2, c3 = vertex.(cv - 1) in
        [ a1; a2; a3; b1; b2; b3; c1; c2; c3 ])
  in
  let norm' =
    Array.init (Array.length face) (fun i ->
        let (_av, an), (_bv, bn), (_cv, cn) = face.(i) in
        let a1, a2, a3 = norm.(an - 1) in
        let b1, b2, b3 = norm.(bn - 1) in
        let c1, c2, c3 = norm.(cn - 1) in
        [ a1; a2; a3; b1; b2; b3; c1; c2; c3 ])
  in
  let vertex = float32array (concat vertex') in
  let norm = float32array (concat norm') in
  vertex, norm

let read_model a =
  let vertex = ref [] in
  let norm = ref [] in
  let face = ref [] in
  Array.iter
    (fun s ->
      match read_line s with
      | None -> ()
      | Some (F (a, b, c)) -> face := (a, b, c) :: !face
      | Some (V (a, b, c)) -> vertex := (a, b, c) :: !vertex
      | Some (VN (a, b, c)) -> norm := (a, b, c) :: !norm)
    a;
  make_model
    (Array.of_list (List.rev !vertex))
    (Array.of_list (List.rev !norm))
    (Array.of_list (List.rev !face))

let http_get url =
  XmlHttpRequest.get url
  >>= fun r ->
  let cod = r.XmlHttpRequest.code in
  let msg = r.XmlHttpRequest.content in
  if cod = 0 || cod = 200 then Lwt.return msg else fst (Lwt.wait ())

let getfile f =
  try Lwt.return (Sys_js.read_file ~name:f) with Not_found -> http_get f >|= fun s -> s

let fetch_model s =
  getfile s
  >|= fun s ->
  let a = Regexp.split (Regexp.regexp "\n") s in
  read_model (Array.of_list a)

let pi = 4. *. atan 1.

let start (pos, norm) =
  let fps_text = Dom_html.document##createTextNode (Js.string "loading") in
  Opt.iter
    (Opt.bind
       (Dom_html.document##getElementById (string "fps"))
       Dom_html.CoerceTo.element)
    (fun span -> Dom.appendChild span fps_text);
  debug "init canvas";
  let _canvas, gl = init_canvas "canvas" in
  debug "create program";
  let prog =
    create_program gl (get_source "vertex-shader") (get_source "fragment-shader")
  in
  debug "use program";
  gl##useProgram prog;
  check_error gl;
  debug "program loaded";
  gl##enable gl##._DEPTH_TEST_;
  gl##depthFunc gl##._LESS;
  let proj_loc = gl##getUniformLocation prog (string "u_proj") in
  let lightPos_loc = gl##getUniformLocation prog (string "u_lightPos") in
  let ambientLight_loc = gl##getUniformLocation prog (string "u_ambientLight") in
  let lightPos = float32array [| 3.; 0.; -1. |] in
  let ambientLight = float32array [| 0.1; 0.1; 0.1 |] in
  gl##uniform3fv_typed lightPos_loc lightPos;
  gl##uniform3fv_typed ambientLight_loc ambientLight;
  let pos_attr = gl##getAttribLocation prog (string "a_position") in
  gl##enableVertexAttribArray pos_attr;
  let array_buffer = gl##createBuffer in
  gl##bindBuffer gl##._ARRAY_BUFFER_ array_buffer;
  gl##bufferData gl##._ARRAY_BUFFER_ pos gl##._STATIC_DRAW_;
  gl##vertexAttribPointer pos_attr 3 gl##._FLOAT _false 0 0;
  let norm_attr = gl##getAttribLocation prog (string "a_normal") in
  gl##enableVertexAttribArray norm_attr;
  let norm_buffer = gl##createBuffer in
  gl##bindBuffer gl##._ARRAY_BUFFER_ norm_buffer;
  gl##bufferData gl##._ARRAY_BUFFER_ norm gl##._STATIC_DRAW_;
  gl##vertexAttribPointer norm_attr 3 gl##._FLOAT _false 0 0;
  let mat =
    Proj3D.(mult (rotate_x (pi /. 2.)) (mult (scale 0.5 0.5 0.5) (translate 0. 0. 0.)))
  in
  check_error gl;
  debug "ready";
  let get_time () = Js.to_float (new%js date_now)##getTime in
  let last_draw = ref (get_time ()) in
  let draw_times = Queue.create () in
  let rec f () =
    let t = Js.to_float (new%js date_now)##getTime /. 1000. in
    let mat' = Proj3D.mult mat (Proj3D.rotate_y (1. *. t)) in
    gl##uniformMatrix4fv_typed proj_loc _false (Proj3D.array mat');
    gl##clear (gl##._DEPTH_BUFFER_BIT_ lor gl##._COLOR_BUFFER_BIT_);
    gl##drawArrays gl##._TRIANGLES 0 (pos##.length / 3);
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
    Lwt_js.sleep 0.02 >>= f
  in
  f ()

let go _ =
  ignore
    (debug "fetching model";
     catch
       (fun () -> fetch_model "monkey.model" >>= start)
       (fun exn -> error "uncaught exception: %s" (Printexc.to_string exn)));
  _true

let _ = Dom_html.window##.onload := Dom_html.handler go
