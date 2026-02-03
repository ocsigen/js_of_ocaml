(* Graph viewer
 * Copyright (C) 2010 Jérôme Vouillon
 * Laboratoire PPS - CNRS Université Paris Diderot
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

type rect =
  { x : int
  ; y : int
  ; width : int
  ; height : int
  }

module Html = Dom_html

let create_canvas w h =
  let c = Html.createCanvas Html.document in
  c##.width := w;
  c##.height := h;
  c

module Common = Viewer_common.F (struct
  type font = Js.js_string Js.t

  type color = Js.js_string Js.t

  type text = Js.js_string Js.t

  let white = Js.string "#e8e8ee"

  type ctx = Html.canvasRenderingContext2D Js.t

  let save ctx = ctx##save

  let restore ctx = ctx##restore

  let scale ctx ~sx ~sy = ctx##scale (Js.float sx) (Js.float sy)

  let translate ctx ~tx ~ty = ctx##translate (Js.float tx) (Js.float ty)

  let begin_path ctx = ctx##beginPath

  let close_path ctx = ctx##closePath

  let move_to ctx ~x ~y = ctx##moveTo (Js.float x) (Js.float y)

  let line_to ctx ~x ~y = ctx##lineTo (Js.float x) (Js.float y)

  let curve_to ctx ~x1 ~y1 ~x2 ~y2 ~x3 ~y3 =
    ctx##bezierCurveTo
      (Js.float x1)
      (Js.float y1)
      (Js.float x2)
      (Js.float y2)
      (Js.float x3)
      (Js.float y3)

  let arc ctx ~xc ~yc ~radius ~angle1 ~angle2 =
    ctx##arc
      (Js.float xc)
      (Js.float yc)
      (Js.float radius)
      (Js.float angle1)
      (Js.float angle2)
      Js._true

  let rectangle ctx ~x ~y ~width ~height =
    ctx##rect (Js.float x) (Js.float y) (Js.float width) (Js.float height)

  let remap_color c =
    let s = Js.to_string c in
    match s with
    | "#000000" -> Js.string "#2a2a3a"
    | "#0000ff" -> Js.string "#3a6a9e"
    | "#ff0000" -> Js.string "#c04040"
    | _ -> c

  let fill ctx c =
    ctx##.fillStyle := remap_color c;
    ctx##fill

  let stroke ctx c =
    ctx##.strokeStyle := remap_color c;
    ctx##stroke

  let clip ctx = ctx##clip

  let draw_text (ctx : ctx) x y txt font fill_color stroke_color =
    ctx##.font := font;
    ctx##.textAlign := Js.string "center";
    ctx##.textBaseline := Js.string "middle";
    (match fill_color with
    | Some c ->
        ctx##.fillStyle := remap_color c;
        ctx##fillText txt (Js.float x) (Js.float y)
    | None -> ());
    match stroke_color with
    | Some c ->
        ctx##.strokeStyle := remap_color c;
        ctx##strokeText txt (Js.float x) (Js.float y)
    | None -> ()

  type window = Html.canvasElement Js.t

  type drawable = window * ctx

  type pixmap = drawable

  let get_drawable w =
    let ctx = w##getContext Html._2d_ in
    ctx##.lineWidth := Js.float 2.;
    w, ctx

  let make_pixmap _ width height =
    let c = Html.createCanvas Html.document in
    c##.width := width;
    c##.height := height;
    get_drawable c

  let drawable_of_pixmap p = p

  let get_context (_p, c) = c

  let put_pixmap
      ~dst:((_p, c) : drawable)
      ~x
      ~y
      ~xsrc
      ~ysrc
      ~width
      ~height
      ((p, _) : pixmap) =
    c##drawImage_fullFromCanvas
      p
      (Js.float (float xsrc))
      (Js.float (float ysrc))
      (Js.float (float width))
      (Js.float (float height))
      (Js.float (float x))
      (Js.float (float y))
      (Js.float (float width))
      (Js.float (float height))

  (****)

  type rectangle = rect =
    { x : int
    ; y : int
    ; width : int
    ; height : int
    }

  let compute_extents _ = assert false
end)

open Common

let redraw st s h v (canvas : Html.canvasElement Js.t) =
  let width = canvas##.width in
  let height = canvas##.height in
  redraw st s h v canvas { x = 0; y = 0; width; height } 0 0 width height

let ( >>= ) = Lwt.bind

let http_get url =
  XmlHttpRequest.get url
  >>= fun { XmlHttpRequest.code = cod; content = msg; _ } ->
  if cod = 0 || cod = 200 then Lwt.return msg else fst (Lwt.wait ())

let getfile f = try Lwt.return (Sys_js.read_file ~name:f) with Sys_error _ -> http_get f

class adjustment
  ?(value = 0.)
  ?(lower = 0.)
  ?(upper = 100.)
  ?(step_incr = 1.)
  ?(page_incr = 10.)
  ?(page_size = 10.)
  () =
  object
    val mutable _value = value

    method value = _value

    val mutable _lower = lower

    method lower = _lower

    val mutable _upper = upper

    method upper = _upper

    val mutable _step_incr = step_incr

    method step_increment = _step_incr

    val mutable _page_incr = page_incr

    method page_increment = _page_incr

    val mutable _page_size = page_size

    method page_size = _page_size

    method set_value v = _value <- v

    method set_bounds ?lower ?upper ?step_incr ?page_incr ?page_size () =
      (match lower with
      | Some v -> _lower <- v
      | None -> ());
      (match upper with
      | Some v -> _upper <- v
      | None -> ());
      (match step_incr with
      | Some v -> _step_incr <- v
      | None -> ());
      (match page_incr with
      | Some v -> _page_incr <- v
      | None -> ());
      match page_size with
      | Some v -> _page_size <- v
      | None -> ()
  end

let handle_drag element f =
  let mx = ref 0. in
  let my = ref 0. in
  element##.onmousedown :=
    Html.handler (fun ev ->
        mx := Js.to_float ev##.clientX;
        my := Js.to_float ev##.clientY;
        element##.style##.cursor := Js.string "move";
        let c1 =
          Html.addEventListener
            Html.document
            Html.Event.mousemove
            (Html.handler (fun ev ->
                 let x = Js.to_float ev##.clientX and y = Js.to_float ev##.clientY in
                 let x' = !mx and y' = !my in
                 mx := x;
                 my := y;
                 f (x -. x') (y -. y');
                 Js._true))
            Js._true
        in
        let c2 = ref Js.null in
        c2 :=
          Js.some
            (Html.addEventListener
               Html.document
               Html.Event.mouseup
               (Html.handler (fun _ ->
                    Html.removeEventListener c1;
                    Js.Opt.iter !c2 Html.removeEventListener;
                    element##.style##.cursor := Js.string "";
                    Js._true))
               Js._true);
        (* We do not want to disable the default action on mouse down
           (here, keyboard focus)
           in this example. *)
        Js._true)

let of_json ~typ v =
  match Sys.backend_type with
  | Other "js_of_ocaml" -> Js._JSON##parse (Js.string v)
  | _ -> Deriving_Json.from_string typ v

type js_string = Js.js_string Js.t

let js_string_to_json _ _ : unit = assert false

let js_string_of_json buf = Js.bytestring (Deriving_Json.Json_string.read buf)

[@@@warning "-20-39"]

type scene =
  (float * float * float * float)
  * (float * float * float * float) array
  * (js_string, js_string, js_string) Scene.element array
[@@deriving json]

let start () =
  let doc = Html.document in
  let page = doc##.documentElement in
  page##.style##.overflow := Js.string "hidden";
  let started = ref false in
  let p = Html.createP doc in
  p##.innerHTML := Js.string "Loading graph...";
  p##.className := Js.string "loading";
  p##.style##.display := Js.string "none";
  Dom.appendChild doc##.body p;
  ignore
    (Lwt_js.sleep 0.5
    >>= fun () ->
    if not !started then p##.style##.display := Js.string "block";
    Lwt.return ());
  (*
  Console.console##time(Js.string "loading");
*)
  getfile "scene.json"
  >>= fun s ->
  (*
  Console.console##timeEnd(Js.string "loading");
  Console.console##time(Js.string "parsing");
*)
  let (x1, y1, x2, y2), bboxes, scene = of_json ~typ:[%json: scene] s in
  (*
  Console.console##timeEnd(Js.string "parsing");
  Console.console##time(Js.string "init");
*)
  started := true;
  Dom.removeChild doc##.body p;
  let st =
    { bboxes
    ; scene
    ; zoom_factor = 1. /. 20.
    ; st_x = x1
    ; st_y = y1
    ; st_width = x2 -. x1
    ; st_height = y2 -. y1
    ; st_pixmap = Common.make_pixmap ()
    }
  in
  let canvas = create_canvas page##.clientWidth page##.clientHeight in
  Dom.appendChild doc##.body canvas;
  let allocation () =
    { x = 0; y = 0; width = canvas##.width; height = canvas##.height }
  in
  let hadj = new adjustment () in
  let vadj = new adjustment () in
  let sadj = new adjustment ~upper:20. ~step_incr:1. ~page_incr:0. ~page_size:0. () in
  let zoom_steps = 8. in
  (* Number of steps to get a factor of 2 *)
  let set_zoom_factor f =
    let count = ceil (log f /. log 2. *. zoom_steps) in
    let f = 2. ** (count /. zoom_steps) in
    sadj#set_bounds ~upper:count ();
    st.zoom_factor <- f
  in
  let get_scale () = (2. ** (sadj#value /. zoom_steps)) /. st.zoom_factor in
  let redraw_queued = ref false in
  let update_view _force =
    (*
Console.console##log_2(Js.string "update", Js.date##now());
*)
    let a = allocation () in
    let scale = get_scale () in
    let aw = ceil (float a.width /. scale) in
    let ah = ceil (float a.height /. scale) in
    hadj#set_bounds
      ~step_incr:(aw /. 20.)
      ~page_incr:(aw /. 2.)
      ~page_size:(min aw st.st_width)
      ~upper:st.st_width
      ();
    let mv = st.st_width -. hadj#page_size in
    if hadj#value < 0. then hadj#set_value 0.;
    if hadj#value > mv then hadj#set_value mv;
    vadj#set_bounds
      ~step_incr:(ah /. 20.)
      ~page_incr:(ah /. 2.)
      ~page_size:(min ah st.st_height)
      ~upper:st.st_height
      ();
    let mv = st.st_height -. vadj#page_size in
    if vadj#value < 0. then vadj#set_value 0.;
    if vadj#value > mv then vadj#set_value mv;
    if not !redraw_queued
    then (
      redraw_queued := true;
      let (_ : Html.animation_frame_request_id) =
        Html.window##requestAnimationFrame
          (Js.wrap_callback (fun _ ->
               redraw_queued := false;
               redraw st (get_scale ()) hadj#value vadj#value canvas))
      in
      ())
    (*
    if force then redraw st (get_scale ()) hadj#value vadj#value canvas else
    if not !redraw_queued then
      ignore (redraw_queued := true;
(*
Console.console##log(Js.string "sleep");
*)
              Lwt_js.yield() >>= fun () ->
              redraw_queued := false;
              redraw st (get_scale ()) hadj#value vadj#value canvas;
              Lwt.return ())
*)
  in
  let a = allocation () in
  let zoom_factor = max (st.st_width /. float a.width) (st.st_height /. float a.height) in
  set_zoom_factor zoom_factor;
  let prev_scale = ref (get_scale ()) in
  let rescale x y =
    let scale = get_scale () in
    let r = 1. -. (!prev_scale /. scale) in
    hadj#set_value (hadj#value +. (hadj#page_size *. r *. x));
    vadj#set_value (vadj#value +. (vadj#page_size *. r *. y));
    prev_scale := scale;
    invalidate_pixmap st.st_pixmap;
    update_view false
  in
  let thumb_h = 28 in
  let slider_w = 28 in
  let height = 400 - thumb_h in
  let points d = Js.string (Printf.sprintf "%dpx" d) in
  let pos = ref height in
  let track = Html.createDiv doc in
  track##.className := Js.string "zoom-track";
  let style = track##.style in
  style##.position := Js.string "absolute";
  style##.width := Js.string "4px";
  style##.top := points (thumb_h / 2);
  style##.bottom := points (thumb_h / 2);
  style##.left := points ((slider_w - 4) / 2);
  let thumb = Html.createDiv doc in
  thumb##.className := Js.string "zoom-thumb";
  let style = thumb##.style in
  style##.position := Js.string "absolute";
  style##.width := points slider_w;
  style##.height := points thumb_h;
  style##.top := points !pos;
  style##.left := Js.string "0px";
  let slider = Html.createDiv doc in
  slider##.className := Js.string "zoom-slider";
  let style = slider##.style in
  style##.position := Js.string "absolute";
  style##.width := points slider_w;
  style##.height := points (height + thumb_h);
  style##.top := Js.string "16px";
  style##.right := Js.string "16px";
  Dom.appendChild slider track;
  Dom.appendChild slider thumb;
  Dom.appendChild doc##.body slider;
  let set_slider_position pos' =
    if pos' <> !pos
    then (
      thumb##.style##.top := points pos';
      pos := pos';
      sadj#set_value (float (height - pos') *. sadj#upper /. float height);
      rescale 0.5 0.5)
  in
  handle_drag thumb (fun _dx dy ->
      set_slider_position (min height (max 0 (!pos + int_of_float dy))));
  slider##.onmousedown :=
    Html.handler (fun ev ->
        let ey = Js.to_float ev##.clientY in
        let _, sy = Dom_html.elementClientPosition slider in
        set_slider_position (max 0 (min height (int_of_float ey - sy - (thumb_h / 2))));
        Js._false);
  let adjust_slider () =
    let pos' = height - truncate ((sadj#value *. float height /. sadj#upper) +. 0.5) in
    thumb##.style##.top := points pos';
    pos := pos'
  in
  Html.window##.onresize :=
    Html.handler (fun _ ->
        let page = doc##.documentElement in
        canvas##.width := page##.clientWidth;
        canvas##.height := page##.clientHeight;
        update_view true;
        Js._true);
  (* Drag the graph using the mouse *)
  handle_drag canvas (fun dx dy ->
      let scale = get_scale () in
      let offset a d =
        a#set_value (min (a#value -. (d /. scale)) (a#upper -. a#page_size))
      in
      offset hadj dx;
      offset vadj dy;
      update_view true);
  let bump_scale x y v =
    let a = allocation () in
    let x = x /. float a.width in
    let y = y /. float a.height in
    let prev = sadj#value in
    let vl = min sadj#upper (max sadj#lower (prev +. (v *. sadj#step_increment))) in
    if vl <> prev
    then (
      sadj#set_value vl;
      adjust_slider ();
      if x >= 0. && x <= 1. && y >= 0. && y <= 1. then rescale x y else rescale 0.5 0.5);
    Js._false
  in
  (* Zoom using the mouse wheel *)
  ignore
    (Html.addMousewheelEventListener
       canvas
       (fun ev ~dx:_ ~dy ->
         let ex, ey = Dom_html.elementClientPosition canvas in
         let x = Js.to_float ev##.clientX -. float ex in
         let y = Js.to_float ev##.clientY -. float ey in
         if dy < 0
         then bump_scale x y 1.
         else if dy > 0
         then bump_scale x y (-1.)
         else Js._false)
       Js._true);
  (*
  Html.addEventListener Html.document Html.Event.keydown
    (Html.handler
       (fun e -> Console.console##log(e##keyCode);
         Js._true))
    Js._true;
*)
  (*
  Html.addEventListener Html.document Html.Event.keypress
    (Html.handler
       (fun e ->
             Console.console##log(Js.string "press");
         match e##keyCode with
         | 37 -> (* left *)
             Js._false
         | 38 -> (* up *)
             Js._false
         | 39 -> (* right *)
             Js._false
         | 40 -> (* down *)
             Js._false
         | _ ->
             Console.console##log(- 1- e##keyCode);
             Js._true))
    Js._true;
*)
  let handle_key_event ev =
    match ev##.keyCode with
    | 37 ->
        (* left *)
        hadj#set_value (hadj#value -. hadj#step_increment);
        update_view false;
        Js._false
    | 38 ->
        (* up *)
        vadj#set_value (vadj#value -. vadj#step_increment);
        update_view false;
        Js._false
    | 39 ->
        (* right *)
        hadj#set_value (hadj#value +. hadj#step_increment);
        update_view false;
        Js._false
    | 40 ->
        (* down *)
        vadj#set_value (vadj#value +. vadj#step_increment);
        update_view false;
        Js._false
    | _ ->
        (*
        Console.console##log_2(Js.string "keycode:", ev##keyCode);
*)
        Js._true
  in
  let ignored_keycode = ref (-1) in
  Html.document##.onkeydown :=
    Html.handler (fun e ->
        ignored_keycode := e##.keyCode;
        handle_key_event e);
  Html.document##.onkeypress :=
    Html.handler (fun e ->
        let k = !ignored_keycode in
        ignored_keycode := -1;
        if e##.keyCode = k then Js._true else handle_key_event e);
  (*
Console.console##time(Js.string "initial drawing");
*)
  update_view true;
  (*
Console.console##timeEnd(Js.string "initial drawing");
Console.console##timeEnd(Js.string "init");
*)
  Lwt.return ()

let () = Lwt.async start
