(* Js_of_ocaml example
 * http://www.ocsigen.org/js_of_ocaml/
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

(*
- check compatibility + wait page

- files to load set in index.html
  ==> tree.txt, icon directory, image_info.json, image directories

- validation tools

- we should stop updating the canvas when it is not visible
  (opaque overlay)

- do not cache images forever (just keep a list of recently used images)
  ==> keep images used last + twice as many / minimum 200 ?

- spinner: draw it on a canvas
  (===> large canvas...)

- webgl?

- swap image once loaded instead of putting it over the canvas (?)

- récupérer le nom des animaux?
  ==> overlay mechanism:
        save what is below, display overlay; restore when moved

- can we have fast shadows? ==> precompute the shadows in a canvas?
- should we have half size images for better rendering???
  (might make it possible fast shadows?)

- support for touch events
- limit how far we can go

- use history to save current location and random seed

- get all image sizes so that we can size precisely images before
  they are all loaded

==============

- find point closest to center; start drawing from it; stop below a
  given threshold
- speed-up rendering by traversing the tree and stopping when the
  edge length becomes below some threshold

===============

- preferred diameter of each node
  spring-like forces around this diameter
  repulsive forces:
    exponential when below the preferred diameter
    decrease exponentially
  http://en.wikipedia.org/wiki/Hyperbolic_geometry#Circles.2C_spheres_and_balls

- importante répulsion quand trop près!
    ==> surtout pour feuilles!
- cap the acceleration

- metric tree/vp-tree?
  r trees / kd trees
*)

(* List of icons to be prefetched *)
let icons =
  [ "commons-38.png"
  ; "wikipedia-38.png"
  ; "info-38.png"
  ; "meeting-point-38.png"
  ; "globe-38.png"
  ; "ocsigen-powered.png"
  ]

let icon nm = Js.string ("icons/" ^ nm)

let tree_color = Js.string "#794c0d"

(*
let outside_color = Js.string (*"#3d2606"*) "#1e1303"
let outside_color = Js.string (*"#0c1a0d"*) "#070718"
*)

let option var = Js.Optdef.get var (fun () -> Js.Unsafe.coerce (new%js Js.array_empty))

class type style = object
  method border : Js.number_t Js.optdef Js.readonly_prop

  method padding : Js.number_t Js.optdef Js.readonly_prop

  method backgroundColor : Js.js_string Js.t Js.optdef Js.readonly_prop

  method boundaryColor : Js.js_string Js.t Js.optdef Js.readonly_prop

  method treeColor : Js.js_string Js.t Js.optdef Js.readonly_prop

  method nodeColor : Js.js_string Js.t Js.optdef Js.readonly_prop

  method nodeBackgroundColor : Js.js_string Js.t Js.optdef Js.readonly_prop

  method nodeFont : Js.js_string Js.t Js.optdef Js.readonly_prop

  method buttonColor : Js.js_string Js.t Js.optdef Js.readonly_prop
end

let style : style Js.t = option Js.Unsafe.global##.hyp_style_

class type messages = object
  method info : Js.js_string Js.t Js.optdef Js.readonly_prop

  method recenter : Js.js_string Js.t Js.optdef Js.readonly_prop

  method noRef : Js.js_string Js.t Js.optdef Js.readonly_prop

  method close : Js.js_string Js.t Js.optdef Js.readonly_prop

  method wikimediaCommons : Js.js_string Js.t Js.optdef Js.readonly_prop

  method language : Js.js_string Js.t Js.optdef Js.readonly_prop

  method noRef : Js.js_string Js.t Js.optdef Js.readonly_prop

  method languages : Js.js_string Js.t Js.optdef Js.readonly_prop

  method ok : Js.js_string Js.t Js.optdef Js.readonly_prop
end

let opt_style v default = Js.Optdef.get v (fun () -> default)

(**** Complex numbers ****)

type c =
  { x : float
  ; y : float
  }

let one = { x = 1.; y = 0. }

let zero = { x = 0.; y = 0. }

(* Scalar operations *)

let sdiv z s = { x = z.x /. s; y = z.y /. s }

(* Norm *)

let sq_norm c = (c.x *. c.x) +. (c.y *. c.y)

let norm c = sqrt (sq_norm c)

let normalize c = sdiv c (norm c)

(* Conjugate and negation *)

let conj z = { x = z.x; y = -.z.y }

let neg z = { x = -.z.x; y = -.z.y }

(* Addition, multiplication and division *)

let add z t = { x = z.x +. t.x; y = z.y +. t.y }

let sub z t = { x = z.x -. t.x; y = z.y -. t.y }

let sq_norm_sub z t =
  let x = z.x -. t.x in
  let y = z.y -. t.y in
  (x *. x) +. (y *. y)

let mul z t = { x = (z.x *. t.x) -. (z.y *. t.y); y = (z.x *. t.y) +. (z.y *. t.x) }

let add_mul a z b =
  { x = (a.x *. z.x) -. (a.y *. z.y) +. b.x; y = (a.x *. z.y) +. (a.y *. z.x) +. b.y }

let div z t =
  let n = sq_norm t in
  { x = ((z.x *. t.x) +. (z.y *. t.y)) /. n; y = ((z.y *. t.x) -. (z.x *. t.y)) /. n }

(* Möbius transformation, hyperbolic transformation *)

(* (a.z + b) / (c.z + d) *)
let transf a b c d z = div (add_mul a z b) (add_mul c z d)

(* (t.z + p) / (conj p.z + 1) *)
let hyp_transf (p, t) =
  let a = t in
  let b = p in
  let c = mul (conj p) t in
  let d = one in
  fun z -> transf a b c d z

let hyp_transf_vect (p, t) v v' =
  let a = t in
  let b = p in
  let c = mul (conj p) t in
  let d = one in
  for i = 0 to Array.length v - 1 do
    v'.(i) <- transf a b c d v.(i)
  done

(* (z + p) / (conj p.z + 1) *)
let transl p = hyp_transf (p, one)

let compose (p1, t1) (p2, t2) =
  let t2p1 = mul t2 p1 in
  let den = add (mul t2p1 (conj p2)) one in
  div (add t2p1 p2) den, normalize (mul (mul t1 t2) (div (conj den) den))

(* Transformation from z0 to z1:
   z1 = (z0 + p) / (conj p.z0 + 1)
   ==> p = (z1.z0.conj (z1 - z0) + z1 - z0) / (1 - |z1.z0|^2) *)
let compute_translation z0 z1 =
  let dz = sub z1 z0 in
  let z0z1 = mul z0 z1 in
  sdiv (add (mul z0z1 (conj dz)) dz) (1. -. sq_norm z0z1)

(******)

let ( >>= ) = Lwt.bind

let lwt_wrap f =
  let t, w = Lwt.task () in
  let cont x = Lwt.wakeup w x in
  f cont;
  t

(******)

module Html = Dom_html

let json : < parse : Js.js_string Js.t -> 'a > Js.t = Js.Unsafe.pure_js_expr "JSON"

let http_get url =
  XmlHttpRequest.get url
  >>= fun r ->
  let cod = r.XmlHttpRequest.code in
  let msg = r.XmlHttpRequest.content in
  if cod = 0 || cod = 200 then Lwt.return msg else fst (Lwt.wait ())

let getfile f = try Lwt.return (Sys_js.read_file ~name:f) with Not_found -> http_get f

let load_image src =
  let img = Html.createImg Html.document in
  lwt_wrap (fun c ->
      img##.onload :=
        Html.handler (fun _ ->
            c ();
            Js._false);
      img##.src := src)
  >>= fun () -> Lwt.return img

let create_canvas w h =
  let d = Html.window##.document in
  let c = Html.createCanvas d in
  c##.width := w;
  c##.height := h;
  c

let _debug_widget =
  let d = Html.document in
  let w = Html.createDiv d in
  w##.style##.position := Js.string "absolute";
  w##.style##.bottom := Js.string "0";
  w##.style##.left := Js.string "0";
  w##.style##.lineHeight := Js.string "0.9em";
  w

let _debug_msg _s = ()

(*
  let d = Html.document in
  Dom.appendChild d##body debug_widget;
  let p = Html.createP d in
  p##innerHTML <- Js.string s;
  Dom.appendChild debug_widget p
                    *)

let handle_drag element move stop click =
  let fuzz = 4. in
  element##.onmousedown :=
    Html.handler (fun ev ->
        let x0 = Js.to_float ev##.clientX and y0 = Js.to_float ev##.clientY in
        (*
debug_msg (Format.sprintf "Mouse down %d %d" x0 y0);
*)
        let started = ref false in
        let c1 =
          Html.addEventListener
            Html.document
            Html.Event.mousemove
            (Html.handler (fun ev ->
                 let x = Js.to_float ev##.clientX and y = Js.to_float ev##.clientY in
                 (*
debug_msg (Format.sprintf "Mouse move %d %d %d %d" x0 y0 x y);
*)
                 if
                   (not !started)
                   && (abs_float (x -. x0) > fuzz || abs_float (y -. y0) > fuzz)
                 then (
                   started := true;
                   element##.style##.cursor := Js.string "move");
                 if !started then move x0 y0 x y;
                 Html.stopPropagation ev;
                 Js._true))
            Js._true
        in
        let c2 = ref Js.null in
        c2 :=
          Js.some
            (Html.addEventListener
               Html.document
               Html.Event.mouseup
               (Html.handler (fun ev ->
                    (*
debug_msg (Format.sprintf "Mouse up %d %d %d %d" x0 y0 ev##clientX ev##clientY);
*)
                    Html.removeEventListener c1;
                    Js.Opt.iter !c2 Html.removeEventListener;
                    if !started
                    then (
                      element##.style##.cursor := Js.string "";
                      stop (Js.to_float ev##.clientX) (Js.to_float ev##.clientY))
                    else click (Js.to_float ev##.clientX) (Js.to_float ev##.clientY);
                    Js._true))
               Js._true);
        Js._true)

let handle_touch_events element move stop cancel click =
  let fuzz = 4. in
  ignore
    (Html.addEventListener
       element
       Html.Event.touchstart
       (Html.handler (fun ev ->
            Js.Optdef.iter
              (ev##.changedTouches##item 0)
              (fun touch ->
                let id = touch##.identifier in
                let x0 = Js.to_float touch##.clientX
                and y0 = Js.to_float touch##.clientY in
                (*
debug_msg (Format.sprintf "Touch start %d %d" x0 y0);
*)
                let started = ref false in
                let c1 =
                  Html.addEventListener
                    Html.document
                    Html.Event.touchmove
                    (Html.handler (fun ev ->
                         for i = 0 to ev##.changedTouches##.length - 1 do
                           Js.Optdef.iter
                             (ev##.changedTouches##item i)
                             (fun touch ->
                               if touch##.identifier = id
                               then (
                                 let x = Js.to_float touch##.clientX
                                 and y = Js.to_float touch##.clientY in
                                 (*
  debug_msg (Format.sprintf "Touch move %d %d %d %d" x0 y0 x y);
*)
                                 if
                                   (not !started)
                                   && (abs_float (x -. x0) > fuzz
                                      || abs_float (y -. y0) > fuzz)
                                 then (
                                   started := true;
                                   element##.style##.cursor := Js.string "move");
                                 if !started then move x0 y0 x y))
                         done;
                         Html.stopPropagation ev;
                         Js._false))
                    Js._true
                in
                let c2 = ref Js.null in
                let c3 = ref Js.null in
                c2 :=
                  Js.some
                    (Html.addEventListener
                       Html.document
                       Html.Event.touchend
                       (Html.handler (fun ev ->
                            for i = 0 to ev##.changedTouches##.length - 1 do
                              Js.Optdef.iter
                                (ev##.changedTouches##item i)
                                (fun touch ->
                                  if touch##.identifier = id
                                  then (
                                    let x = Js.to_float touch##.clientX
                                    and y = Js.to_float touch##.clientY in
                                    (*
debug_msg (Format.sprintf "Touch end %d %d %d %d" x0 y0 x y);
*)
                                    Html.removeEventListener c1;
                                    Js.Opt.iter !c2 Html.removeEventListener;
                                    Js.Opt.iter !c3 Html.removeEventListener;
                                    if !started
                                    then (
                                      element##.style##.cursor := Js.string "";
                                      stop x y)
                                    else click x y))
                            done;
                            Js._true))
                       Js._true);
                c3 :=
                  Js.some
                    (Html.addEventListener
                       Html.document
                       Html.Event.touchend
                       (Html.handler (fun ev ->
                            for i = 0 to ev##.changedTouches##.length - 1 do
                              Js.Optdef.iter
                                (ev##.changedTouches##item i)
                                (fun touch ->
                                  if touch##.identifier = id
                                  then (
                                    let x = touch##.clientX and y = touch##.clientY in
                                    (*
debug_msg (Format.sprintf "Touch cancel %d %d %d %d" x0 y0 x y);
*)
                                    Html.removeEventListener c1;
                                    Js.Opt.iter !c2 Html.removeEventListener;
                                    Js.Opt.iter !c3 Html.removeEventListener;
                                    if !started
                                    then element##.style##.cursor := Js.string "";
                                    cancel x y))
                            done;
                            Js._false))
                       Js._true));
            Js._false))
       Js._true)

(*
let handle_touch_events element move stop cancel =
  let fuzz = 4 in
  let id = ref 0 in
  let x0 = ref 0 in
  let y0 = ref 0 in
  ignore (Html.addEventListener element Html.Event.touchstart
    (Html.handler (fun ev ->
       OptDef.iter (ev##changedTouches##item(0)) (fun touch ->
       id := touch##identifier; in
       x0 := touch##clientX;
       y0 := touch##clientY));
(* x0 := ev##pageX; y0 := ev##pageY;*)
debug_msg (Format.sprintf "Touch down %d %d" !x0 !y0);
 Js._false))
    Js._false);
  ignore (Html.addEventListener element Html.Event.touchmove
    (Html.handler
       (fun ev ->
(*
          let x = ev##pageX and y = ev##pageY in
*)
debug_msg (Format.sprintf "Touch move %d %d %d %d" !x0 !y0 x y);
          move !x0 !y0 x y;
          Js._false))
    Js._false);
  ignore (Html.addEventListener element Html.Event.touchend
    (Html.handler (fun _ ->
debug_msg (Format.sprintf "Touch end");
 stop (); Js._false))
    Js._false);
  ignore (Html.addEventListener element Html.Event.touchcancel
    (Html.handler (fun _ -> cancel (); Js._true))
    Js._false)
*)

let roundRectPath c x y w h r =
  let r = min r (min w h /. 2.) in
  c##beginPath;
  c##moveTo (Js.float (x +. r)) (Js.float y);
  c##arcTo
    (Js.float (x +. w))
    (Js.float y)
    (Js.float (x +. w))
    (Js.float (y +. r))
    (Js.float r);
  c##arcTo
    (Js.float (x +. w))
    (Js.float (y +. h))
    (Js.float (x +. w -. r))
    (Js.float (y +. h))
    (Js.float r);
  c##arcTo
    (Js.float x)
    (Js.float (y +. h))
    (Js.float x)
    (Js.float (y +. h -. r))
    (Js.float r);
  c##arcTo (Js.float x) (Js.float y) (Js.float (x +. r)) (Js.float y) (Js.float r)

let text_size_div =
  let doc = Html.document in
  lazy
    (let d = Html.createDiv doc in
     d##.style##.visibility := Js.string "hidden";
     d##.style##.position := Js.string "absolute";
     d##.style##.whiteSpace := Js.string "nowrap";
     Dom.appendChild doc##.body d;
     d)

let text_size font txt =
  let doc = Html.document in
  let d = Lazy.force text_size_div in
  d##.style##.font := font;
  let txt = doc##createTextNode (Js.string txt) in
  Dom.appendChild d txt;
  let res = d##.clientWidth, d##.clientHeight in
  Dom.removeChild d txt;
  res

(******)

let default_language () =
  (Js.Optdef.get
     Dom_html.window##.navigator##.language
     (fun () ->
       Js.Optdef.get Dom_html.window##.navigator##.userLanguage (fun () -> Js.string "en")))
  ##substring
    0
    2

let language =
  ref
    (Js.Optdef.case Html.window##.localStorage default_language (fun st ->
         Js.Opt.get (st##getItem (Js.string "hyp_lang")) default_language))

let _ = Firebug.console##log !language

let set_language lang =
  Js.Optdef.iter Html.window##.localStorage (fun st ->
      st##setItem (Js.string "hyp_lang") lang);
  language := lang

let load_messages () =
  getfile "messages.json" >>= fun s -> Lwt.return (json##parse (Js.string s))

let local_messages msgs : messages Js.t = option (Js.Unsafe.get msgs !language)

(******)

let screen_transform canvas =
  let offset =
    Js.to_float (opt_style style##.border (Js.float 0.5))
    +. Js.to_float (opt_style style##.padding (Js.float 0.))
  in
  let w = canvas##.width in
  let h = canvas##.height in
  (*
  let r = float (min  w h) /. 2. in
*)
  let rx = float w /. 2. in
  let ry = float h /. 2. in
  let dx = float w /. 2. in
  let dy = float h /. 2. in
  let rx = max 5. (rx -. offset) in
  let ry = max 5. (ry -. offset) in
  rx, ry, dx, dy

let eps = 0.05

(*
let to_screen z = ((z.x +. 1.) *. r, (z.y +. 1.) *. r)
*)
let from_screen canvas x y =
  let rx, ry, dx, dy = screen_transform canvas in
  let z = { x = (x -. dx) /. rx; y = (y -. dy) /. ry } in
  let n = norm z in
  if n <= 1. -. eps then z else sdiv z (n /. (1. -. eps))

let pi = 4. *. atan 1.

let arc c (rx, ry, dx, dy) z0 z1 z2 =
  let rd = norm (sub z1 z0) in
  let start = atan2 (z1.y -. z0.y) (z1.x -. z0.x) in
  let fin = atan2 (z2.y -. z0.y) (z2.x -. z0.x) in
  c##beginPath;
  let alpha = mod_float (fin -. start +. (2. *. pi)) (2. *. pi) in
  c##ellipse
    (Js.float ((z0.x *. rx) +. dx))
    (Js.float ((z0.y *. ry) +. dy))
    (Js.float (rd *. rx))
    (Js.float (rd *. ry))
    (Js.float 0.)
    (Js.float start)
    (Js.float fin)
    (Js.bool (alpha > pi));
  c##stroke

let line c (rx, ry, dx, dy) z1 z2 =
  c##beginPath;
  c##moveTo (Js.float ((z1.x *. rx) +. dx)) (Js.float ((z1.y *. ry) +. dy));
  c##lineTo (Js.float ((z2.x *. rx) +. dx)) (Js.float ((z2.y *. ry) +. dy));
  c##stroke

(*
We have
  |z0|^2 = 1 + r^2  (the "line" is orthogonal to the unit circle)
  |z1-z0|^2 = r^2   (z1 is on the circle)
  |z2-z0|^2 = r^2   (z2 is on the circle)
By solving this set of equations, we get z0.
*)
let segment c transf z1 z2 =
  let d = 2. *. ((z1.x *. z2.y) -. (z1.y *. z2.x)) in
  if abs_float d < 0.05
  then line c transf z1 z2
  else
    let n1 = sq_norm z1 +. 1. in
    let n2 = sq_norm z2 +. 1. in
    let z0 =
      { x = ((z2.y *. n1) -. (z1.y *. n2)) /. d; y = ((z1.x *. n2) -. (z2.x *. n1)) /. d }
    in
    arc c transf z0 z1 z2

type boxes =
  { bx : float array
  ; by : float array
  ; bw : float array
  ; bh : float array
  }

let shadow = false

let draw canvas vertices edges nodes boxes =
  Firebug.console##time (Js.string "draw");
  let c = canvas##getContext Html._2d_ in
  let ((rx, ry, dx, dy) as transf) = screen_transform canvas in
  c##clearRect
    (Js.float 0.)
    (Js.float 0.)
    (Js.float (float canvas##.width))
    (Js.float (float canvas##.height));
  let padding = Js.to_float (opt_style style##.padding (Js.float 0.)) in
  c##beginPath;
  c##ellipse
    (Js.float dx)
    (Js.float dy)
    (Js.float (rx +. padding))
    (Js.float (ry +. padding))
    (Js.float 0.)
    (Js.float 0.)
    (Js.float 7.)
    Js._false;
  Js.Optdef.iter style##.backgroundColor (fun color ->
      c##.fillStyle := color;
      c##fill);
  Js.Optdef.iter style##.boundaryColor (fun color ->
      c##.lineWidth := Js.float 1.;
      c##.strokeStyle := color;
      c##stroke);
  c##.lineWidth := Js.float 2.;
  c##.lineCap := Js.string "round";
  c##.strokeStyle := opt_style style##.treeColor tree_color;
  let rx, ry, _, _ = transf in
  for i = 0 to Array.length edges - 1 do
    let j, j', w = edges.(i) in
    let z = vertices.(j) in
    let z' = vertices.(j') in
    if rx *. ry *. sq_norm_sub z z' > 4.
    then (
      c##.lineWidth := Js.float w;
      segment c transf z z')
  done;
  let image_count = ref 0 in
  let large_image_count = ref 0 in
  for i = 0 to Array.length nodes - 1 do
    let l, img = nodes.(i) in
    match img with
    | `Img (img, _) -> (
        boxes.bw.(i) <- 0.;
        (* Invalidate image location. *)
        let z = vertices.(i) in
        let min_scale l w h s =
          let s = ref s in
          for i = 0 to Array.length l - 1 do
            let j, large = l.(i) in
            let sx = abs_float (vertices.(j).x -. z.x) /. w in
            let sy = abs_float (vertices.(j).y -. z.y) /. h in
            let s' = if sx > sy then sx else sy in
            let s' = if large then s' else 1.9 *. s' in
            if s' < !s then s := s'
          done;
          !s
        in
        if not (Lazy.is_val img)
        then (
          let s = min_scale l 1. 1. 1. in
          if s *. max rx ry > 1. then ignore (Lazy.force img))
        else
          match Lwt.poll (Lazy.force img) with
          | Some img ->
              (*
        if min_w *. r > 1. && min_h *. r > 1. then begin
*)
              let w = float img##.width in
              let h = float img##.height in
              (*
              let s = min_scale l w h (1. /. min w h) in
              let scale = s *. r in
*)
              let s = min_scale l 1. 1. 1. in
              let scale = s /. max (w /. rx) (h /. ry) in
              let w = w *. scale /. 2. in
              let h = h *. scale /. 2. in
              if w > 1. && h > 1.
              then (
                (*
              let ratio = r *. min (min_w /. w) (min_h /. h) in
              let w = w *. ratio in
              let h = h *. ratio in
*)
                if shadow
                then (
                  c##save;
                  (*
                let blur = 7. /. img_r *. rd in
                let offset = 5. /. img_r *. rd in
*)
                  let blur = 7. *. scale in
                  let offset = 5. *. scale in
                  c##.shadowBlur := Js.float (if blur < 1. then 0. else blur);
                  c##.shadowOffsetX := Js.float (if blur < 1. then 0. else offset);
                  c##.shadowOffsetY := Js.float (if blur < 1. then 0. else offset);
                  c##.shadowColor := Js.string "black");
                let x = (z.x *. rx) +. dx in
                let y = (z.y *. ry) +. dy in
                c##drawImage_withSize
                  img
                  (Js.float (x -. w))
                  (Js.float (y -. h))
                  (Js.float (2. *. w))
                  (Js.float (2. *. h));
                (*
              c##drawImage_fromCanvasWithSize
                   (img, x -. w, y -. h, 2. *. w, 2. *. h);
*)
                if shadow then c##restore;
                boxes.bx.(i) <- x;
                boxes.by.(i) <- y;
                boxes.bw.(i) <- w;
                boxes.bh.(i) <- h;
                incr image_count;
                if w >= 2. && h >= 2. then incr large_image_count)
          | None -> ())
    | `Txt (circle, Some txt, _) ->
        let z = vertices.(i) in
        let r2 =
          Array.fold_left (fun r2 (j, _) -> min r2 (sq_norm (sub vertices.(j) z))) 1. l
        in
        let w = float txt##.width in
        let h = float txt##.height in
        let img_d = sqrt ((w *. w) +. (h *. h)) in
        let rd = sqrt (r2 *. rx *. ry) *. 0.5 in
        let rd = if rd > img_d /. 2. then img_d /. 2. else rd in
        let w = w /. img_d *. rd in
        let h = h /. img_d *. rd in
        if circle
        then (
          c##beginPath;
          c##.fillStyle := opt_style style##.nodeBackgroundColor tree_color;
          c##arc
            (Js.float ((z.x *. rx) +. dx))
            (Js.float ((z.y *. ry) +. dy))
            (Js.float (sqrt ((w *. w) +. (h *. h))))
            (Js.float 0.)
            (Js.float 7.)
            Js._false;
          c##fill);
        c##drawImage_fromCanvasWithSize
          txt
          (Js.float ((z.x *. rx) +. dx -. w))
          (Js.float ((z.y *. ry) +. dy -. h))
          (Js.float (2. *. w))
          (Js.float (2. *. h))
    | `Txt (_, None, _) | `None -> ()
  done;
  Firebug.console##timeEnd (Js.string "draw");
  Firebug.console##log_2 !image_count !large_image_count

let tree_url = "tree.json"

let ( >> ) x f = f x

type 'a tree = Node of 'a * 'a tree array

let rec tree_vertice_count n =
  let (Node (_, l)) = n in
  Array.fold_left (fun s n -> s + tree_vertice_count n) 1 l

let rec tree_edge_count n =
  let (Node (_, l)) = n in
  Array.fold_left (fun s n -> s + 1 + tree_edge_count n) 0 l

let rec randomize_tree n =
  let (Node (_info, ch)) = n in
  for i = Array.length ch - 1 downto 0 do
    let v = ch.(i) in
    let j = truncate (Js.to_float Js.math##random *. float (i + 1)) in
    ch.(i) <- ch.(j);
    ch.(j) <- v
  done;
  Array.iter randomize_tree ch

let need_redraw = ref false

let redraw_funct = ref (fun () -> ())

let perform_redraw () =
  need_redraw := false;
  !redraw_funct ()

let schedule_redraw () =
  if not !need_redraw
  then (
    need_redraw := true;
    let (_ : Html.animation_frame_request_id) =
      Html.window##requestAnimationFrame
        (Js.wrap_callback (fun _ -> if !need_redraw then perform_redraw ()))
    in
    ())

(*
let load_image src =
  load_image src >>= fun img ->
  let w = img##width in
  let h = img##height in
  let canvas = create_canvas (w + 14) (h + 14) in
  let c = canvas##getContext (Html._2d_) in
  c##shadowBlur <- 7.;
  c##shadowOffsetX <- 5.;
  c##shadowOffsetY <- 5.;
  c##shadowColor <- Js.string "black";
  c##drawImage_withSize (img, 2., 2., float w, float h);
  Lwt.return canvas
*)

let image_node img =
  `Img
    ( lazy
        (Lwt_js.yield ()
        >>= fun () ->
        load_image (Js.string ("thumbnails/" ^ img ^ ".jpg"))
        >>= fun img ->
        schedule_redraw ();
        Lwt.return img)
    , img )

let compute_text_node info =
  let font = opt_style style##.nodeFont (Js.string "20px sans-serif") in
  let w, h = text_size font info in
  let w = w + 8 in
  let h = h + 8 in
  let canvas = create_canvas w h in
  let c = canvas##getContext Html._2d_ in
  c##.fillStyle := opt_style style##.nodeBackgroundColor tree_color;
  roundRectPath c 0. 0. (float w) (float h) 4.;
  c##fill;
  c##.font := font;
  c##.fillStyle := opt_style style##.nodeColor (Js.string "black");
  c##.textAlign := Js.string "center";
  c##.textBaseline := Js.string "middle";
  c##fillText (Js.string info) (Js.float (float w /. 2.)) (Js.float (float h /. 2.));
  canvas

let compute_text_nodes node_names nodes =
  let names =
    try fst (List.assq !language node_names)
    with Not_found -> (
      try fst (List.assq (Js.string "en") node_names)
      with Not_found -> Hashtbl.create 11)
  in
  Html.document##.title :=
    Js.string (try Hashtbl.find names "<TITLE>" with Not_found -> "");
  for i = 0 to Array.length nodes - 1 do
    match nodes.(i) with
    | neigh, `Txt (is_root, _, info) ->
        let canvas =
          try Some (compute_text_node (Hashtbl.find names info)) with Not_found -> None
        in
        nodes.(i) <- neigh, `Txt (is_root, canvas, info)
    | _ -> ()
  done

let make_node info is_root _children =
  if String.length info = 0
  then `None
  else if info.[0] = '|'
  then image_node (String.sub info 1 (String.length info - 1))
  else `Txt (is_root, None, info)

let compute_nodes node =
  let rec compute is_root n =
    let (Node (info, ch)) = n in
    let info = make_node (Js.to_string info) is_root ch in
    let ch = Array.map (fun n -> compute false n) ch in
    Node (info, ch)
  in
  compute true node

let compute_neighbors nodes tree =
  let frontiers = Array.make (Array.length nodes) ([||], [||]) in
  let node_info (Node (i, _)) = i in
  let status i =
    let _, info = nodes.(i) in
    i, info <> `None
  in
  let rec compute_frontiers node =
    let (Node (i, l)) = node in
    Array.iter compute_frontiers l;
    frontiers.(i) <-
      (if Array.length l = 0
       then [| status i |], [| status i |]
       else
         fst frontiers.(node_info l.(0)), snd frontiers.(node_info l.(Array.length l - 1)))
    (*
      (i :: fst frontiers.(node_info (List.hd l)),
       i :: snd frontiers.(node_info (list_tl l)))
*)
  in
  compute_frontiers tree;
  let neighboors = Array.make (Array.length nodes) [||] in
  let rec compute_neigh node parent lft rght =
    let (Node (i, ch)) = node in
    let children = Array.map (fun n -> status (node_info n)) ch in
    neighboors.(i) <- Array.concat [ parent; lft; rght; children ];
    let is_root = Array.length parent = 0 in
    let n = Array.length ch in
    for j = 0 to n - 1 do
      let lft =
        if j > 0
        then snd frontiers.(node_info ch.(j - 1))
        else if is_root
        then snd frontiers.(node_info ch.(n - 1))
        else lft
      in
      let rght =
        if j < n - 1
        then fst frontiers.(node_info ch.(j + 1))
        else if is_root
        then fst frontiers.(node_info ch.(0))
        else rght
      in
      compute_neigh ch.(j) [| status i |] lft rght
    done
  in
  compute_neigh tree [||] [||] [||];
  for i = 0 to Array.length nodes - 1 do
    let _l, info = nodes.(i) in
    nodes.(i) <- neighboors.(i), info
  done

let weight_sum l =
  Array.fold_left
    (fun s n ->
      let (Node (w, _)) = n in
      s +. w)
    0.
    l

let rec compute_weights node =
  let (Node (_, l)) = node in
  if Array.length l = 0
  then Node (1., [||])
  else
    let l' = Array.map compute_weights l in
    Node (0.6 *. weight_sum l', l')

let array_map2 f a1 a2 =
  let l = Array.length a1 in
  assert (Array.length a2 = l);
  if l = 0
  then [||]
  else
    let r = Array.make l (f a1.(0) a2.(0)) in
    for i = 1 to l - 1 do
      r.(i) <- f a1.(i) a2.(i)
    done;
    r

let tree_layout node_names root =
  let root = compute_nodes root in
  let node_count = tree_vertice_count root in
  let vertices = Array.make node_count zero in
  let edges = Array.make (tree_edge_count root) (0, 0, 0.) in
  let nodes = Array.make node_count ([||], `None) in
  let vi = ref 0 in
  let ei = ref 0 in
  let weights = compute_weights root in
  let rec layout_rec current current_weights is_root transf lineWidth dir =
    let (Node (info, ch)) = current in
    let (Node (_, ch_weights)) = current_weights in
    let i = !vi in
    incr vi;
    vertices.(i) <- fst transf;
    let l = Array.length ch in
    let ch' =
      if l = 0
      then [||]
      else
        let j = ref 0 in
        let total_weight = weight_sum ch_weights in
        let w0 =
          ref
            (if is_root
             then
               let (Node (w, _)) = ch_weights.(0) in
               (total_weight -. w) /. 2.
             else 0.)
        in
        array_map2
          (fun node weights ->
            let (Node (w, _)) = weights in
            let angle = acos dir.x *. w /. total_weight in
            let th = acos dir.x *. (((!w0 +. (w /. 2.)) /. total_weight *. 2.) -. 1.) in
            incr j;
            w0 := !w0 +. w;
            let min_angle = pi /. 3. in
            let a =
              { x = (cos angle -. cos min_angle) /. (1. -. cos (min_angle +. angle))
              ; y = 0.
              }
            in
            let dir = { x = cos min_angle; y = sin min_angle } in
            let a, dir =
              if a.x > 0.25
              then a, dir
              else
                let a = { x = 0.25; y = 0. } in
                a, transl (neg a) { x = cos angle; y = sin angle }
            in
            let u = compose (a, one) (zero, { x = cos th; y = sin th }) in
            let (Node (i', _) as ch) =
              layout_rec node weights false (compose u transf) (lineWidth *. 0.94) dir
            in
            let k = !ei in
            incr ei;
            edges.(k) <- i, i', lineWidth;
            ch)
          ch
          ch_weights
    in
    nodes.(i) <- [||], info;
    Node (i, ch')
  in
  let tree = layout_rec root weights true (zero, one) 6. { x = -1.; y = 0. } in
  compute_neighbors nodes tree;
  let boxes =
    { bx = Array.make node_count 0.
    ; by = Array.make node_count 0.
    ; bw = Array.make node_count 0.
    ; bh = Array.make node_count 0.
    }
  in
  compute_text_nodes node_names nodes;
  vertices, edges, nodes, boxes

let load_tree () =
  getfile tree_url
  >>= fun s ->
  let info :
      Js.js_string Js.t tree
      * (Js.js_string Js.t
        * (Js.js_string Js.t * Js.js_string Js.t) array
        * Js.js_string Js.t)
        array =
    json##parse (Js.string s)
  in
  let tree, node_names = info in
  randomize_tree tree;
  let node_names =
    node_names
    >> Array.map (fun (lang, tbl, about) ->
           let h = Hashtbl.create 101 in
           Array.iter (fun (k, v) -> Hashtbl.add h (Js.to_string k) (Js.to_string v)) tbl;
           lang, (h, about))
    >> Array.to_list
  in
  Lwt.return (tree_layout node_names tree, node_names)

type info =
  { name : Js.js_string Js.t
  ; url : Js.js_string Js.t
  ; attribution : Js.js_string Js.t
  ; width : int
  ; height : int
  ; links : (Js.js_string Js.t * Js.js_string Js.t * Js.js_string Js.t) array
  ; img_url : Js.js_string Js.t option
  }

let load_image_info () : info array Lwt.t =
  getfile "image_info.json" >>= fun s -> Lwt.return (json##parse (Js.string s))

let close_button over =
  let color = opt_style style##.buttonColor (Js.string "#888888") in
  let size = 32 in
  let offset = 4. in
  let lw = 4. in
  let canvas = create_canvas size size in
  let c = canvas##getContext Html._2d_ in
  c##save;
  c##.lineWidth := Js.float 2.;
  c##.strokeStyle := color;
  if over
  then (
    c##.shadowBlur := Js.float offset;
    c##.shadowColor := color);
  c##beginPath;
  let a = offset +. (lw /. sqrt 2.) in
  let b = float size -. offset -. (lw /. sqrt 2.) in
  c##moveTo (Js.float a) (Js.float a);
  c##lineTo (Js.float b) (Js.float b);
  c##moveTo (Js.float a) (Js.float b);
  c##lineTo (Js.float b) (Js.float a);
  c##stroke;
  c##restore;
  canvas##.className := Js.string (if over then "on" else "off");
  canvas##.style##.position := Js.string "absolute";
  canvas##.style##.top := Js.string "0";
  canvas##.style##.right := Js.string "0";
  canvas

let img_button ?href h src =
  let doc = Html.document in
  let decoration over =
    let img = Html.createImg doc in
    img##.src := icon src;
    let div = Html.createDiv doc in
    div##.style##.position := Js.string "absolute";
    div##.style##.width := Js.string "38px";
    div##.style##.height := Js.string (string_of_int (max 38 h) ^ "px");
    div##.style##.margin := Js.string "2px";
    (Js.Unsafe.coerce div##.style)##.borderRadius := Js.string "2px";
    let extra = max 6 (44 - h) in
    div##.style##.padding
    := Js.string
         (*
      (Format.sprintf "%dpx 3px %dpx 3px" (extra / 2) (extra - extra / 2));
*)
         (string_of_int (extra / 2)
         ^ "px 3px "
         ^ string_of_int (extra - (extra / 2))
         ^ "px 3px");
    div##.className := Js.string ("filled_button " ^ if over then "on" else "off");
    Dom.appendChild div img;
    div
  in
  let button = Html.createDiv doc in
  button##.className := Js.string "button";
  button##.style##.width := Js.string "48px";
  button##.style##.height := Js.string (string_of_int (8 + max 38 h) ^ "px");
  let container =
    match href with
    | None -> (button :> Html.element Js.t)
    | Some url ->
        let a = Html.createA doc in
        a##.target := Js.string "_blank";
        a##.href := url;
        Dom.appendChild button a;
        (a :> Html.element Js.t)
  in
  Dom.appendChild container (decoration true);
  Dom.appendChild container (decoration false);
  button

let tooltip txt =
  let tooltip = Html.createDiv Html.document in
  tooltip##.style##.position := Js.string "absolute";
  tooltip##.className := Js.string "tooltip on";
  tooltip##.innerHTML := txt;
  tooltip

let show_on_click button txt =
  let activated = ref false in
  button##.onclick :=
    Html.handler (fun ev ->
        if not !activated
        then (
          activated := true;
          let c = ref Js.null in
          c :=
            Js.some
              (Html.addEventListener
                 Html.document
                 Html.Event.click
                 (Html.handler (fun _ev ->
                      ignore
                        (Lwt_js.yield ()
                        >>= fun () ->
                        Js.Opt.iter !c Html.removeEventListener;
                        txt##.className := Js.string "text on";
                        activated := false;
                        Lwt.return ());
                      Js._true))
                 Js._true);
          txt##.className := Js.string "text");
        Html.stopPropagation ev;
        Js._false)

let show_image all_messages image_info name small_image =
  image_info
  >>= fun image_info ->
  let messages = local_messages all_messages in
  (*
  let suffix_re = Regexp.regexp "\\.[^.]*$" in
  let name = Js.string (Regexp.global_replace suffix_re name "") in
*)
  let i = ref (-1) in
  let name_js = Js.string name in
  for j = 0 to Array.length image_info - 1 do
    if name_js == image_info.(j).name then i := j
  done;
  if !i >= 0
  then (
    let info = image_info.(!i) in
    Firebug.console##log_2 name !i;
    let d = Html.document in
    let container = Html.createDiv d in
    container##.style##.margin := Js.string "10px";
    container##.style##.position := Js.string "absolute";
    container##.style##.top := Js.string "0";
    container##.style##.bottom := Js.string "0";
    container##.style##.left := Js.string "0";
    container##.style##.right := Js.string "0";
    let img_container = Html.createDiv d in
    img_container##.style##.position := Js.string "absolute";
    img_container##.style##.top := Js.string "0";
    img_container##.style##.bottom := Js.string "4em";
    img_container##.style##.left := Js.string "38px";
    img_container##.style##.right := Js.string "38px";
    let wrap elt =
      let w = Html.createDiv d in
      w##.style##.position := Js.string "absolute";
      w##.style##.top := Js.string "0";
      w##.style##.bottom := Js.string "0";
      w##.style##.left := Js.string "0";
      w##.style##.right := Js.string "0";
      w##.style##.margin := Js.string "auto";
      (*
      w##style##width <- Js.string "100%";
      w##style##height <- Js.string "100%";
      w##style##maxWidth <- Js.string (string_of_int info.width ^ "px");
*)
      w##.style##.maxHeight := Js.string (string_of_int info.height ^ "px");
      Dom.appendChild w elt;
      w
    in
    let img = Html.createImg d in
    (match Lwt.poll (Lazy.force small_image) with
    | Some small_image ->
        let canvas = create_canvas info.width info.height in
        let c = canvas##getContext Html._2d_ in
        c##drawImage_withSize
          small_image
          (Js.float 0.)
          (Js.float 0.)
          (Js.float (float info.width))
          (Js.float (float info.height));
        canvas##.style##.display := Js.string "block";
        canvas##.style##.height := Js.string "auto";
        canvas##.style##.width := Js.string "auto";
        canvas##.style##.maxWidth := Js.string "100%";
        canvas##.style##.maxHeight := Js.string "100%";
        canvas##.style##.marginLeft := Js.string "auto";
        canvas##.style##.marginRight := Js.string "auto";
        let w = wrap canvas in
        Dom.appendChild img_container w;
        img##.onload :=
          Html.handler (fun _ ->
              Dom.removeChild img_container w;
              Js._false)
    | None -> ());
    (img##.src :=
       match info.img_url with
       | None -> Js.string ("images/" ^ name ^ ".jpg")
       | Some url -> url);
    img##.width := info.width;
    img##.height := info.height;
    img##.style##.display := Js.string "block";
    img##.style##.height := Js.string "auto";
    img##.style##.width := Js.string "auto";
    img##.style##.maxWidth := Js.string "100%";
    img##.style##.maxHeight := Js.string "100%";
    img##.style##.marginLeft := Js.string "auto";
    img##.style##.marginRight := Js.string "auto";
    let w = wrap img in
    let handle_error _ =
      Dom.removeChild img_container w;
      Js._false
    in
    img##.onerror := Html.handler handle_error;
    img##.onabort := Html.handler handle_error;
    Dom.appendChild img_container w;
    Dom.appendChild container img_container;
    let legend = Html.createDiv d in
    legend##.innerHTML := info.attribution;
    (*
    let p = Html.createP d in
    p##innerHTML <-
      Js.string ("See the <a target=\"_blank\" href=\"" ^
                  Js.to_string info.url ^ "\">image \
                  description page</a> on Wikimedia Commons for more \
                  information.");
    Dom.appendChild legend p;
*)
    legend##.onclick :=
      Html.handler (fun ev ->
          Html.stopPropagation ev;
          Js._true);
    legend##.className := Js.string "text";
    legend##.style##.position := Js.string "absolute";
    legend##.style##.bottom := Js.string "0";
    legend##.style##.marginRight := Js.string "auto";
    legend##.style##.marginLeft := Js.string "auto";
    Dom.appendChild container legend;
    let background = Html.createDiv d in
    Dom.appendChild background container;
    background##.className := Js.string "overlay";
    background##.style##.width := Js.string "100%";
    background##.style##.height := Js.string "100%";
    background##.style##.position := Js.string "absolute";
    background##.style##.top := Js.string "0";
    background##.style##.left := Js.string "0";
    background##.style##.zIndex := Js.string "1";
    let button = Html.createDiv d in
    button##.className := Js.string "button";
    button##.style##.position := Js.string "absolute";
    button##.style##.top := Js.string "0";
    button##.style##.right := Js.string "0";
    button##.style##.cursor := Js.string "pointer";
    Dom.appendChild button (close_button true);
    Dom.appendChild button (close_button false);
    let tt =
      tooltip
        (opt_style messages##.close (Js.string "Click anywhere to return to the tree"))
    in
    tt##.style##.right := Js.string "32px";
    tt##.style##.top := Js.string "20px";
    Dom.appendChild button tt;
    Dom.appendChild background button;
    let buttons = Html.createDiv d in
    buttons##.style##.position := Js.string "absolute";
    buttons##.style##.top := Js.string "0";
    buttons##.style##.left := Js.string "0";
    let url =
      let suffix =
        if !language == Js.string "en" then "" else "?uselang=" ^ Js.to_string !language
      in
      "http://commons.wikimedia.org/wiki/File:" ^ Js.to_string info.url ^ suffix
    in
    let commons = img_button ~href:(Js.string url) 52 "commons-38.png" in
    let tt =
      tooltip
        (opt_style
           messages##.wikimediaCommons
           (Js.string "See image description on Wikimedia Commons"))
    in
    tt##.style##.left := Js.string "48px";
    tt##.style##.top := Js.string "12px";
    Dom.appendChild commons tt;
    Dom.appendChild buttons commons;
    let wikipedia = img_button 34 "wikipedia-38.png" in
    let txt = Html.createDiv d in
    let count = ref 0 in
    let dl = Html.createDl d in
    let list title lang =
      let ul = Html.createUl d in
      let empty = ref true in
      Array.iter
        (fun (name, lang', refer) ->
          if lang' == lang
          then (
            empty := false;
            let a = Html.createA d in
            Dom.appendChild a (d##createTextNode name);
            a##.target := Js.string "_blank";
            let refer = Js.to_string refer in
            let url =
              "http://"
              ^ Js.to_string lang
              ^ ".wikipedia.org/wiki/"
              ^ if String.length refer = 0 then Js.to_string name else refer
            in
            a##.href := Js.string url;
            let li = Html.createLi d in
            Dom.appendChild li a;
            Dom.appendChild ul li))
        info.links;
      if not !empty
      then (
        incr count;
        let dd = Html.createDd d in
        Dom.appendChild dd (d##createTextNode title);
        Dom.appendChild dl dd;
        let dt = Html.createDt d in
        Dom.appendChild dt ul;
        Dom.appendChild dl dt)
    in
    list (opt_style messages##.language (Js.string "In English")) !language;
    if !language != Js.string "en" then list (Js.string "In English") (Js.string "en");
    if !count > 0
    then Dom.appendChild txt dl
    else
      Dom.appendChild
        txt
        (d##createTextNode (opt_style messages##.noRef (Js.string "No reference found.")));
    txt##.className := Js.string "text on";
    txt##.style##.position := Js.string "absolute";
    txt##.style##.left := Js.string "48px";
    txt##.style##.top := Js.string "62px";
    txt##.style##.whiteSpace := Js.string "nowrap";
    Dom.appendChild wikipedia txt;
    Dom.appendChild buttons wikipedia;
    txt##.onclick :=
      Html.handler (fun ev ->
          Html.stopPropagation ev;
          Js._true);
    show_on_click wikipedia txt;
    buttons##.onclick :=
      Html.handler (fun ev ->
          Html.stopPropagation ev;
          Js._true);
    Dom.appendChild background buttons;
    Dom.appendChild d##.body background;
    background##.onclick :=
      Html.handler (fun _ ->
          Dom.removeChild d##.body background;
          Js._true));
  Lwt.return 0

let information_en =
  Js.string
    "<h1>A tree of animals</h1> This <a target=\"_blank\" \
     href=\"http://en.wikipedia.org/wiki/Phylogenetic_tree\"><em>phylogenetic</em> \
     tree</a> displays the relationships among animals.<h2>Usage</h2>Browse the tree by \
     dragging it with the mouse. Click on any image to display a larger \
     version.<h2>Credits</h2>This software and the images it uses are under free \
     licenses. Images are from <a target=\"_blank\" \
     href=\"http://commons.wikimedia.org/wiki/Main_Page\">Wikimedia Commons</a>.  You \
     can click on each image for details.  The software has been written by Jérôme \
     Vouillon (CNRS)."

let show_information_page messages tree_i18n =
  let info =
    try snd (List.assq !language tree_i18n)
    with Not_found -> (
      try snd (List.assq (Js.string "en") tree_i18n) with Not_found -> information_en)
  in
  let doc = Html.document in
  let txt = Html.createDiv doc in
  txt##.className := Js.string "text";
  txt##.style##.width := Js.string "80%";
  txt##.style##.margin := Js.string "auto";
  txt##.innerHTML := info;
  let cell = Html.createDiv doc in
  cell##.style##.display := Js.string "table-cell";
  cell##.style##.verticalAlign := Js.string "middle";
  Dom.appendChild cell txt;
  let table = Html.createDiv doc in
  table##.style##.width := Js.string "100%";
  table##.style##.height := Js.string "100%";
  table##.style##.display := Js.string "table";
  Dom.appendChild table cell;
  let overlay = Html.createDiv doc in
  overlay##.className := Js.string "overlay translucent";
  Dom.appendChild overlay table;
  let c = ref Js.null in
  let close_info () =
    Dom.removeChild doc##.body overlay;
    Js.Opt.iter !c Html.removeEventListener
  in
  c :=
    Js.some
      (Html.addEventListener
         Html.document
         Html.Event.keydown
         (Html.handler (fun e ->
              match e##.keyCode with
              | 27 | 13 ->
                  close_info ();
                  Js._false
              | _ -> Js._true))
         Js._true);
  let button = Html.createButton doc in
  Dom.appendChild button (doc##createTextNode (opt_style messages##.ok (Js.string "OK")));
  button##.onclick :=
    Html.handler (fun _ ->
        close_info ();
        Js._false);
  let button_div = Html.createDiv doc in
  button_div##.style##.textAlign := Js.string "center";
  button_div##.style##.margin := Js.string "2em auto";
  Dom.appendChild button_div button;
  Dom.appendChild txt button_div;
  Dom.appendChild doc##.body overlay

let unsupported_messages () =
  let doc = Html.document in
  let txt = Html.createDiv doc in
  txt##.className := Js.string "text";
  txt##.style##.width := Js.string "80%";
  txt##.style##.margin := Js.string "auto";
  txt##.innerHTML :=
    Js.string
      "Unfortunately, this browser is not supported. Please try again with another \
       browser, such as <a href=\"http://www.mozilla.org/firefox/\">Firefox</a>, <a \
       href=\"http://www.google.com/chrome/\">Chrome</a> or <a \
       href=\"http://www.opera.com/\">Opera</a>.";
  let cell = Html.createDiv doc in
  cell##.style##.display := Js.string "table-cell";
  cell##.style##.verticalAlign := Js.string "middle";
  Dom.appendChild cell txt;
  let table = Html.createDiv doc in
  table##.style##.width := Js.string "100%";
  table##.style##.height := Js.string "100%";
  table##.style##.display := Js.string "table";
  Dom.appendChild table cell;
  let overlay = Html.createDiv doc in
  overlay##.className := Js.string "overlay";
  Dom.appendChild overlay table;
  Dom.appendChild doc##.body overlay

let _ =
  (*
Random.self_init ();
*)
  (* Prefetch icons. *)
  List.iter (fun src -> ignore (load_image (icon src))) icons

let all_messages = load_messages ()

let tree_info = load_tree ()

let image_info = load_image_info ()

let start _ =
  Lwt.ignore_result
    (tree_info
    >>= fun ((vertices, edges, nodes, boxes), tree_i18n) ->
    all_messages
    >>= fun all_messages ->
    let doc = Html.document in
    let page = doc##.documentElement in
    page##.style##.overflow := Js.string "hidden";
    page##.style##.height := Js.string "100%";
    doc##.body##.style##.overflow := Js.string "hidden";
    doc##.body##.style##.margin := Js.string "0px";
    doc##.body##.style##.height := Js.string "100%";
    let w = page##.clientWidth in
    let h = page##.clientHeight in
    let canvas = create_canvas w h in
    Dom.appendChild doc##.body canvas;
    let tr = ref (zero, one) in
    let tr' = ref !tr in
    let vertices' = Array.copy vertices in
    (redraw_funct :=
       fun () ->
         need_redraw := false;
         Firebug.console##time (Js.string "transform");
         (*
          let transf = hyp_transf !tr' in
          for i = 0 to Array.length vertices - 1 do
            vertices'.(i) <- transf vertices.(i)
          done;
*)
         let w = page##.clientWidth in
         let h = page##.clientHeight in
         if w <> canvas##.width || h <> canvas##.height
         then (
           canvas##.width := w;
           canvas##.height := h);
         hyp_transf_vect !tr' vertices vertices';
         Firebug.console##timeEnd (Js.string "transform");
         draw canvas vertices' edges nodes boxes);
    perform_redraw ();
    Html.window##.onresize :=
      Html.handler (fun _ ->
          let page = doc##.documentElement in
          let w = page##.clientWidth in
          let h = page##.clientHeight in
          (*
debug_msg (Format.sprintf "Resize %d %d" w h);
*)
          if w <> canvas##.width || h <> canvas##.height
          then
            (*
            canvas##width <- w;
            canvas##height <- h;
            perform_redraw ()
*)
            schedule_redraw ();
          Js._true);
    (*
     let eventually t f =
       let scheduled = ref false in
       fun () ->
         if not !scheduled then begin
           scheduled := true;
           ignore
             (Lwt_js.sleep t >>= fun () ->
              scheduled := false;
              f ();
              Lwt.return ())
         end
     in
*)
    let find_box boxes x y =
      let p = ref (-1) in
      for i = 0 to Array.length boxes.bw - 1 do
        if
          Array.unsafe_get boxes.bw i > 0.
          && abs_float (x -. Array.unsafe_get boxes.bx i) < Array.unsafe_get boxes.bw i
          && abs_float (y -. Array.unsafe_get boxes.by i) < Array.unsafe_get boxes.bh i
        then p := i
      done;
      !p
    in
    let on_image = ref false in
    let update_cursor x y =
      let i = find_box boxes x y in
      if i <> -1
      then (
        if not !on_image
        then (
          canvas##.style##.cursor := Js.string "pointer";
          on_image := true))
      else if !on_image
      then (
        canvas##.style##.cursor := Js.string "";
        on_image := false)
    in
    canvas##.onmousemove :=
      Html.handler (fun ev ->
          update_cursor (Js.to_float ev##.clientX) (Js.to_float ev##.clientY);
          Js._false);
    handle_drag
      canvas
      (fun x0 y0 x1 y1 ->
        let z0 = from_screen canvas x0 y0 in
        let z1 = from_screen canvas x1 y1 in
        (*
         (* Transformation from z0 to z1:
            z1 = (z0 + p) / (conj p.z0 + 1)
            ==> p = (z1.z0.conj (z1 - z0) + z1 - z0) / (1 - |z1.z0|^2) *)
         let dz = sub z1 z0 in
         let z0z1 = mul z0 z1 in
         let p = sdiv (add (mul z0z1 (conj dz)) dz) (1. -. sq_norm z0z1) in
         tr' := compose !tr (p, one);
*)
        let p, _ = !tr in
        let z0' = transl (neg p) z0 in
        let p' = compute_translation z0' z1 in
        tr' := p', one;
        schedule_redraw ()) (*perform_redraw ()*)
      (fun x y ->
        tr := !tr';
        on_image := false;
        update_cursor x y)
      (fun x y ->
        let i = find_box boxes x y in
        if i > 0
        then
          match nodes.(i) with
          | _, `Img (img, name) -> ignore (show_image all_messages image_info name img)
          | _ -> ());
    handle_touch_events
      canvas
      (fun x0 y0 x1 y1 ->
        Firebug.console##time (Js.string "transform");
        let z0 = from_screen canvas x0 y0 in
        let z1 = from_screen canvas x1 y1 in
        (*
         (* Transformation from z0 to z1:
            z1 = (z0 + p) / (conj p.z0 + 1)
            ==> p = (z1.z0.conj (z1 - z0) + z1 - z0) / (1 - |z1.z0|^2) *)
         let dz = sub z1 z0 in
         let z0z1 = mul z0 z1 in
         let p = sdiv (add (mul z0z1 (conj dz)) dz) (1. -. sq_norm z0z1) in
         tr' := compose !tr (p, one);
*)
        let p, _ = !tr in
        let z0' = transl (neg p) z0 in
        let p' = compute_translation z0' z1 in
        tr' := p', one;
        schedule_redraw ())
      (fun _ _ -> tr := !tr')
      (fun _ _ -> tr := !tr')
      (fun x y ->
        let i = find_box boxes x y in
        if i > 0
        then
          match nodes.(i) with
          | _, `Img (img, name) -> ignore (show_image all_messages image_info name img)
          | _ -> ());
    let handle_key_event ev =
      match ev##.keyCode with
      | 37 ->
          (* left *)
          let z0 = { x = 0.; y = 0. } in
          let z1 = { x = 0.1; y = 0. } in
          let p, _ = !tr in
          let z0' = transl (neg p) z0 in
          let p' = compute_translation z0' z1 in
          tr' := p', one;
          tr := !tr';
          schedule_redraw ();
          Js._false
      | 38 ->
          (* up *)
          let z0 = { x = 0.; y = 0. } in
          let z1 = { x = 0.; y = 0.1 } in
          let p, _ = !tr in
          let z0' = transl (neg p) z0 in
          let p' = compute_translation z0' z1 in
          tr' := p', one;
          tr := !tr';
          schedule_redraw ();
          Js._false
      | 39 ->
          (* right *)
          let z0 = { x = 0.; y = 0. } in
          let z1 = { x = -0.1; y = 0. } in
          let p, _ = !tr in
          let z0' = transl (neg p) z0 in
          let p' = compute_translation z0' z1 in
          tr' := p', one;
          tr := !tr';
          schedule_redraw ();
          Js._false
      | 40 ->
          (* down *)
          let z0 = { x = 0.; y = 0. } in
          let z1 = { x = 0.; y = -0.1 } in
          let p, _ = !tr in
          let z0' = transl (neg p) z0 in
          let p' = compute_translation z0' z1 in
          tr' := p', one;
          tr := !tr';
          schedule_redraw ();
          Js._false
      | _ -> Js._true
    in
    ignore
      (Html.addEventListener
         Html.document
         Html.Event.keydown
         (Html.handler handle_key_event)
         Js._true);
    let prev_buttons = ref None in
    let rec make_buttons () =
      (match !prev_buttons with
      | None -> ()
      | Some buttons -> Dom.removeChild doc##.body buttons);
      let buttons = Html.createDiv doc in
      buttons##.style##.position := Js.string "absolute";
      buttons##.style##.right := Js.string "0";
      buttons##.style##.bottom := Js.string "0";
      let messages = local_messages all_messages in
      let info = img_button 38 "info-38.png" in
      info##.style##.position := Js.string "absolute";
      info##.style##.bottom := Js.string "2px";
      info##.style##.right := Js.string "0";
      info##.style##.cursor := Js.string "pointer";
      info##.onclick :=
        Html.handler (fun _ ->
            show_information_page messages tree_i18n;
            Js._false);
      let tt = tooltip (opt_style messages##.info (Js.string "Information")) in
      tt##.style##.right := Js.string "36px";
      tt##.style##.bottom := Js.string "36px";
      Dom.appendChild info tt;
      Dom.appendChild buttons info;
      let lang = img_button 38 "globe-38.png" in
      lang##.style##.position := Js.string "absolute";
      lang##.style##.bottom := Js.string "2px";
      lang##.style##.right := Js.string "48px";
      lang##.style##.cursor := Js.string "pointer";
      let languages = [ "Français", "fr"; "English", "en" ] in
      let txt = Html.createDiv doc in
      let dl = Html.createDl doc in
      let ul = Html.createUl doc in
      List.iter
        (fun (name, id) ->
          let a = Html.createA doc in
          Dom.appendChild a (doc##createTextNode (Js.string name));
          a##.href := Js.string "#";
          a##.onclick :=
            Html.handler (fun _ ->
                set_language (Js.string id);
                make_buttons ();
                compute_text_nodes tree_i18n nodes;
                schedule_redraw ();
                Js._false);
          let li = Html.createLi doc in
          Dom.appendChild li a;
          Dom.appendChild ul li)
        languages;
      let dd = Html.createDd doc in
      Dom.appendChild
        dd
        (doc##createTextNode (opt_style messages##.languages (Js.string "Languages")));
      Dom.appendChild dl dd;
      let dt = Html.createDt doc in
      Dom.appendChild dt ul;
      Dom.appendChild dl dt;
      Dom.appendChild txt dl;
      txt##.className := Js.string "text on";
      txt##.style##.position := Js.string "absolute";
      txt##.style##.right := Js.string "0px";
      txt##.style##.bottom := Js.string "46px";
      txt##.style##.whiteSpace := Js.string "nowrap";
      Dom.appendChild lang txt;
      show_on_click lang txt;
      Dom.appendChild buttons lang;
      let recenter = img_button 38 "meeting-point-38.png" in
      recenter##.style##.position := Js.string "absolute";
      recenter##.style##.bottom := Js.string "2px";
      recenter##.style##.right := Js.string "96px";
      recenter##.style##.cursor := Js.string "pointer";
      recenter##.onclick :=
        Html.handler (fun _ ->
            tr' := zero, one;
            tr := !tr';
            schedule_redraw ();
            Js._false);
      let tt = tooltip (opt_style messages##.recenter (Js.string "Recenter")) in
      tt##.style##.right := Js.string "36px";
      tt##.style##.bottom := Js.string "36px";
      Dom.appendChild recenter tt;
      Dom.appendChild buttons recenter;
      Dom.appendChild doc##.body buttons;
      prev_buttons := Some buttons
    in
    make_buttons ();
    let img = Html.createImg doc in
    img##.src := icon "ocsigen-powered.png";
    let a = Html.createA doc in
    a##.target := Js.string "_blank";
    a##.href := Js.string "http://ocsigen.org/";
    Dom.appendChild a img;
    let logo = Html.createDiv doc in
    logo##.style##.position := Js.string "absolute";
    logo##.style##.left := Js.string "0";
    logo##.style##.bottom := Js.string "0";
    Dom.appendChild logo a;
    Dom.appendChild doc##.body logo;
    Lwt.return ());
  Js._false

let start _ =
  try
    ignore (Html.createCanvas Html.window##.document);
    start ()
  with Html.Canvas_not_available ->
    unsupported_messages ();
    Js._false

let _ = Html.window##.onload := Html.handler start
