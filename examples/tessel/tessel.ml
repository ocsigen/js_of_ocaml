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

(*
- Use mouse events to rotate the sphere
  ==> put lightnings on the texture
- Options: shadows / clipped / rotating / summer or winter or equinox
*)

type vertex = { x : float; y : float; z : float }

let vertex x y z = { x = x; y = y; z = z }

type face = { v1 : int; v2 : int; v3 : int }

let face v1 v2 v3 = { v1 = v1; v2 = v2; v3 = v3 };

type t = { vertices : vertex array; faces : face array }

let octahedron =
  { vertices =
      [| vertex   0.   0.   1.;
         vertex   1.   0.   0.;
         vertex   0.   1.   0.;
         vertex (-1.)  0.   0.;
         vertex   0. (-1.)  0.;
         vertex   0.   0. (-1.) |];
    faces =
      [| face 0 1 2;
         face 0 2 3;
         face 0 3 4;
         face 0 4 1;
         face 1 5 2;
         face 1 4 5;
         face 3 5 4;
         face 3 2 5 |] }

let vect {x = x1; y = y1; z = z1} {x = x2; y = y2; z = z2} =
  {x = x2 -. x1; y = y2 -. y1; z = z2 -. z1}

let cross_product {x = x1; y = y1; z = z1} {x = x2; y = y2; z = z2} =
  {x = y1 *. z2 -. y2 *. z1;
   y = z1 *. x2 -. z2 *. x1;
   z = x1 *. y2 -. x2 *. y1}

let dot_product {x = x1; y = y1; z = z1} {x = x2; y = y2; z = z2} =
  x1 *. x2 +. y1 *. y2 +. z1 *. z2

let normalize v =
  let { x = x; y = y; z = z } = v in
  let r = sqrt (x *. x +. y *. y +. z *. z) in
  { x = x /. r; y = y /. r; z = z /. r }

let divide o =
  let vn = Array.length o.vertices + Array.length o.faces * 3 / 2 in
  let vertices = Array.make vn (vertex 0. 0. 0.) in
  let j = ref (Array.length o.vertices) in
  Array.blit o.vertices 0 vertices 0 !j;
  let fn = 4 * Array.length o.faces in
  let faces = Array.make fn (face 0 0 0) in
  let midpoints = Hashtbl.create 17 in
  let midpoint v1 v2 =
    let p = if v1 < v2 then (v1, v2) else (v2, v1) in
    try
      Hashtbl.find midpoints p
    with Not_found ->
      let v1 = o.vertices.(v1) in
      let v2 = o.vertices.(v2) in
      let v =
        normalize
          { x = (v1.x +. v2.x) /. 2.;
            y = (v1.y +. v2.y) /. 2.;
            z = (v1.z +. v2.z) /. 2. }
      in
      let res = !j in
assert (res < Array.length vertices);
      vertices.(res) <- v;
      Hashtbl.add midpoints p res;
      incr j; res
  in
  for i = 0 to Array.length o.faces - 1 do
    let { v1 = v1; v2 = v2; v3 = v3 } = o.faces.(i) in
    let w1 = midpoint v1 v2 in
    let w2 = midpoint v2 v3 in
    let w3 = midpoint v3 v1 in
    faces.(4 * i) <- { v1 = v1; v2 = w1; v3 = w3 };
    faces.(4 * i + 1) <- { v1 = w1; v2 = v2; v3 = w2 };
    faces.(4 * i + 2) <- { v1 = w3; v2 = w2; v3 = v3 };
    faces.(4 * i + 3) <- { v1 = w1; v2 = w2; v3 = w3 }
  done;
  assert (!j = Array.length vertices);
  { vertices = vertices; faces = faces }

(****)

module Html = Dom_html

let create_canvas w h =
  let c = Html.createCanvas Html.document in
  c##width <- w; c##height <- h; c

(****)

let (>>=) = Lwt.bind

let lwt_wrap f =
  let (t, w) = Lwt.task () in
  let cont x = Lwt.wakeup w x in
  f cont;
  t

(****)

let load_image src =
  let img = Html.createImg Html.document in
  lwt_wrap
    (fun c ->
       img##onload <- Html.handler (fun _ -> c (); Js._false); img##src <- src)
    >>= fun () ->
  Lwt.return img

let width = 350
let height = width

let pi = 4. *. atan 1.

let obliquity = 23.5 *. pi /. 180.
let gamma = 2.
let dark = 0.2 ** gamma

(****)

let shade () =
  let r = float width /. 2. *. 0.95 in
  let x0 = float width /. 2. in
  let y0 = float height /. 2. in

  let canvas = create_canvas width height in
  let ctx = canvas##getContext (Html._2d_) in
  let img = ctx##createImageData (width, height) in

  let data = img##data in
  let r2 = r *. r in
  let cst = (1. -. dark) /. r in
  let inv_gamma  = 1. /. gamma in
  for i = 0 to width - 1 do
    for j = 0 to height - 1 do
      let i = float i in
      let j = float j in
      let k = truncate (4. *. (i +. j *. float width)) in
      let x = i -. x0 in
      let y = j -. y0 in
      let z2 = r2 -. x *. x -. y *. y in
      if z2 >= 0. then begin
        if x > 0. then
          Html.pixel_set data (k + 3)
            (255 - truncate (255.99 *. dark ** inv_gamma))
        else
          Html.pixel_set data (k + 3)
            (255 - truncate (255.99 *. (dark -. x *. cst) ** inv_gamma))
      end else
        Html.pixel_set data (k + 3) 255
    done
  done;
  ctx##putImageData (img, 0., 0.);
  canvas

let shadow texture =
Firebug.console##time (Js.string "foo");
  let w = texture##naturalWidth in
  let h = texture##naturalHeight in
  let canvas = create_canvas w h in
  let ctx = canvas##getContext (Html._2d_) in
  let (w, h) = (w / 8, h / 8) in
  let img = ctx##createImageData (w, h) in
  let data = img##data in
  let inv_gamma  = 1. /. gamma in
  let cos_obl = cos obliquity in
  let sin_obl = -. sin obliquity in
  for j = 0 to h - 1 do
    for i = 0 to w / 2 - 1 do
      let k = truncate (4. *. (float i +. float j *. float w)) in
      let k' =
        truncate (4. *. (float w -. float i +. float j *. float w -. 1.)) in
      let theta = (float j /. float h -. 0.5) *. pi in
      let phi = (float i /. float w) *. 2. *. pi in
      let x = cos phi *. cos theta in
      let y = sin theta in
      let (x, y) =
        (x *. cos_obl +. y *. sin_obl,
         -. x *. sin_obl +. y *. cos_obl)
      in
(*
      let z = sin phi *. cos theta in
*)
      let c =
        if x > 0. then
          (255 - truncate (255.99 *. dark ** inv_gamma))
        else
          (255 - truncate (255.99 *. (dark -. x *. (1. -. dark)) ** inv_gamma))
      in
      Html.pixel_set data (k + 3) c;
      Html.pixel_set data (k' + 3) c
    done
  done;
  ctx##putImageData (img, 0., 0.);
  ctx##globalCompositeOperation <- Js.string "copy";
  ctx##save ();
  ctx##scale (8., 8.);
  ctx##drawImage_fromCanvas (canvas, 0., 0.);

ctx##restore ();
(*FIX: does not yield the right alpha... *)
  ctx##globalCompositeOperation <- Js.string "destination-over";
  ctx##drawImage (texture, 0., 0.);

Firebug.console##timeEnd (Js.string "foo");
(*
  Dom.appendChild (Html.document##body) canvas
*)
  canvas

(****)

let to_uv tw th {x = x; y = y; z = z} =
  let cst1 = (tw /. 2. -. 0.99) /. pi in
  let cst2 = th /. 2. in
  let cst3 = (th -. 0.99) /. pi in
  let u =
    mod_float (float (truncate (tw -. atan2 z x *. cst1))) tw in
  let v = float (truncate (cst2 +. asin y *. cst3)) in
assert (0. <= u);
assert (u < tw);
assert (0. <= v);
assert (v < th);
  (u, v)

let min (u : float) v = if u < v then u else v
let max (u : float) v = if u < v then v else u

let draw (ctx : Html.canvasRenderingContext2D Js.t) img o uv normals dir =
  ctx##clearRect(-100., -100., 10000., 10000.);
  let tw = float img##naturalWidth in
  let cos_obl = cos obliquity in
  let sin_obl = -. sin obliquity in
  Array.iteri
    (fun i { v1 = v1; v2 = v2; v3 = v3 } ->
       let {x = x1; y = y1; z = z1} = o.vertices.(v1) in
       let {x = x2; y = y2; z = z2} = o.vertices.(v2) in
       let {x = x3; y = y3; z = z3} = o.vertices.(v3) in
       (* We could do the rotation once and for all for each visible
          point... *)
       let (x1, y1) =
         (x1 *. cos_obl +. y1 *. sin_obl,
          -. x1 *. sin_obl +. y1 *. cos_obl)
       in
       let (x2, y2) =
         (x2 *. cos_obl +. y2 *. sin_obl,
          -. x2 *. sin_obl +. y2 *. cos_obl)
       in
       let (x3, y3) =
         (x3 *. cos_obl +. y3 *. sin_obl,
          -. x3 *. sin_obl +. y3 *. cos_obl)
       in

       if dot_product normals.(i) dir >= 0. then begin
         ctx##beginPath ();
         ctx##moveTo (x1, y1);
         ctx##lineTo (x2, y2);
         ctx##lineTo (x3, y3);
         ctx##closePath ();
         ctx##save();
         ctx##clip ();

let (u1, v1) = uv.(v1) in
let (u2, v2) = uv.(v2) in
let (u3, v3) = uv.(v3) in
let mid = tw /. 2. in
let u1 = if u1 = 0. && (u2 > mid || u3 > mid) then tw -. 1. else u1 in
let u2 = if u2 = 0. && (u1 > mid || u3 > mid) then tw -. 1. else u2 in
let u3 = if u3 = 0. && (u2 > mid || u1 > mid) then tw -. 1. else u3 in
let du2 = u2 -. u1 in
let du3 = u3 -. u1 in
let dv2 = v2 -. v1 in
let dv3 = v3 -. v1 in
let dx2 = x2 -. x1 in
let dx3 = x3 -. x1 in
let dy2 = y2 -. y1 in
let dy3 = y3 -. y1 in
let a = (dx2*.dv3-.dx3*.dv2) /. (du2*.dv3-.du3*.dv2) in
let b = (dx2*.du3-.dx3*.du2) /. (dv2*.du3-.dv3*.du2) in
let c = x1 -. a *. u1 -. b *. v1 in
let d = (dy2*.dv3-.dy3*.dv2) /. (du2*.dv3-.du3*.dv2) in
let e = (dy2*.du3-.dy3*.du2) /. (dv2*.du3-.dv3*.du2) in
let f = y1 -. d *. u1 -. e *. v1 in

ctx##transform (a, d, b, e, c, f);
let u = max 0. (min u1 (min u2 u3) -. 1.) in
let v = max 0. (min v1 (min v2 v3) -. 1.) in
let u' = max u1 (max u2 u3) +. 1. in
let v' = max v1 (max v2 v3) +. 1. in
let du = u' -. u in
let dv = v' -. v in
ctx##drawImage_full (img, u, v, du, dv, u, v, du, dv);
ctx##restore()
       end
    )
    o.faces

let (>>) x f = f x

let o = octahedron >> divide >> divide

let texture = Js.string "../planet/texture.jpg"
let texture = Js.string "../planet/land_ocean_ice_cloud_2048.jpg"

let rotate a o v =
  let cos_a = cos a in
  let sin_a = sin a in
  {o with vertices =
     Array.map
       (fun {x = x; y = y; z = z} ->
          {x = x *. cos_a +. z *. sin_a;
           y = y;
           z = -. x *. sin_a +. z *. cos_a})
       o.vertices},
  let {x = x; y = y; z = z} = v in
  {x = x *. cos_a -. z *. sin_a;
   y = y;
   z = x *. sin_a +. z *. cos_a}

let start _ =
  Lwt.ignore_result
    (load_image texture >>= fun texture ->
  let sh = shadow texture in

  let canvas = create_canvas width height in
  let canvas' = create_canvas width height in
  Dom.appendChild Html.document##body canvas;
  let ctx = canvas##getContext (Html._2d_) in
  let ctx' = canvas'##getContext (Html._2d_) in
(*
  ctx##globalCompositeOperation <- Js.string "copy";
*)
(*
  ctx##lineWidth <- (1. /. 200.);
*)
  let r = float width /. 2. in
(*
  ctx##beginPath ();
  ctx##arc(r, r, r *. 0.95, 0., 2. *. pi, Js._true);
  ctx##clip();
*)
  let tw = float texture##naturalWidth in
  let th = float texture##naturalHeight in
  let uv = Array.map (fun v -> to_uv tw th v) o.vertices in
  let normals =
    Array.map
      (fun {v1 = v1; v2 = v2; v3 = v3} ->
         let v1 = o.vertices.(v1) in
         let v2 = o.vertices.(v2) in
         let v3 = o.vertices.(v3) in
         cross_product (vect v1 v2) (vect v1 v3))
      o.faces
  in

  let sh = shade () in

  let rec loop o v t =
(*
    let t1 = Js.to_float (Js.date##now()) in
*)
    ctx'##clearRect (0., 0., float width, float height);
    ctx'##save ();
    ctx'##setTransform (r -. 2., 0., 0., r -. 2., r, r);
    ctx'##globalCompositeOperation <- Js.string "lighter";
    draw ctx' texture o uv normals v;
    ctx'##restore ();

    ctx'##globalCompositeOperation <- Js.string "copy";
(*
    ctx'##globalCompositeOperation <- Js.string "over";
    ctx'##drawImage_fromCanvas (sh, 0., 0.);
*)
    ctx##drawImage_fromCanvas (canvas', 0., 0.);

(*
    let t2 = Js.to_float (Js.date##now()) in
    Dom.appendChild (Html.document##body) (Html.document##createTextNode (Js.string (Printf.sprintf "%d / %d ==> (%f / %f)       " (Array.length o.vertices) (Array.length o.faces) t2 t1)));
*)
    Lwt_js.sleep 0.05 >>= fun () ->
    let t' = Js.to_float (Js.date##now ()) in
    let dt = t' -. t in
    let dt = if dt < 0. then 0. else if dt > 1000. then 0. else dt in
    let angle = 2. *. pi *. dt /. 1000. /. 10. in
    let (o, v) = rotate angle o v in
(*
Dom.appendChild (Html.document##body) (Html.document##createTextNode (Js.string (Printf.sprintf "(%f/%f/%f)" v.x v.y v.z)));
*)
if true then Lwt.return () else
    loop o v t'
  in
  loop o {x = 0.; y = 0.; z = 1.} (Js.to_float (Js.date##now ()))
); Js._false

let _ =
Html.window##onload <- Html.handler start
