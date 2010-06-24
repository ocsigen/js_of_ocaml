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
- Options: shadows / clipped / rotating / summer or winter or equinox
    reset rotation
  ==> pause (no rotation, no light move) / follow rotation
  ==> larger/smaller
- adaptative size
  ==> time 3 frames and take min
  ==> if fast, try larger image
- precompute as much as possible ==> du/dv/...
*)

let width = 600
let height = width

let pi = 4. *. atan 1.

let obliquity = 23.5 *. pi /. 180.
let gamma = 2.
let dark = 0.2 ** gamma

(****)

type vertex = { x : float; y : float; z : float }

let vertex x y z = { x = x; y = y; z = z }

type matrix = { r1 : vertex; r2 : vertex; r3 : vertex }

let vect {x = x1; y = y1; z = z1} {x = x2; y = y2; z = z2} =
  {x = x2 -. x1; y = y2 -. y1; z = z2 -. z1}

let cross_product {x = x1; y = y1; z = z1} {x = x2; y = y2; z = z2} =
  {x = y1 *. z2 -. y2 *. z1;
   y = z1 *. x2 -. z2 *. x1;
   z = x1 *. y2 -. x2 *. y1}

let dot_product {x = x1; y = y1; z = z1} {x = x2; y = y2; z = z2} =
  x1 *. x2 +. y1 *. y2 +. z1 *. z2

let matrix_vect_mul m {x = x; y = y; z = z} =
  let {r1 = r1; r2 = r2; r3 = r3} = m in
  let x' = x *. r1.x +. y *. r1.y +. z *. r1.z in
  let y' = x *. r2.x +. y *. r2.y +. z *. r2.z in
  let z' = x *. r3.x +. y *. r3.y +. z *. r3.z in
  {x = x'; y = y'; z = z'}

let matrix_transp m =
  let {r1 = r1; r2 = r2; r3 = r3} = m in
  { r1 = { x = r1.x; y = r2.x; z = r3.x };
    r2 = { x = r1.y; y = r2.y; z = r3.y };
    r3 = { x = r1.z; y = r2.z; z = r3.z } }

let matrix_mul m m' =
  let m' = matrix_transp m' in
  { r1 = matrix_vect_mul m' m.r1;
    r2 = matrix_vect_mul m' m.r2;
    r3 = matrix_vect_mul m' m.r3 }

let normalize v =
  let { x = x; y = y; z = z } = v in
  let r = sqrt (x *. x +. y *. y +. z *. z) in
  { x = x /. r; y = y /. r; z = z /. r }

let xz_rotation phi =
  let cos_phi = cos phi in
  let sin_phi = sin phi in
  { r1 = vertex cos_phi 0. sin_phi;
    r2 = vertex 0.      1.  0.;
    r3 = vertex (-. sin_phi) 0. cos_phi }

let xy_rotation phi =
  let cos_phi = cos phi in
  let sin_phi = sin phi in
  { r1 = vertex     cos_phi  sin_phi 0.;
    r2 = vertex (-. sin_phi) cos_phi 0.;
    r3 = vertex          0.       0. 1. }

let yz_rotation phi =
  let cos_phi = cos phi in
  let sin_phi = sin phi in
  { r1 = vertex 1. 0. 0.;
    r2 = vertex 0. cos_phi sin_phi;
    r3 = vertex 0. (-. sin_phi) cos_phi }

(* Assumes that m is orthogonal *)
let rotate_normal m v = matrix_vect_mul (matrix_transp m) v

(****)

type face = { v1 : int; v2 : int; v3 : int }

let face v1 v2 v3 = { v1 = v1; v2 = v2; v3 = v3 };

type t = { vertices : vertex array; faces : face array }

let rotate_object m o =
  {o with vertices = Array.map (fun v -> matrix_vect_mul m v) o.vertices}

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

(****)

(* 0 <= phi < 2pi *)
(* -pi/2 <= theta <= pi/2 *)
let tesselate_sphere p_div t_div =
  let p_delta = 2. *. pi /. float p_div in
  let t_delta = pi /. float t_div in
  let t_offset = (pi -. t_delta) /. 2. in
  let n = t_div * p_div in
  let vertices = Array.make (n + 2) (vertex 0. 0. 0.) in
  let faces = Array.make (n * 2) (face 0 0 0) in
  let north = n and south = n + 1 in
  vertices.(north) <- vertex 0. (-1.) 0.;
  vertices.(south) <- vertex 0. 1. 0.;
  for i = 0 to p_div - 1 do
    for j = 0 to t_div - 1 do
      let phi = float i *. p_delta in
      let theta = float j *. t_delta -. t_offset in
      let x = cos phi *. cos theta in
      let y = sin theta in
      let z = sin phi *. cos theta in
      let k = i * t_div + j in
      vertices.(k) <- vertex x y z;
      if j = 0 then begin
        faces.(2 * k) <- face north k ((k + t_div) mod n);
        faces.(2 * k + 1) <-
          face south ((k + 2 * t_div - 1) mod n) (k + t_div - 1);
      end else begin
        faces.(2 * k) <- face k ((k + t_div) mod n) (k - 1);
        faces.(2 * k + 1) <-
          face (k - 1) ((k + t_div) mod n) ((k + t_div - 1) mod n)
      end
    done
  done;
  { vertices = vertices; faces = faces }

(****)

let divide all o =
  let vn =
    if all then Array.length o.vertices + Array.length o.faces * 3 / 2 else
    Array.length o.vertices + 16
  in
  let vertices = Array.make vn (vertex 0. 0. 0.) in
  let j = ref (Array.length o.vertices) in
  Array.blit o.vertices 0 vertices 0 !j;
  let fn =
    if all then 4 * Array.length o.faces else Array.length o.faces + 24 in
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
        { x = (v1.x +. v2.x) /. 2.;
          y = (v1.y +. v2.y) /. 2.;
          z = (v1.z +. v2.z) /. 2. }
      in
      let v =
        if all || abs_float v1.y = 1. || abs_float v2.y = 1. then
          normalize v
        else
          v
      in
      let res = !j in
assert (res < Array.length vertices);
      vertices.(res) <- v;
      Hashtbl.add midpoints p res;
      incr j; res
  in
  let k = ref 0 in
  for i = 0 to Array.length o.faces - 1 do
    let { v1 = v1; v2 = v2; v3 = v3 } = o.faces.(i) in
    if
      all ||
      abs_float o.vertices.(v1).y = 1. || abs_float o.vertices.(v2).y = 1. ||
      abs_float o.vertices.(v3).y = 1.
    then begin
      let w1 = midpoint v1 v2 in
      let w2 = midpoint v2 v3 in
      let w3 = midpoint v3 v1 in
      faces.(!k) <- { v1 = v1; v2 = w1; v3 = w3 };
      faces.(!k + 1) <- { v1 = w1; v2 = v2; v3 = w2 };
      faces.(!k + 2) <- { v1 = w3; v2 = w2; v3 = v3 };
      faces.(!k + 3) <- { v1 = w1; v2 = w2; v3 = w3 };
      k := !k + 4
    end else begin
      faces.(!k) <- o.faces.(i);
      incr k
    end
  done;
  assert (!j = Array.length vertices);
  assert (!k = Array.length faces);
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
(*
      let z = sin phi *. cos theta in
*)
      let (x, y) =
        (x *. cos_obl +. y *. sin_obl,
         -. x *. sin_obl +. y *. cos_obl)
      in
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
  ctx##scale (8. *. float (w + 2) /. float w, 8. *. float (h + 2) /. float h);
  ctx##translate (-1., -1.);
  ctx##drawImage_fromCanvas (canvas, 0., 0.);
  ctx##restore ();

  let w = texture##naturalWidth in
  let h = texture##naturalHeight in
  let canvas' = create_canvas w h in
  let ctx' = canvas'##getContext (Html._2d_) in

  let update_texture phi =
    let phi = mod_float phi (2. *. pi) in

    ctx'##drawImage (texture, 0., 0.);
    let i = truncate (mod_float ((2. *. pi -. phi) *. float w /. 2. /. pi) (float w)) in
    ctx'##drawImage_fromCanvas (canvas, float i, 0.);
    ctx'##drawImage_fromCanvas (canvas, float i -. float w, 0.)
  in
Firebug.console##timeEnd (Js.string "foo");
(*
  Dom.appendChild Html.document##body canvas';
*)
  (canvas', update_texture)

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

let draw (ctx : Html.canvasRenderingContext2D Js.t) img shd o uv normals dir =
  let tw = float img##naturalWidth in
  let th = float img##naturalHeight in
(*
  let cos_obl = cos obliquity in
  let sin_obl = -. sin obliquity in
*)
  Array.iteri
    (fun i { v1 = v1; v2 = v2; v3 = v3 } ->
       let {x = x1; y = y1; z = z1} = o.vertices.(v1) in
       let {x = x2; y = y2; z = z2} = o.vertices.(v2) in
       let {x = x3; y = y3; z = z3} = o.vertices.(v3) in
(*
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
*)

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

let u1 = if u1 = 0. && (u2 > mid || u3 > mid) then tw -. 2. else u1 in
let u2 = if u2 = 0. && (u1 > mid || u3 > mid) then tw -. 2. else u2 in
let u3 = if u3 = 0. && (u2 > mid || u1 > mid) then tw -. 2. else u3 in

let mth = th -. 2. in
let u1 = if v1 = 0. || v1 >= mth then (u2 +. u3) /. 2. else u1 in
let u2 = if v2 = 0. || v2 >= mth then (u1 +. u3) /. 2. else u2 in
let u3 = if v3 = 0. || v3 >= mth then (u2 +. u1) /. 2. else u3 in

let u1 = max 1. u1 in
let u2 = max 1. u2 in
let u3 = max 1. u3 in

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
let u = max 0. (min u1 (min u2 u3) -. 4.) in
let v = max 0. (min v1 (min v2 v3) -. 4.) in
(*
let u' = max u1 (max u2 u3) +. 4. in
let v' = max v1 (max v2 v3) +. 4. in
*)
let u' = min tw (max u1 (max u2 u3) +. 4.) in
let v' = min th (max v1 (max v2 v3) +. 4.) in
let du = u' -. u in
let dv = v' -. v in
(*
ctx##drawImage_full (img, u, v, du, dv, u, v, du, dv);
*)
ctx##drawImage_fullFromCanvas (shd, u, v, du, dv, u, v, du, dv);
ctx##restore()
       end
    )
    o.faces

let (>>) x f = f x

(*
let o = octahedron >> divide true >> divide true >> divide false
*)
let o = tesselate_sphere 12 8

let texture = Js.string "black.jpg"
let texture = Js.string "../planet/land_ocean_ice_cloud_2048.jpg"
let texture = Js.string "../planet/texture.jpg"

let _ = Firebug.console##log (Array.length o.faces)

let start _ =
  Lwt.ignore_result
    (load_image texture >>= fun texture ->
  let (shd, update_texture) = shadow texture in

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

  let _ = shade () in

  let m_obliq = xy_rotation (-.obliquity) in
  let m = ref m_obliq in

  let mx = ref 0 in
  let my = ref 0 in
  canvas##onmousedown <- Dom_html.handler
    (fun ev ->
       Firebug.console##log ("mousedown");
       mx := ev##clientX; my := ev##clientY;
       let c1 =
         Html.addEventListener Html.document Html.Event.mousemove
           (Dom_html.handler
              (fun ev ->
                 let x = ev##clientX and y = ev##clientY in
                 let dx = x - !mx and dy = y - !my in
                 Firebug.console##log_3 ("mousemove", dx, dy);
                 if dy != 0 then
                   m := matrix_mul (yz_rotation (2. *. float dy /. float width)) !m;
                 if dx != 0 then
                   m := matrix_mul (xz_rotation (2. *. float dx /. float width)) !m;
                 mx := x; my := y;
                 Js._true))
           Js._true
       in
       let c2 = ref Js.null in
       c2 := Js.some
         (Html.addEventListener Html.document Html.Event.mouseup
            (Dom_html.handler
               (fun _ ->
                  Firebug.console##log ("mouseup");
                  Html.removeEventListener c1;
                  Js.Opt.iter !c2 Html.removeEventListener;
                  Js._true))
            Js._true);
       Js._false);
  let ti = ref (Js.to_float (Js.date##now ())) in

  let rec loop o v t phi =
    let rotation = xz_rotation phi in
    update_texture phi;
    let m = matrix_mul !m rotation in
    let o' = rotate_object m o in
    let v' = rotate_normal m v in

    ctx'##clearRect (0., 0., float width, float height);
    ctx'##save ();
    ctx'##beginPath();
    ctx'##arc(r, r, r *. 0.95, 0., -. 2. *. pi, Js._true);
    ctx'##clip();
(*CLIP
*)

    ctx'##setTransform (r -. 2., 0., 0., r -. 2., r, r);
    ctx'##globalCompositeOperation <- Js.string "lighter";
    draw ctx' texture shd o' uv normals v';
    ctx'##restore ();

    ctx##globalCompositeOperation <- Js.string "copy";
    ctx##drawImage_fromCanvas (canvas', 0., 0.);
    begin try ignore (ctx##getImageData (0., 0., 1., 1.)) with _ -> () end;
    let t' = Js.to_float (Js.date##now ()) in
    Firebug.console##log (t' -. !ti);
    ti := t';
    Lwt_js.sleep 0.01 >>= fun () ->
    let t' = Js.to_float (Js.date##now ()) in
    let dt = t' -. t in
    let dt = if dt < 0. then 0. else if dt > 1000. then 0. else dt in
    let angle = 2. *. pi *. dt /. 1000. /. 10. in
(*
if true then Lwt.return () else
*)
    loop o v t' (phi +. angle)
  in
  loop o {x = 0.; y = 0.; z = 1.} (Js.to_float (Js.date##now ())) 0.
); Js._false

let _ =
Html.window##onload <- Html.handler start
