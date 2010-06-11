(*
IDEAS
=====
- saisons
- satellites: geostationnaires, différentes altitudes
  ==> trajectoire + mouvement du satellite
- can we have antialiased edges?
- affiche l'axe de rotation de la terre, la direction du soleil
- autres planétes

Should experiment with sphere tessellation...
   http://sol.gfxile.net/sphere/index.html
   http://www.nihilogic.dk/labs/canvas3dtexture_0.2/

Stop animation when not visible!
===> use window.onfocus/onblur

http://visibleearth.nasa.gov/view_rec.php?id=2431
http://maps.jpl.nasa.gov/
*)

let texture = Js.string "texture.jpg"

module Html = Dom_html

let create_canvas w h =
  let c = Html.createCanvasElement Html.document in
  c##width <- w; c##height <- h; c

let lwt_wrap f =
  let (t, w) = Lwt.task () in
  let cont x = Lwt.wakeup w x in
  f cont;
  t

let (>>=) = Lwt.bind

let load_image src =
  let img = Html.createImageElement Html.document in
  lwt_wrap (fun c -> img##onload <- c; img##src <- src) >>= fun () ->
  let w = img##naturalWidth in
  let h = img##naturalHeight in
  let canvas = create_canvas w h in
  let ctx = canvas##getContext (Html._2d_) in
  ctx##drawImage (img, 0., 0.);
  Lwt.return (w, h, ctx##getImageData (0., 0., float w, float h))

let r = 100
let x0 = 250
let y0 = 250
let width = 2 * x0 + 1
let height = 2 * y0 + 1

let pi = 4. *. atan 1.

let obliquity = 23.5 *. pi /. 180.
let gamma = 2.
let dark = 0.2 ** gamma

let prepare_rendering (tw, th, texture) =
  let canvas = create_canvas width height in
  Dom.appendChild Html.document##body canvas;
  let ctx = canvas##getContext (Html._2d_) in
  let img = ctx##createImageData (width, height) in

  let data = img##data in
  let rf = float r in
  let r2 = rf *. rf in
  let cst = (1. -. dark) /. rf in
  let inv_gamma  = 1. /. gamma in
  for i = 0 to width - 1 do
    for j = 0 to height - 1 do
      let i = float i in
      let j = float j in
      let k = truncate (4. *. (i +. j *. float width)) in
      let x = i -. float x0 in
      let y = j -. float y0 in
      let z2 = r2 -. x *. x -. y *. y in
      if z2 >= 0. then begin
        if x > 0. then
          Html.pixel_set data (k + 3) (truncate (255.99 *. dark ** inv_gamma))
        else
          Html.pixel_set data (k + 3)
            (truncate (255.99 *. (dark -. x *. cst) ** inv_gamma))
      end
    done
  done;

  (ctx, img, tw, th, texture)

let render (ctx, img, tw, th, texture) offset =
  let tw = float tw in
  let th = float th in
  let cos_obl = cos obliquity in
  let sin_obl = sin obliquity in
  let data = img##data in
  let widthf = float width in
  let rf = float r in
  let r2 = rf *. rf in
  let tdata = texture##data in
  let cst1 = (tw /. 2. -. 0.99) /. pi in
  let cst2 = th /. 2. in
  let cst3 = (th -. 0.99) /. pi in
  let cst4 = (th -. 0.99) /. 2. /. rf in
  let offset = float (truncate (mod_float (-. tw *. offset) tw)) in
  let offset = tw +. if offset < 0. then offset +. tw else offset in
  for i = x0 - r to x0 + r do
    for j = y0 - r to x0 + r do
      let i = float i in
      let j = float j in
      let x = i -. float x0 in
      let y = j -. float y0 in
      let z2 = r2 -. x *. x -. y *. y in
      if z2 >= 0. then begin
        let k = truncate (4. *. (i +. j *. widthf)) in
        let x' = x *. cos_obl +. y *. sin_obl in
        let y' = -. x *. sin_obl +. y *. cos_obl in
        let x = x' in
        let y = y' in
        let z = sqrt z2 in
        let i =
          mod_float (offset +. float (truncate (atan2 x z *. cst1))) tw in
        let j = float (truncate (cst2 +. asin (y /. rf) *. cst3)) in
(*
        let j = float (truncate (cst2 +. y *. cst4)) in
*)
(*
assert (0. <= i);
assert (i < tw);
assert (0. <= j);
assert (j < th);
*)
        let tk = truncate (4. *. (i +. (j *. tw))) in
        Html.pixel_set data k (Html.pixel_get tdata tk);
        Html.pixel_set data (k + 1) (Html.pixel_get tdata (tk + 1));
        Html.pixel_set data (k + 2) (Html.pixel_get tdata (tk + 2));
      end
    done
  done;
  ctx##putImageData (img, 0., 0.);
  ctx##globalCompositeOperation <- Js.string "destination-over";
  ctx##fillStyle <- Js.string "argb(0,0,0,255)";
  ctx##fillRect (0., 0., float width, float height)

let rec loop state offset =
  render state offset;
  Lwt_js.sleep 0.05 >>= fun () ->
  loop state (offset +. 1. /. 200.)

let _ =
  Lwt.ignore_result
    (load_image texture >>= fun texture ->
     let state = prepare_rendering texture in
     loop state 0.)
