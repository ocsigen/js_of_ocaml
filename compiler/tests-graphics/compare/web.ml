(* jsoo renderer: draw the shared scene onto a DOM canvas, then emit the raw
   canvas raster (row 0 = top, hex rrggbb per pixel) into <pre id="dump">. *)
open Js_of_ocaml
module Html = Dom_html

let () =
  let w = 200 and h = 200 in
  let canvas = Html.createCanvas Html.document in
  canvas##.width := w;
  canvas##.height := h;
  Dom.appendChild Html.document##.body canvas;
  Graphics_js.open_canvas canvas;
  Gscene.Scene.draw ();
  let ctx = canvas##getContext Html._2d_ in
  let n = Js.number_of_float in
  let img = ctx##getImageData (n 0.) (n 0.) (n (float w)) (n (float h)) in
  let d = img##.data in
  let buf = Buffer.create (w * h * 6) in
  let hx = "0123456789abcdef" in
  let put v =
    Buffer.add_char buf hx.[(v lsr 4) land 0xf];
    Buffer.add_char buf hx.[v land 0xf]
  in
  for r = 0 to h - 1 do
    for c = 0 to w - 1 do
      let idx = ((r * w) + c) * 4 in
      put (Html.pixel_get d idx);
      put (Html.pixel_get d (idx + 1));
      put (Html.pixel_get d (idx + 2))
    done
  done;
  let pre = Html.createPre Html.document in
  pre##.id := Js.string "dump";
  pre##.textContent := Js.some (Js.string (Buffer.contents buf));
  Dom.appendChild Html.document##.body pre
