open Js_of_ocaml
open Graphics_js
module Html = Dom_html

let w = 200

let h = 200

(* Independent raw pixel reader: read canvas pixel at *canvas* coords
   (cx, cy) -> (r, g, b), bypassing the Graphics coordinate primitives. *)
let raw_ctx = ref None

let n = Js.number_of_float

(* read a pixel from an arbitrary 2d context *)
let read_ctx ctx cx cy =
  let img = ctx##getImageData (n (float cx)) (n (float cy)) (n 1.) (n 1.) in
  let d = img##.data in
  Html.pixel_get d 0, Html.pixel_get d 1, Html.pixel_get d 2

let raw_read cx cy =
  let ctx =
    match !raw_ctx with
    | Some c -> c
    | None -> assert false
  in
  read_ctx ctx cx cy

(* Convert OCaml (bottom-left origin) coords to the canonical canvas row. *)
let canvas_row oy = h - 1 - oy

let results = Buffer.create 256

let pass = ref 0

let fail = ref 0

let check name ok detail =
  if ok then incr pass else incr fail;
  Buffer.add_string
    results
    (Printf.sprintf "[%s] %s : %s\n" (if ok then "PASS" else "FAIL") name detail)

let hex (r, g, b) = Printf.sprintf "#%02x%02x%02x" r g b

let is_white (r, g, b) = r = 255 && g = 255 && b = 255

(* Did anything get drawn (a non-white pixel appear) in the canvas-space
   rectangle [cx0, cx0+ww) x [cy0, cy0+hh) ? Used for anti-aliased
   primitives (strokes, arcs, text) where exact colours are unreliable. *)
let scan_non_white cx0 cy0 ww hh =
  let found = ref false in
  for yy = cy0 to cy0 + hh - 1 do
    for xx = cx0 to cx0 + ww - 1 do
      if not (is_white (raw_read xx yy)) then found := true
    done
  done;
  !found

(* any ink within +/-3 px of canvas point (cx, cy)? (for anti-aliased
   curves, where the exact pixel is unreliable) *)
let drew_at cx cy = scan_non_white (cx - 3) (cy - 3) 7 7

let raises f =
  try
    f ();
    false
  with _ -> true

let white () =
  set_color (rgb 255 255 255);
  fill_rect 0 0 w h

let () =
  let canvas = Html.createCanvas Html.document in
  canvas##.width := w;
  canvas##.height := h;
  Dom.appendChild Html.document##.body canvas;
  open_canvas canvas;
  raw_ctx := Some (canvas##getContext Html._2d_);
  let main_ctx = get_context () in

  (* --- Test 1: fill_rect placement + point_color agreement (off-by-one) --- *)
  white ();
  set_color (rgb 255 0 0);
  fill_rect 5 5 1 1;
  let fr = raw_read 5 (canvas_row 5) in
  check
    "fill_rect places at height-1-y"
    (fr = (255, 0, 0))
    (Printf.sprintf "raw(5,%d)=%s" (canvas_row 5) (hex fr));
  let pc = point_color 5 5 in
  check
    "point_color reads back fill_rect color"
    (pc = 0xFF0000)
    (Printf.sprintf "point_color 5 5 = 0x%06x (want 0xff0000)" pc);

  (* --- Test 2: plot placement uses height-1-y --- *)
  white ();
  set_color (rgb 0 0 255);
  plot 10 20;
  let pr = raw_read 10 (canvas_row 20) in
  check
    "plot places at height-1-y"
    (pr = (0, 0, 255))
    (Printf.sprintf "raw(10,%d)=%s" (canvas_row 20) (hex pr));
  (* old buggy row height-y would land one row lower (off intended pixel) *)
  let pr_old = raw_read 10 (h - 20) in
  check
    "plot does NOT place at buggy height-y"
    (pr_old = (255, 255, 255))
    (Printf.sprintf "raw(10,%d)=%s (should be white)" (h - 20) (hex pr_old));

  (* --- Test 3: plot at y=0 is on-canvas (visible) and round-trips --- *)
  white ();
  set_color (rgb 0 255 0);
  plot 15 0;
  let p0 = raw_read 15 (canvas_row 0) in
  check
    "plot x 0 is visible (bottom row)"
    (p0 = (0, 255, 0))
    (Printf.sprintf "raw(15,%d)=%s" (canvas_row 0) (hex p0));
  let pc0 = point_color 15 0 in
  check
    "point_color x 0 reads it back"
    (pc0 = 0x00FF00)
    (Printf.sprintf "point_color 15 0 = 0x%06x" pc0);

  (* --- Test 4: draw_arc 90->180 renders the top-left quadrant --- *)
  white ();
  set_color (rgb 0 0 0);
  let cx = 100 and cy = 100 and r = 60 in
  draw_arc cx cy r r 90 180;
  (* scan the whole canvas once, classify colored pixels by OCaml quadrant *)
  let img =
    (match !raw_ctx with
      | Some c -> c
      | None -> assert false)##getImageData
      (n 0.)
      (n 0.)
      (n (float w))
      (n (float h))
  in
  let d = img##.data in
  let tl = ref 0 and tr = ref 0 and bl = ref 0 and br = ref 0 in
  for cyi = 0 to h - 1 do
    for cxi = 0 to w - 1 do
      let idx = ((cyi * w) + cxi) * 4 in
      let rr = Html.pixel_get d idx in
      let gg = Html.pixel_get d (idx + 1) in
      let bb = Html.pixel_get d (idx + 2) in
      if rr < 128 && gg < 128 && bb < 128
      then begin
        let ox = cxi and oy = h - 1 - cyi in
        if ox < cx && oy > cy
        then incr tl
        else if ox > cx && oy > cy
        then incr tr
        else if ox < cx && oy < cy
        then incr bl
        else if ox > cx && oy < cy
        then incr br
      end
    done
  done;
  check
    "draw_arc 90->180 is in top-left quadrant"
    (!tl > 10 && !br = 0)
    (Printf.sprintf "counts tl=%d tr=%d bl=%d br=%d" !tl !tr !bl !br);

  (* --- Test 5: first draw_image is synchronous --- *)
  white ();
  let red = rgb 255 0 0 in
  let img5 = make_image (Array.make_matrix 4 4 red) in
  draw_image img5 50 50;
  (* immediately (synchronously) read a pixel inside the drawn image.
     image top-left canvas coords = (50, h - 4 - 50); fill a pixel inside. *)
  let ix = 51 and iy = h - 4 - 50 + 1 in
  let di = raw_read ix iy in
  check
    "first draw_image is synchronous"
    (di = (255, 0, 0))
    (Printf.sprintf "raw(%d,%d)=%s immediately after draw_image" ix iy (hex di));

  (* ===================================================================== *)
  (* The remaining tests exercise the rest of the Graphics_js API surface.  *)
  (* ===================================================================== *)

  (* --- Test 6: window geometry (size_x / size_y) --- *)
  check
    "size_x/size_y report the canvas size"
    (size_x () = w && size_y () = h)
    (Printf.sprintf "size_x=%d size_y=%d" (size_x ()) (size_y ()));

  (* --- Test 7: moveto / current_x / current_y / current_point --- *)
  moveto 12 34;
  let curp = current_point () in
  check
    "moveto sets current_x/current_y/current_point"
    (current_x () = 12 && current_y () = 34 && curp = (12, 34))
    (Printf.sprintf
       "current=(%d,%d) current_point=(%d,%d)"
       (current_x ())
       (current_y ())
       (fst curp)
       (snd curp));

  (* --- Test 8: rmoveto is relative to the current point --- *)
  moveto 10 10;
  rmoveto 5 7;
  check
    "rmoveto moves relative to current point"
    (current_point () = (15, 17))
    (Printf.sprintf "current_point=(%d,%d)" (current_x ()) (current_y ()));

  (* --- Test 9: set_window_title is a harmless no-op (must not raise) --- *)
  check
    "set_window_title does not raise"
    (not (raises (fun () -> set_window_title "gr2296")))
    "set_window_title \"gr2296\"";

  (* --- Test 10: plots draws every point of the array --- *)
  white ();
  set_color (rgb 0 0 0);
  plots [| 30, 30; 31, 31; 32, 32 |];
  check
    "plots draws each point"
    (raw_read 30 (canvas_row 30) = (0, 0, 0)
    && raw_read 31 (canvas_row 31) = (0, 0, 0)
    && raw_read 32 (canvas_row 32) = (0, 0, 0))
    "three plotted pixels are black";

  (* --- Test 11: fill_poly fills a triangle's interior --- *)
  (* fill_poly fills the triangle interior only: the centroid and a point
     near the base are coloured, but the upper corners of the bounding box
     -- outside the sloped edges -- stay white (a filled rectangle would
     colour them). *)
  white ();
  let tcol = rgb 10 20 200 in
  set_color tcol;
  fill_poly [| 60, 60; 90, 60; 75, 90 |];
  let fp_centroid = point_color 75 70 = tcol in
  let fp_base = point_color 75 62 = tcol in
  let fp_out_l = point_color 62 85 = 0xFFFFFF in
  let fp_out_r = point_color 88 85 = 0xFFFFFF in
  check
    "fill_poly fills the triangle interior only (corners stay white)"
    (fp_centroid && fp_base && fp_out_l && fp_out_r)
    (Printf.sprintf
       "centroid=0x%06x base=0x%06x outL(62,85)=0x%06x outR(88,85)=0x%06x"
       (point_color 75 70)
       (point_color 75 62)
       (point_color 62 85)
       (point_color 88 85));

  (* --- Test 12: fill_circle / fill_ellipse / fill_arc fill the centre --- *)
  (* fill_circle fills a round disc: points within the radius are
     coloured, but the bounding-box corner (distance r*sqrt2 > r) and a
     point just past the radius stay white -- a square fill would colour
     both. *)
  white ();
  let ccol = rgb 200 30 30 in
  set_color ccol;
  fill_circle 100 100 20;
  let c_centre = point_color 100 100 = ccol in
  let c_in = point_color 115 100 = ccol && point_color 100 115 = ccol in
  let c_out_edge = point_color 125 100 = 0xFFFFFF in
  let c_out_corner = point_color 116 116 = 0xFFFFFF in
  check
    "fill_circle fills a round disc (bbox corner stays white)"
    (c_centre && c_in && c_out_edge && c_out_corner)
    (Printf.sprintf
       "centre=0x%06x (115,100)=0x%06x (125,100)=0x%06x corner(116,116)=0x%06x"
       (point_color 100 100)
       (point_color 115 100)
       (point_color 125 100)
       (point_color 116 116));
  (* fill_ellipse must honour distinct radii (rx<>ry): a wide, short
     ellipse (rx=40, ry=15) covers (cx+30,cy) but leaves (cx,cy+30)
     white -- a circle of radius 40 would colour both. *)
  white ();
  let ecol = rgb 30 200 30 in
  set_color ecol;
  fill_ellipse 100 100 40 15;
  let e_centre = point_color 100 100 = ecol in
  let e_in_h = point_color 130 100 = ecol in
  let e_in_v = point_color 100 108 = ecol in
  let e_out_v = point_color 100 130 = 0xFFFFFF in
  check
    "fill_ellipse honours rx<>ry (wide and short)"
    (e_centre && e_in_h && e_in_v && e_out_v)
    (Printf.sprintf
       "centre=0x%06x (130,100)=0x%06x (100,108)=0x%06x (100,130)=0x%06x"
       (point_color 100 100)
       (point_color 130 100)
       (point_color 100 108)
       (point_color 100 130));
  white ();
  let acol = rgb 30 30 200 in
  set_color acol;
  fill_arc 100 100 25 25 0 360;
  check
    "fill_arc (full disk) fills the centre"
    (point_color 100 100 = acol)
    (Printf.sprintf "centre = 0x%06x" (point_color 100 100));

  (* --- Test 13: lineto / rlineto draw a stroke --- *)
  white ();
  set_color (rgb 0 0 0);
  moveto 20 100;
  lineto 180 100;
  (* lineto strokes at canvas row h-y; scan a band around it *)
  check
    "lineto draws a horizontal stroke"
    (scan_non_white 21 (h - 100 - 2) 158 5)
    "non-white pixels found along the line";
  white ();
  set_color (rgb 0 0 0);
  moveto 10 30;
  rlineto 60 0;
  check
    "rlineto draws relative to current point"
    (scan_non_white 11 (h - 30 - 2) 58 5)
    "non-white pixels found along the relative line";

  (* --- Test 14: draw_rect draws a rectangle outline --- *)
  white ();
  set_color (rgb 0 0 0);
  draw_rect 40 40 50 30;
  check
    "draw_rect strokes a rectangle outline"
    (scan_non_white 38 (h - 70 - 2) 56 36)
    "non-white pixels found on the rectangle border";

  (* --- Test 15: draw_poly_line / draw_poly / draw_segments --- *)
  white ();
  set_color (rgb 0 0 0);
  draw_poly_line [| 10, 10; 50, 10; 50, 50 |];
  check
    "draw_poly_line strokes an open path"
    (scan_non_white 8 (h - 50 - 2) 46 46)
    "non-white pixels found on the poly line";
  white ();
  set_color (rgb 0 0 0);
  draw_poly [| 120, 120; 160, 120; 140, 160 |];
  check
    "draw_poly strokes a closed path"
    (scan_non_white 118 (h - 160 - 2) 46 46)
    "non-white pixels found on the closed poly";
  white ();
  set_color (rgb 0 0 0);
  draw_segments [| 5, 5, 60, 60; 60, 5, 5, 60 |];
  check
    "draw_segments strokes the given segments"
    (scan_non_white 4 (h - 60 - 2) 60 60)
    "non-white pixels found on the segments";

  (* --- Test 16: draw_arc full circle / draw_circle / draw_ellipse --- *)
  (* draw_circle strokes a round outline: ink at all four vertices but
     none at the bounding-box corner (cx+r, cy+r) *)
  white ();
  set_color (rgb 0 0 0);
  draw_circle 100 100 40;
  let dc_right = drew_at 140 (h - 100) in
  let dc_left = drew_at 60 (h - 100) in
  let dc_top = drew_at 100 (h - 140) in
  let dc_bottom = drew_at 100 (h - 60) in
  let dc_corner_clear = not (drew_at 140 (h - 140)) in
  check
    "draw_circle strokes a round outline (bbox corner is clear)"
    (dc_right && dc_left && dc_top && dc_bottom && dc_corner_clear)
    (Printf.sprintf
       "right=%b left=%b top=%b bottom=%b corner-clear=%b"
       dc_right
       dc_left
       dc_top
       dc_bottom
       dc_corner_clear);
  (* draw_ellipse honours distinct radii (rx=45, ry=20): ink at the right
     vertex (cx+rx,cy) and top vertex (cx,cy+ry), but none where a circle
     of radius rx would be (cx, cy+rx) *)
  white ();
  set_color (rgb 0 0 0);
  draw_ellipse 100 100 45 20;
  let de_right = drew_at 145 (h - 100) in
  let de_top = drew_at 100 (h - 120) in
  let de_not_circle = not (drew_at 100 (h - 145)) in
  check
    "draw_ellipse honours rx<>ry (no ink at a circle's radius)"
    (de_right && de_top && de_not_circle)
    (Printf.sprintf
       "right=%b top=%b no-ink-at-(100,145)=%b"
       de_right
       de_top
       de_not_circle);

  (* --- curveto strokes a cubic Bezier and leaves the current point at
     its endpoint (it draws the spline via draw_poly_line then moveto). --- *)
  white ();
  set_color (rgb 0 0 0);
  moveto 20 100;
  curveto (60, 160) (140, 160) (180, 100);
  let cv_end = current_point () = (180, 100) in
  let cv_start_ink = drew_at 20 (h - 100) in
  let cv_end_ink = drew_at 180 (h - 100) in
  let cv_path_ink = scan_non_white 20 (h - 160) 161 70 in
  check
    "curveto strokes a cubic Bezier ending at the last point"
    (cv_end && cv_start_ink && cv_end_ink && cv_path_ink)
    (Printf.sprintf
       "end=(%d,%d) start_ink=%b end_ink=%b path_ink=%b"
       (current_x ())
       (current_y ())
       cv_start_ink
       cv_end_ink
       cv_path_ink);

  (* --- Test 17: set_line_width thickens strokes --- *)
  white ();
  set_color (rgb 0 0 0);
  set_line_width 5;
  moveto 20 50;
  lineto 180 50;
  (* the line is centred on canvas row h-1-50; a 5px width inks a band of
     about 5 rows there, vs ~2 for a 1px line *)
  let cy = h - 1 - 50 in
  let band = ref 0 in
  for r = cy - 4 to cy + 4 do
    if not (is_white (raw_read 100 r)) then incr band
  done;
  check
    "set_line_width 5 produces a thick line (>= 4 px band)"
    (!band >= 4)
    (Printf.sprintf "non-white rows in column 100 around y=%d: %d" cy !band);
  set_line_width 1;

  (* --- Test 18: set_font / set_text_size / text_size --- *)
  set_font "sans-serif";
  set_text_size 20;
  let tw, th = text_size "Hi" in
  check
    "text_size reports the configured text height"
    (th = 20 && tw > 0)
    (Printf.sprintf "text_size \"Hi\" = (%d,%d)" tw th);

  (* --- Test 19: draw_string / draw_char render glyphs and advance x --- *)
  white ();
  set_color (rgb 0 0 0);
  set_text_size 26;
  moveto 20 100;
  draw_string "Hello";
  check
    "draw_string renders glyphs"
    (scan_non_white 20 (h - 100 - 26) 110 28)
    "non-white pixels found where the string was drawn";
  check
    "draw_string advances current_x"
    (current_x () > 20)
    (Printf.sprintf "current_x = %d (was 20)" (current_x ()));
  white ();
  set_color (rgb 0 0 0);
  moveto 20 100;
  draw_char 'A';
  check
    "draw_char renders a glyph"
    (scan_non_white 20 (h - 100 - 26) 30 28)
    "non-white pixels found where the char was drawn";

  (* --- Test 20: make_image / dump_image round-trips a colour matrix --- *)
  let m =
    Array.init 3 (fun i -> Array.init 3 (fun j -> rgb (i * 10) (j * 10) (i + j + 1)))
  in
  let dumped = dump_image (make_image m) in
  let matrix_eq = ref true in
  Array.iteri
    (fun i row ->
      Array.iteri (fun j c -> if dumped.(i).(j) <> c then matrix_eq := false) row)
    m;
  check
    "make_image/dump_image round-trip a colour matrix"
    !matrix_eq
    "dump_image reproduces the source matrix";

  (* --- Test 21: create_image + blit_image capture the canvas --- *)
  white ();
  let bcol = rgb 7 8 9 in
  set_color bcol;
  fill_rect 50 50 4 4;
  let cap = create_image 4 4 in
  blit_image cap 50 50;
  let cdump = dump_image cap in
  let blit_ok = ref true in
  Array.iter
    (fun row -> Array.iter (fun c -> if c <> bcol then blit_ok := false) row)
    cdump;
  check
    "create_image + blit_image capture canvas pixels"
    !blit_ok
    (Printf.sprintf "all blitted pixels = 0x%06x" bcol);

  (* --- Test 22: get_image returns the captured region --- *)
  white ();
  let gcol = rgb 11 22 33 in
  set_color gcol;
  fill_rect 80 80 5 5;
  let gi = get_image 80 80 5 5 in
  let gdump = dump_image gi in
  let get_ok = ref true in
  Array.iter
    (fun row -> Array.iter (fun c -> if c <> gcol then get_ok := false) row)
    gdump;
  check
    "get_image captures the requested region"
    !get_ok
    (Printf.sprintf "all captured pixels = 0x%06x" gcol);

  (* --- Test 23: transparent image pixels (transp) show the background --- *)
  let bg = rgb 0 0 200 in
  set_color bg;
  fill_rect 0 0 w h;
  let tm = Array.make_matrix 4 4 (rgb 255 0 0) in
  tm.(2).(2) <- transp;
  let timg = make_image tm in
  draw_image timg 120 120;
  (* image top-left canvas = (120, h-4-120); matrix (row,col) -> canvas
     (120+col, (h-4-120)+row) *)
  let top = h - 4 - 120 in
  let transp_px = raw_read (120 + 2) (top + 2) in
  let opaque_px = raw_read (120 + 0) (top + 0) in
  check
    "transparent image pixel shows the background"
    (transp_px = (0, 0, 200))
    (Printf.sprintf "transp pixel = %s (want background blue)" (hex transp_px));
  check
    "opaque image pixel shows the image colour"
    (opaque_px = (255, 0, 0))
    (Printf.sprintf "opaque pixel = %s" (hex opaque_px));

  (* --- Test 24: clear_graph empties the canvas --- *)
  set_color (rgb 123 0 0);
  fill_rect 0 0 w h;
  clear_graph ();
  let cleared = point_color 100 100 in
  check
    "clear_graph clears the canvas"
    (cleared = 0)
    (Printf.sprintf "point_color after clear = 0x%06x" cleared);

  (* --- Test 25: get_context / set_context round-trip and route drawing --- *)
  let saved = get_context () in
  set_context saved;
  white ();
  set_color (rgb 0 0 0);
  plot 70 70;
  check
    "set_context (round-trip) keeps drawing on the same canvas"
    (raw_read 70 (canvas_row 70) = (0, 0, 0))
    "plot landed on the main canvas after set_context";

  (* drawing is routed to whichever context is current *)
  let canvas2 = Html.createCanvas Html.document in
  canvas2##.width := w;
  canvas2##.height := h;
  Dom.appendChild Html.document##.body canvas2;
  let ctx2 = canvas2##getContext Html._2d_ in
  open_canvas canvas2;
  (* now canvas2 is current *)
  set_color (rgb 255 0 0);
  fill_rect 0 0 w h;
  set_context main_ctx;
  set_color (rgb 0 255 0);
  fill_rect 0 0 w h;
  let on_main = raw_read 100 100 in
  let on_two = read_ctx ctx2 100 100 in
  check
    "open_canvas/set_context route drawing to the current context"
    (on_main = (0, 255, 0) && on_two = (255, 0, 0))
    (Printf.sprintf "main=%s canvas2=%s" (hex on_main) (hex on_two));
  set_context main_ctx;

  (* --- Test 26: resize_window updates the reported and canvas size --- *)
  let canvas3 = Html.createCanvas Html.document in
  canvas3##.width := w;
  canvas3##.height := h;
  Dom.appendChild Html.document##.body canvas3;
  open_canvas canvas3;
  resize_window 120 90;
  check
    "resize_window updates size_x/size_y and the canvas"
    (size_x () = 120 && size_y () = 90 && canvas3##.width = 120 && canvas3##.height = 90)
    (Printf.sprintf
       "size=(%d,%d) canvas=(%d,%d)"
       (size_x ())
       (size_y ())
       canvas3##.width
       canvas3##.height);

  (* --- Test 27: close_graph tears the canvas down --- *)
  close_graph ();
  check
    "close_graph zeroes the canvas dimensions"
    (canvas3##.width = 0 && canvas3##.height = 0)
    (Printf.sprintf "canvas=(%d,%d)" canvas3##.width canvas3##.height);
  set_context main_ctx;

  (* --- open_graph opens a popup window (globalThis.open); in a headless
     or popup-blocked environment it raises a clean failure, otherwise it
     makes the popup canvas current. Either way the runtime must stay
     usable once we restore the main context. --- *)
  let opened =
    try
      open_graph " 100x80";
      true
    with _ -> false
  in
  set_context main_ctx;
  white ();
  set_color (rgb 0 0 0);
  plot 90 90;
  check
    "open_graph is callable; main context usable afterwards"
    (raw_read 90 (canvas_row 90) = (0, 0, 0))
    (Printf.sprintf
       "open_graph %s; plot after restore works"
       (if opened then "opened a window" else "raised (popup blocked)"));

  (* --- Test 28: unimplemented primitives fail cleanly (must raise) --- *)
  check
    "synchronize raises (unimplemented)"
    (raises (fun () -> synchronize ()))
    "Graphics_js.synchronize";
  check
    "display_mode raises (unimplemented)"
    (raises (fun () -> display_mode true))
    "Graphics_js.display_mode";
  check
    "remember_mode raises (unimplemented)"
    (raises (fun () -> remember_mode true))
    "Graphics_js.remember_mode";
  check
    "wait_next_event raises (use Graphics_js loop instead)"
    (raises (fun () -> ignore (wait_next_event [ Poll ])))
    "Graphics_js.wait_next_event";
  check
    "key_pressed raises (unimplemented polling)"
    (raises (fun () -> ignore (key_pressed ())))
    "Graphics_js.key_pressed";
  check
    "sound raises (no primitive)"
    (raises (fun () -> sound 440 100))
    "Graphics_js.sound";
  check
    "auto_synchronize raises (delegates to synchronize)"
    (raises (fun () -> auto_synchronize true))
    "Graphics_js.auto_synchronize";

  (* --- Test 29: the Lwt event API installs handlers without blocking --- *)
  let event_api_ok =
    not
      (raises (fun () ->
           ignore (mouse_pos ());
           ignore (button_down ());
           ignore (read_key ());
           loop [] (fun _ -> ());
           loop_at_exit [] (fun _ -> ())))
  in
  check
    "Lwt event API (mouse_pos/button_down/read_key/loop) installs handlers"
    event_api_ok
    "interactive handlers installed without blocking";

  (* leave the main context current for any at_exit loop *)
  set_context main_ctx;

  (* --- Report --- *)
  Buffer.add_string results (Printf.sprintf "SUMMARY: %d passed, %d failed\n" !pass !fail);
  let txt = Buffer.contents results in
  Console.console##log (Js.string txt);
  let pre = Html.createPre Html.document in
  pre##.id := Js.string "results";
  pre##.textContent := Js.some (Js.string txt);
  Dom.appendChild Html.document##.body pre
