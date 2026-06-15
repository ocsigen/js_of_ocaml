(* Shared drawing, used identically by the native and the jsoo executables.
   Uses only plain Graphics primitives (Graphics_js re-exports the same ones,
   and the jsoo runtime implements the same caml_gr_* externals).

   The canvas is split into two horizontal bands (see compare/diff.py):

   - EXACT zone, OCaml y < 99 (screen rows >= 101): axis-aligned fill_rect
     and fill_poly, single-pixel plots and images. The native X11 backend
     does no anti-aliasing, so these must match the jsoo canvas output
     pixel-for-pixel -- any difference here is a real placement/colour bug.

   - AA zone, OCaml y >= 99 (screen rows <= 100): curved and stroked
     primitives (arcs, circles, ellipses, lines, polylines, segments,
     Bezier) plus the filled curves (fill_arc, fill_circle, fill_ellipse).
     Their curved edges are anti-aliased by the browser but hard-edged by
     X11, so per-pixel differences along them are expected and tolerated.

   Text (draw_string) is included in the AA zone, but native and browser
   font rasterizers are entirely different, so the glyphs themselves are
   never pixel-comparable -- only the baseline placement is meaningful. The
   per-glyph differences are expected and tolerated. *)

let make_img () =
  let open Graphics in
  let m = Array.make_matrix 12 12 (rgb 0 0 0) in
  for r = 0 to 11 do
    for c = 0 to 11 do
      m.(r).(c) <- rgb (r * 20) (c * 20) 160
    done
  done;
  m

(* an 8x8 tile, opaque yellow with a transparent 4x4 hole in the middle *)
let make_hole () =
  let open Graphics in
  let m = Array.make_matrix 8 8 (rgb 255 255 0) in
  for i = 2 to 5 do
    for j = 2 to 5 do
      m.(i).(j) <- transp
    done
  done;
  m

let draw () =
  let open Graphics in
  (* background *)
  set_color (rgb 255 255 255);
  fill_rect 0 0 200 200;

  (* ===================== EXACT zone (OCaml y < 99) ===================== *)
  (* solid rectangles, including origin- and edge-touching ones; these
     exercise fill_rect placement and the (w+1)x(h+1) far-edge extent *)
  set_color (rgb 255 0 0);
  fill_rect 8 8 30 20;
  set_color (rgb 0 0 255);
  fill_rect 70 55 40 30;
  set_color (rgb 0 160 0);
  fill_rect 150 8 30 25;
  (* the origin pixel and a low-y block (origin-sensitive, #2296) *)
  set_color (rgb 0 0 0);
  fill_rect 0 0 5 5;

  (* axis-aligned fill_poly (an L/staircase: every edge horizontal or
     vertical, so no diagonal AA -- matches native exactly) *)
  set_color (rgb 200 0 200);
  fill_poly [| 45, 40; 65, 40; 65, 60; 55, 60; 55, 90; 45, 90 |];

  (* single-pixel plots: origin, bottom edge, bottom-right corner, and a
     diagonal cluster via plots *)
  set_color (rgb 0 0 0);
  plot 0 0;
  plot 6 0;
  plot 0 6;
  plot 199 0;
  plots [| 120, 50; 121, 51; 122, 52; 123, 53; 124, 54 |];

  (* a drawn image (origin + draw_image sync sensitive) *)
  draw_image (make_image (make_img ())) 160 55;

  (* draw_image with a transparent hole over a coloured patch: the hole
     must show the patch colour through (transp compositing) *)
  set_color (rgb 0 200 200);
  fill_rect 120 8 24 24;
  draw_image (make_image (make_hole ())) 124 12;

  (* ====================== AA zone (OCaml y >= 99) ======================
     Everything here is tolerated, so primitives may overlap freely; the
     only constraint is that nothing dips below y ~= 102 (so anti-aliasing
     never bleeds into the exact zone). *)
  (* filled curves (interiors match native; only the curved edge is AA) *)
  set_color (rgb 0 0 200);
  fill_circle 100 120 14;
  set_color (rgb 0 160 0);
  fill_ellipse 140 118 24 12;
  set_color (rgb 200 120 0);
  fill_arc 178 120 16 16 0 270;
  (* outlined curves *)
  set_color (rgb 0 0 0);
  set_line_width 1;
  draw_arc 30 165 22 22 90 180;
  draw_circle 95 165 18;
  draw_ellipse 150 168 28 12;
  (* straight strokes: a horizontal then a vertical segment via lineto *)
  moveto 12 145;
  lineto 32 145;
  lineto 32 160;
  (* a rectangle outline *)
  draw_rect 188 150 8 30;
  (* an open polyline and a pair of crossing segments *)
  draw_poly_line [| 60, 145; 85, 145; 85, 160; 62, 160 |];
  draw_segments [| 108, 145, 135, 162; 108, 162, 135, 145 |];
  (* a thick line (set_line_width) *)
  set_line_width 4;
  moveto 10 192;
  lineto 90 192;
  set_line_width 1;
  (* a cubic Bezier *)
  moveto 105 184;
  curveto (130, 199) (165, 199) (192, 186);
  (* text: glyphs differ between X11 and the browser, but the baseline is
     at the current point in both *)
  set_font "courier";
  set_text_size 18;
  moveto 6 106;
  draw_string "Abg123"
