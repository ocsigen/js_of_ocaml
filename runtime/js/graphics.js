// Js_of_ocaml runtime support
// http://www.ocsigen.org/js_of_ocaml/
// Copyright (C) 2014 Hugo Heuzard

// This program is free software; you can redistribute it and/or modify
// it under the terms of the GNU Lesser General Public License as published by
// the Free Software Foundation, with linking exception;
// either version 2.1 of the License, or (at your option) any later version.

// This program is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU Lesser General Public License for more details.

// You should have received a copy of the GNU Lesser General Public License
// along with this program; if not, write to the Free Software
// Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.

//Provides: caml_gr_state
var caml_gr_state;

//Provides: caml_gr_state_get
//Requires: caml_gr_state
//Requires: caml_named_value, caml_string_of_jsbytes
//Requires: caml_maybe_attach_backtrace
function caml_gr_state_get() {
  if (caml_gr_state) {
    return caml_gr_state;
  }
  throw caml_maybe_attach_backtrace([
    0,
    caml_named_value("Graphics.Graphic_failure"),
    caml_string_of_jsbytes("Not initialized"),
  ]);
}
//Provides: caml_gr_state_set
//Requires: caml_gr_state,caml_gr_state_init
function caml_gr_state_set(ctx) {
  caml_gr_state = ctx;
  caml_gr_state_init();
  return 0;
}

//Provides: caml_gr_open_graph
//Requires: caml_gr_state_create
//Requires: caml_gr_state_set
//Requires: caml_failwith
//Requires: caml_jsstring_of_string
function caml_gr_open_graph(info) {
  var info = caml_jsstring_of_string(info);
  function get(name) {
    var res = info.match("(^|,) *" + name + " *= *([a-zA-Z0-9_]+) *(,|$)");
    if (res) return res[2];
  }
  var specs = [];
  if (!(info === "")) specs.push(info);
  var target = get("target");
  if (!target) target = "";
  var status = get("status");
  if (!status) specs.push("status=1");

  var w = get("width");
  w = w ? Number.parseInt(w) : 200;
  specs.push("width=" + w);

  var h = get("height");
  h = h ? Number.parseInt(h) : 200;
  specs.push("height=" + h);

  var win = globalThis.open("about:blank", target, specs.join(","));
  if (!win) {
    caml_failwith("Graphics.open_graph: cannot open the window");
  }
  var doc = win.document;
  var canvas = doc.createElement("canvas");
  canvas.width = w;
  canvas.height = h;
  var ctx = caml_gr_state_create(canvas, w, h);
  ctx.set_title = function (title) {
    doc.title = title;
  };
  caml_gr_state_set(ctx);
  var body = doc.body;
  body.style.margin = "0px";
  body.appendChild(canvas);
  return 0;
}

//Provides: caml_gr_state_init
//Requires: caml_gr_state
//Requires: caml_gr_set_color,caml_gr_moveto,caml_gr_resize_window
//Requires: caml_gr_set_line_width,caml_gr_set_text_size,caml_gr_set_font
//Requires: caml_gr_set_window_title
function caml_gr_state_init() {
  caml_gr_moveto(caml_gr_state.x, caml_gr_state.y);
  caml_gr_resize_window(caml_gr_state.width, caml_gr_state.height);
  caml_gr_set_line_width(caml_gr_state.line_width);
  caml_gr_set_text_size(caml_gr_state.text_size);
  caml_gr_set_font(caml_gr_state.font);
  caml_gr_set_color(caml_gr_state.color);
  caml_gr_set_window_title(caml_gr_state.title);
  //caml_gr_resize_window might reset some canvas' properties
  caml_gr_state.context.textBaseline = "bottom";
}

//Provides: caml_gr_state_create
//Requires: caml_string_of_jsbytes
function caml_gr_state_create(canvas, w, h) {
  var context = canvas.getContext("2d");
  // the native X11 backend draws thick lines with round caps and joins
  context.lineCap = "round";
  context.lineJoin = "round";
  return {
    context: context,
    canvas: canvas,
    x: 0,
    y: 0,
    width: w,
    height: h,
    line_width: 1,
    font: caml_string_of_jsbytes("fixed"),
    text_size: 26,
    color: 0x000000,
    title: caml_string_of_jsbytes(""),
  };
}

//Provides: caml_gr_doc_of_state
function caml_gr_doc_of_state(state) {
  if (state.canvas.ownerDocument) return state.canvas.ownerDocument;
}

//Provides: caml_gr_close_graph
//Requires: caml_gr_state_get
function caml_gr_close_graph() {
  var s = caml_gr_state_get();
  s.canvas.width = 0;
  s.canvas.height = 0;
  return 0;
}

//Provides: caml_gr_set_window_title
//Requires: caml_gr_state_get
//Requires: caml_jsstring_of_string
function caml_gr_set_window_title(name) {
  var s = caml_gr_state_get();
  s.title = name;
  var jsname = caml_jsstring_of_string(name);
  if (s.set_title) s.set_title(jsname);
  return 0;
}

//Provides: caml_gr_resize_window
//Requires: caml_gr_state_get
function caml_gr_resize_window(w, h) {
  var s = caml_gr_state_get();
  s.width = w;
  s.height = h;
  if (w !== s.canvas.width) s.canvas.width = w;
  if (h !== s.canvas.height) s.canvas.height = h;
  return 0;
}

//Provides: caml_gr_clear_graph
//Requires: caml_gr_state_get
function caml_gr_clear_graph() {
  var s = caml_gr_state_get();
  s.context.clearRect(0, 0, s.canvas.width, s.canvas.height);
  return 0;
}

//Provides: caml_gr_size_x
//Requires: caml_gr_state_get
function caml_gr_size_x() {
  var s = caml_gr_state_get();
  return s.width;
}
//Provides: caml_gr_size_y
//Requires: caml_gr_state_get
function caml_gr_size_y() {
  var s = caml_gr_state_get();
  return s.height;
}

//Provides: caml_gr_set_color
//Requires: caml_gr_state_get
function caml_gr_set_color(color) {
  var s = caml_gr_state_get();
  function convert(number) {
    var str = "" + number.toString(16);
    while (str.length < 2) str = "0" + str;
    return str;
  }
  var r = (color >> 16) & 0xff,
    g = (color >> 8) & 0xff,
    b = (color >> 0) & 0xff;
  s.color = color;
  var c_str = "#" + convert(r) + convert(g) + convert(b);
  s.context.fillStyle = c_str;
  s.context.strokeStyle = c_str;
  return 0;
}
// Coordinate helpers. Graphics uses a bottom-left origin; the canvas uses a
// top-left origin, so the row of the bottom-left-origin coordinate y is
// height-1-y. Two conventions are used, shared by the JS and the wasm paths:
//   - caml_gr_y: the pixel row itself. plot, point_color, fill_rect and
//     fill_poly address whole pixels, so they use this.
//   - caml_gr_xc / caml_gr_yc: the centre of the pixel (x+0.5, row+0.5).
//     Strokes use it so a 1px line lands crisply on the pixel grid, and
//     curved fills use it so a disc/ellipse is centred on the pixel rather
//     than on the pixel corner (which would shift it half a pixel).

//Provides: caml_gr_y
function caml_gr_y(s, y) {
  return s.height - 1 - y;
}

//Provides: caml_gr_xc
function caml_gr_xc(x) {
  return x + 0.5;
}

//Provides: caml_gr_yc
//Requires: caml_gr_y
function caml_gr_yc(s, y) {
  return caml_gr_y(s, y) + 0.5;
}

//Provides: caml_gr_plot
//Requires: caml_gr_state_get, caml_gr_y
function caml_gr_plot(x, y) {
  var s = caml_gr_state_get();
  var im = s.context.createImageData(1, 1);
  var d = im.data;
  var color = s.color;
  d[0] = (color >> 16) & 0xff; //r
  (d[1] =
    (color >> 8) &
    0xff), //g
    (d[2] = (color >> 0) & 0xff); //b
  d[3] = 0xff; //a
  s.x = x;
  s.y = y;
  s.context.putImageData(im, x, caml_gr_y(s, y));
  return 0;
}

//Provides: caml_gr_point_color
//Requires: caml_gr_state_get, caml_gr_y
function caml_gr_point_color(x, y) {
  var s = caml_gr_state_get();
  var im = s.context.getImageData(x, caml_gr_y(s, y), 1, 1);
  var d = im.data;
  return (d[0] << 16) + (d[1] << 8) + d[2];
}
//Provides: caml_gr_moveto
//Requires: caml_gr_state_get
function caml_gr_moveto(x, y) {
  var s = caml_gr_state_get();
  s.x = x;
  s.y = y;
  return 0;
}

//Provides: caml_gr_current_x
//Requires: caml_gr_state_get
function caml_gr_current_x() {
  var s = caml_gr_state_get();
  return s.x;
}
//Provides: caml_gr_current_y
//Requires: caml_gr_state_get
function caml_gr_current_y() {
  var s = caml_gr_state_get();
  return s.y;
}
//Provides: caml_gr_lineto
//Requires: caml_gr_state_get, caml_gr_xc, caml_gr_yc
function caml_gr_lineto(x, y) {
  var s = caml_gr_state_get();
  s.context.beginPath();
  s.context.moveTo(caml_gr_xc(s.x), caml_gr_yc(s, s.y));
  s.context.lineTo(caml_gr_xc(x), caml_gr_yc(s, y));
  s.context.stroke();
  s.x = x;
  s.y = y;
  return 0;
}
//Provides: caml_gr_draw_rect
//Requires: caml_gr_state_get, caml_gr_xc, caml_gr_yc
function caml_gr_draw_rect(x, y, w, h) {
  var s = caml_gr_state_get();
  // explicit path rather than strokeRect: strokeRect renders its left edge
  // half a pixel off from its right edge in some browsers
  var x0 = caml_gr_xc(x);
  var y0 = caml_gr_yc(s, y);
  var x1 = caml_gr_xc(x + w);
  var y1 = caml_gr_yc(s, y + h);
  s.context.beginPath();
  s.context.moveTo(x0, y0);
  s.context.lineTo(x1, y0);
  s.context.lineTo(x1, y1);
  s.context.lineTo(x0, y1);
  // close with an explicit lineTo, not closePath(): the closePath segment is
  // anti-aliased half a pixel off, while an explicit edge stays crisp
  s.context.lineTo(x0, y0);
  s.context.stroke();
  return 0;
}

//Provides: caml_gr_arc_aux
function caml_gr_arc_aux(ctx, cx, cy, ry, rx, a1, a2) {
  while (a1 > a2) a2 += 360;
  a1 /= 180;
  a2 /= 180;
  var rot = 0,
    xPos,
    yPos,
    xPos_prev,
    yPos_prev;
  var space = 2;
  var num = (((a2 - a1) * Math.PI * ((rx + ry) / 2)) / space) | 0;
  var delta = ((a2 - a1) * Math.PI) / num;
  // negate the angle: the canvas y axis is flipped, so the OCaml angle
  // is the negative of the canvas angle
  var i = -a1 * Math.PI;
  for (var j = 0; j <= num; j++) {
    xPos =
      cx -
      rx * Math.sin(i) * Math.sin(rot * Math.PI) +
      ry * Math.cos(i) * Math.cos(rot * Math.PI);
    xPos = xPos.toFixed(2);
    yPos =
      cy +
      ry * Math.cos(i) * Math.sin(rot * Math.PI) +
      rx * Math.sin(i) * Math.cos(rot * Math.PI);
    yPos = yPos.toFixed(2);
    if (j === 0) {
      ctx.moveTo(xPos, yPos);
    } else if (xPos_prev !== xPos || yPos_prev !== yPos) {
      ctx.lineTo(xPos, yPos);
    }
    xPos_prev = xPos;
    yPos_prev = yPos;
    i -= delta; //ccw
  }
  return 0;
}

//Provides: caml_gr_draw_arc
//Requires: caml_gr_state_get, caml_gr_arc_aux, caml_gr_xc, caml_gr_yc
function caml_gr_draw_arc(x, y, rx, ry, a1, a2) {
  var s = caml_gr_state_get();
  s.context.beginPath();
  caml_gr_arc_aux(s.context, caml_gr_xc(x), caml_gr_yc(s, y), rx, ry, a1, a2);
  s.context.stroke();
  return 0;
}

//Provides: caml_gr_set_line_width
//Requires: caml_gr_state_get
function caml_gr_set_line_width(w) {
  var s = caml_gr_state_get();
  s.line_width = w;
  s.context.lineWidth = w;
  // resizing the canvas resets these to their defaults; restore them
  s.context.lineCap = "round";
  s.context.lineJoin = "round";
  return 0;
}

//Provides: caml_gr_fill_rect
//Requires: caml_gr_state_get, caml_gr_y
function caml_gr_fill_rect(x, y, w, h) {
  var s = caml_gr_state_get();
  // match the native X11 backend, which fills (w+1)x(h+1) pixels: the far
  // edges (column x+w and row y+h) are included. The bottom-left pixel is
  // (x, caml_gr_y(s, y)); fill up and to the right from there.
  s.context.fillRect(x, caml_gr_y(s, y) - h, w + 1, h + 1);
  return 0;
}
//Provides: caml_gr_fill_poly
//Requires: caml_gr_state_get, caml_gr_y
function caml_gr_fill_poly(ar) {
  var s = caml_gr_state_get();
  s.context.beginPath();
  s.context.moveTo(ar[1][1], caml_gr_y(s, ar[1][2]));
  for (var i = 2; i < ar.length; i++)
    s.context.lineTo(ar[i][1], caml_gr_y(s, ar[i][2]));
  s.context.lineTo(ar[1][1], caml_gr_y(s, ar[1][2]));
  s.context.fill();
  return 0;
}

//Provides: caml_gr_fill_arc
//Requires: caml_gr_state_get, caml_gr_arc_aux, caml_gr_xc, caml_gr_yc
function caml_gr_fill_arc(x, y, rx, ry, a1, a2) {
  var s = caml_gr_state_get();
  // centre the arc on the pixel (x, height-1-y), as native does, instead of
  // on the pixel corner (which shifts a filled ellipse/disc half a pixel)
  var cx = caml_gr_xc(x);
  var cy = caml_gr_yc(s, y);
  s.context.beginPath();
  caml_gr_arc_aux(s.context, cx, cy, rx, ry, a1, a2);
  // close through the centre so a partial arc fills a pie slice (like the
  // native X11 backend), not the circular segment a bare fill() would give
  s.context.lineTo(cx, cy);
  s.context.fill();
  return 0;
}

//Provides: caml_gr_draw_str
//Requires: caml_gr_state_get, caml_gr_y
function caml_gr_draw_str(str) {
  var s = caml_gr_state_get();
  var m = s.context.measureText(str);
  var dx = m.width;
  // the text baseline sits at the current point; it is placed one row below
  // the pixel row (the continuous bottom-left-origin coordinate), which is
  // caml_gr_y(s, s.y) + 1
  s.context.fillText(str, s.x, caml_gr_y(s, s.y) + 1);
  s.x += dx | 0;
  return 0;
}

//Provides: caml_gr_draw_char
//Requires: caml_gr_draw_str
function caml_gr_draw_char(c) {
  caml_gr_draw_str(String.fromCharCode(c));
  return 0;
}

//Provides: caml_gr_draw_string
//Requires: caml_gr_draw_str
//Requires: caml_jsstring_of_string
function caml_gr_draw_string(str) {
  caml_gr_draw_str(caml_jsstring_of_string(str));
  return 0;
}

//Provides: caml_gr_set_font
//Requires: caml_gr_state_get
//Requires: caml_jsstring_of_string
function caml_gr_set_font(f) {
  var s = caml_gr_state_get();
  s.font = f;
  s.context.font = s.text_size + "px " + caml_jsstring_of_string(s.font);
  return 0;
}

//Provides: caml_gr_set_text_size
//Requires: caml_gr_state_get
//Requires: caml_jsstring_of_string
function caml_gr_set_text_size(size) {
  var s = caml_gr_state_get();
  s.text_size = size;
  s.context.font = s.text_size + "px " + caml_jsstring_of_string(s.font);
  return 0;
}

//Provides: caml_gr_text_size
//Requires: caml_gr_state_get
//Requires: caml_jsstring_of_string
function caml_gr_text_size(txt) {
  var s = caml_gr_state_get();
  var w = s.context.measureText(caml_jsstring_of_string(txt)).width;
  return [0, w, s.text_size];
}

//Provides: caml_gr_make_image
//Requires: caml_gr_state_get
function caml_gr_make_image(arr) {
  var s = caml_gr_state_get();
  var h = arr.length - 1;
  var w = arr[1].length - 1;
  var im = s.context.createImageData(w, h);
  for (var i = 0; i < h; i++) {
    for (var j = 0; j < w; j++) {
      var c = arr[i + 1][j + 1];
      var o = i * (w * 4) + j * 4;
      if (c === -1) {
        im.data[o + 0] = 0;
        im.data[o + 1] = 0;
        im.data[o + 2] = 0;
        im.data[o + 3] = 0;
      } else {
        im.data[o + 0] = (c >> 16) & 0xff;
        im.data[o + 1] = (c >> 8) & 0xff;
        im.data[o + 2] = (c >> 0) & 0xff;
        im.data[o + 3] = 0xff;
      }
    }
  }
  return im;
}
//Provides: caml_gr_dump_image
//Requires: caml_gr_state_get
function caml_gr_dump_image(im) {
  var data = [0];
  for (var i = 0; i < im.height; i++) {
    data[i + 1] = [0];
    for (var j = 0; j < im.width; j++) {
      var o = i * (im.width * 4) + j * 4,
        r = im.data[o + 0],
        g = im.data[o + 1],
        b = im.data[o + 2];
      data[i + 1][j + 1] = (r << 16) + (g << 8) + b;
    }
  }
  return data;
}
//Provides: caml_gr_draw_image
//Requires: caml_gr_state_get, caml_gr_y
function caml_gr_draw_image(im, x, y) {
  var s = caml_gr_state_get();
  if (!im.image) {
    var canvas = document.createElement("canvas");
    canvas.width = s.width;
    canvas.height = s.height;
    canvas.getContext("2d").putImageData(im, 0, 0);
    // a canvas is a valid drawImage source immediately, so draw it
    // directly instead of round-tripping through an Image + data URL
    // (which made the first draw asynchronous)
    im.image = canvas;
  }
  // the image's bottom row is caml_gr_y(s, y); drawImage takes the top-left
  s.context.drawImage(im.image, x, caml_gr_y(s, y) - im.height + 1);
  return 0;
}
//Provides: caml_gr_create_image
//Requires: caml_gr_state_get
function caml_gr_create_image(x, y) {
  var s = caml_gr_state_get();
  return s.context.createImageData(x, y);
}
//Provides: caml_gr_blit_image
//Requires: caml_gr_state_get, caml_gr_y
function caml_gr_blit_image(im, x, y) {
  var s = caml_gr_state_get();
  // the image's bottom row is caml_gr_y(s, y); getImageData takes the top-left
  var im2 = s.context.getImageData(
    x,
    caml_gr_y(s, y) - im.height + 1,
    im.width,
    im.height,
  );
  for (var i = 0; i < im2.data.length; i += 4) {
    im.data[i] = im2.data[i];
    im.data[i + 1] = im2.data[i + 1];
    im.data[i + 2] = im2.data[i + 2];
    im.data[i + 3] = im2.data[i + 3];
  }
  return 0;
}
//Provides: caml_gr_sigio_handler
function caml_gr_sigio_handler() {
  return 0;
}
//Provides: caml_gr_sigio_signal
function caml_gr_sigio_signal() {
  return 0;
}
//Provides: caml_gr_wait_event
//Requires: caml_failwith
function caml_gr_wait_event(_evl) {
  caml_failwith("caml_gr_wait_event not Implemented: use Graphics_js instead");
}

//Provides: caml_gr_synchronize
//Requires: caml_failwith
function caml_gr_synchronize() {
  caml_failwith("caml_gr_synchronize not Implemented");
}
//Provides: caml_gr_remember_mode
//Requires: caml_failwith
function caml_gr_remember_mode() {
  caml_failwith("caml_gr_remember_mode not Implemented");
}
//Provides: caml_gr_display_mode
//Requires: caml_failwith
function caml_gr_display_mode() {
  caml_failwith("caml_gr_display_mode not Implemented");
}

//Provides: caml_gr_window_id
//Requires: caml_failwith
function caml_gr_window_id(_a) {
  caml_failwith("caml_gr_window_id not Implemented");
}

//Provides: caml_gr_open_subwindow
//Requires: caml_failwith
function caml_gr_open_subwindow(_a, _b, _c, _d) {
  caml_failwith("caml_gr_open_subwindow not Implemented");
}

//Provides: caml_gr_close_subwindow
//Requires: caml_failwith
function caml_gr_close_subwindow(_a) {
  caml_failwith("caml_gr_close_subwindow not Implemented");
}

// ---- Wasm helpers ----
// These functions take/return plain JS values (ints, JS strings, JS objects)
// and access caml_gr_state directly. State checking is done in WAT.

//Provides: gr_state_for_wasm
//Requires: caml_gr_state
//If: wasm
function gr_state_for_wasm() {
  if (caml_gr_state) return caml_gr_state;
  return null;
}

//Provides: gr_state_create_for_wasm
//If: wasm
function gr_state_create_for_wasm(canvas, w, h) {
  var context = canvas.getContext("2d");
  context.lineCap = "round";
  context.lineJoin = "round";
  return {
    context: context,
    canvas: canvas,
    x: 0,
    y: 0,
    width: w,
    height: h,
    line_width: 1,
    font: "fixed",
    text_size: 26,
    color: 0x000000,
    title: "",
  };
}

//Provides: gr_state_init_for_wasm
//Requires: caml_gr_state
//Requires: gr_set_color_for_wasm, gr_moveto_for_wasm, gr_resize_window_for_wasm
//Requires: gr_set_line_width_for_wasm, gr_set_text_size_for_wasm
//Requires: gr_set_font_for_wasm, gr_set_window_title_for_wasm
//If: wasm
function gr_state_init_for_wasm() {
  var s = caml_gr_state;
  gr_moveto_for_wasm(s.x, s.y);
  gr_resize_window_for_wasm(s.width, s.height);
  gr_set_line_width_for_wasm(s.line_width);
  gr_set_text_size_for_wasm(s.text_size);
  gr_set_font_for_wasm(s.font);
  gr_set_color_for_wasm(s.color);
  gr_set_window_title_for_wasm(s.title);
  s.context.textBaseline = "bottom";
}

//Provides: gr_state_set_for_wasm
//Requires: caml_gr_state, gr_state_init_for_wasm
//If: wasm
function gr_state_set_for_wasm(ctx) {
  caml_gr_state = ctx;
  gr_state_init_for_wasm();
}

//Provides: gr_open_for_wasm
//Requires: gr_state_create_for_wasm, gr_state_set_for_wasm
//If: wasm
function gr_open_for_wasm(info) {
  function get(name) {
    var res = info.match("(^|,) *" + name + " *= *([a-zA-Z0-9_]+) *(,|$)");
    if (res) return res[2];
  }
  var specs = [];
  if (!(info === "")) specs.push(info);
  var target = get("target");
  if (!target) target = "";
  var status = get("status");
  if (!status) specs.push("status=1");
  var w = get("width");
  w = w ? Number.parseInt(w) : 200;
  specs.push("width=" + w);
  var h = get("height");
  h = h ? Number.parseInt(h) : 200;
  specs.push("height=" + h);
  var win = globalThis.open("about:blank", target, specs.join(","));
  if (!win) return -1;
  var doc = win.document;
  var canvas = doc.createElement("canvas");
  canvas.width = w;
  canvas.height = h;
  var ctx = gr_state_create_for_wasm(canvas, w, h);
  ctx.set_title = function (title) {
    doc.title = title;
  };
  gr_state_set_for_wasm(ctx);
  var body = doc.body;
  body.style.margin = "0px";
  body.appendChild(canvas);
  return 0;
}

//Provides: gr_close_for_wasm
//Requires: caml_gr_state
//If: wasm
function gr_close_for_wasm() {
  caml_gr_state.canvas.width = 0;
  caml_gr_state.canvas.height = 0;
}

//Provides: gr_clear_for_wasm
//Requires: caml_gr_state
//If: wasm
function gr_clear_for_wasm() {
  var s = caml_gr_state;
  s.context.clearRect(0, 0, s.canvas.width, s.canvas.height);
}

//Provides: gr_size_x_for_wasm
//Requires: caml_gr_state
//If: wasm
function gr_size_x_for_wasm() {
  return caml_gr_state.width;
}

//Provides: gr_size_y_for_wasm
//Requires: caml_gr_state
//If: wasm
function gr_size_y_for_wasm() {
  return caml_gr_state.height;
}

//Provides: gr_current_x_for_wasm
//Requires: caml_gr_state
//If: wasm
function gr_current_x_for_wasm() {
  return caml_gr_state.x;
}

//Provides: gr_current_y_for_wasm
//Requires: caml_gr_state
//If: wasm
function gr_current_y_for_wasm() {
  return caml_gr_state.y;
}

//Provides: gr_moveto_for_wasm
//Requires: caml_gr_state
//If: wasm
function gr_moveto_for_wasm(x, y) {
  caml_gr_state.x = x;
  caml_gr_state.y = y;
}

//Provides: gr_set_color_for_wasm
//Requires: caml_gr_state
//If: wasm
function gr_set_color_for_wasm(color) {
  var s = caml_gr_state;
  function convert(number) {
    var str = "" + number.toString(16);
    while (str.length < 2) str = "0" + str;
    return str;
  }
  var r = (color >> 16) & 0xff,
    g = (color >> 8) & 0xff,
    b = (color >> 0) & 0xff;
  s.color = color;
  var c_str = "#" + convert(r) + convert(g) + convert(b);
  s.context.fillStyle = c_str;
  s.context.strokeStyle = c_str;
}

//Provides: gr_plot_for_wasm
//Requires: caml_gr_state, caml_gr_y
//If: wasm
function gr_plot_for_wasm(x, y) {
  var s = caml_gr_state;
  var im = s.context.createImageData(1, 1);
  var d = im.data;
  var color = s.color;
  d[0] = (color >> 16) & 0xff;
  d[1] = (color >> 8) & 0xff;
  d[2] = (color >> 0) & 0xff;
  d[3] = 0xff;
  s.x = x;
  s.y = y;
  s.context.putImageData(im, x, caml_gr_y(s, y));
}

//Provides: gr_point_color_for_wasm
//Requires: caml_gr_state, caml_gr_y
//If: wasm
function gr_point_color_for_wasm(x, y) {
  var s = caml_gr_state;
  var im = s.context.getImageData(x, caml_gr_y(s, y), 1, 1);
  var d = im.data;
  return (d[0] << 16) + (d[1] << 8) + d[2];
}

//Provides: gr_lineto_for_wasm
//Requires: caml_gr_state, caml_gr_xc, caml_gr_yc
//If: wasm
function gr_lineto_for_wasm(x, y) {
  var s = caml_gr_state;
  s.context.beginPath();
  s.context.moveTo(caml_gr_xc(s.x), caml_gr_yc(s, s.y));
  s.context.lineTo(caml_gr_xc(x), caml_gr_yc(s, y));
  s.context.stroke();
  s.x = x;
  s.y = y;
}

//Provides: gr_draw_rect_for_wasm
//Requires: caml_gr_state, caml_gr_xc, caml_gr_yc
//If: wasm
function gr_draw_rect_for_wasm(x, y, w, h) {
  var s = caml_gr_state;
  // explicit path (see caml_gr_draw_rect): strokeRect can render its left
  // edge half a pixel off from its right edge
  var x0 = caml_gr_xc(x);
  var y0 = caml_gr_yc(s, y);
  var x1 = caml_gr_xc(x + w);
  var y1 = caml_gr_yc(s, y + h);
  s.context.beginPath();
  s.context.moveTo(x0, y0);
  s.context.lineTo(x1, y0);
  s.context.lineTo(x1, y1);
  s.context.lineTo(x0, y1);
  s.context.lineTo(x0, y0);
  s.context.stroke();
}

//Provides: gr_fill_rect_for_wasm
//Requires: caml_gr_state, caml_gr_y
//If: wasm
function gr_fill_rect_for_wasm(x, y, w, h) {
  // match the native X11 backend: fill (w+1)x(h+1) pixels (see caml_gr_fill_rect)
  var s = caml_gr_state;
  s.context.fillRect(x, caml_gr_y(s, y) - h, w + 1, h + 1);
}

//Provides: gr_draw_arc_for_wasm
//Requires: caml_gr_state, caml_gr_arc_aux, caml_gr_xc, caml_gr_yc
//If: wasm
function gr_draw_arc_for_wasm(x, y, rx, ry, a1, a2) {
  var s = caml_gr_state;
  s.context.beginPath();
  caml_gr_arc_aux(s.context, caml_gr_xc(x), caml_gr_yc(s, y), rx, ry, a1, a2);
  s.context.stroke();
}

//Provides: gr_fill_arc_for_wasm
//Requires: caml_gr_state, caml_gr_arc_aux, caml_gr_xc, caml_gr_yc
//If: wasm
function gr_fill_arc_for_wasm(x, y, rx, ry, a1, a2) {
  var s = caml_gr_state;
  var cx = caml_gr_xc(x);
  var cy = caml_gr_yc(s, y);
  s.context.beginPath();
  caml_gr_arc_aux(s.context, cx, cy, rx, ry, a1, a2);
  // pie slice (close through the centre), matching native fill_arc
  s.context.lineTo(cx, cy);
  s.context.fill();
}

//Provides: gr_set_line_width_for_wasm
//Requires: caml_gr_state
//If: wasm
function gr_set_line_width_for_wasm(w) {
  caml_gr_state.line_width = w;
  caml_gr_state.context.lineWidth = w;
  caml_gr_state.context.lineCap = "round";
  caml_gr_state.context.lineJoin = "round";
}

//Provides: gr_resize_window_for_wasm
//Requires: caml_gr_state
//If: wasm
function gr_resize_window_for_wasm(w, h) {
  var s = caml_gr_state;
  s.width = w;
  s.height = h;
  if (w !== s.canvas.width) s.canvas.width = w;
  if (h !== s.canvas.height) s.canvas.height = h;
}

//Provides: gr_draw_char_for_wasm
//Requires: gr_draw_str_for_wasm
//If: wasm
function gr_draw_char_for_wasm(c) {
  gr_draw_str_for_wasm(String.fromCharCode(c));
}

//Provides: gr_draw_str_for_wasm
//Requires: caml_gr_state, caml_gr_y
//If: wasm
function gr_draw_str_for_wasm(str) {
  var s = caml_gr_state;
  var m = s.context.measureText(str);
  var dx = m.width;
  // text baseline = current point, one row below the pixel (see caml_gr_draw_str)
  s.context.fillText(str, s.x, caml_gr_y(s, s.y) + 1);
  s.x += dx | 0;
}

//Provides: gr_set_font_for_wasm
//Requires: caml_gr_state
//If: wasm
function gr_set_font_for_wasm(f) {
  var s = caml_gr_state;
  s.font = f;
  s.context.font = s.text_size + "px " + f;
}

//Provides: gr_set_text_size_for_wasm
//Requires: caml_gr_state
//If: wasm
function gr_set_text_size_for_wasm(size) {
  var s = caml_gr_state;
  s.text_size = size;
  s.context.font = s.text_size + "px " + s.font;
}

//Provides: gr_set_window_title_for_wasm
//Requires: caml_gr_state
//If: wasm
function gr_set_window_title_for_wasm(name) {
  var s = caml_gr_state;
  s.title = name;
  if (s.set_title) s.set_title(name);
}

//Provides: gr_text_size_w_for_wasm
//Requires: caml_gr_state
//If: wasm
function gr_text_size_w_for_wasm(txt) {
  return caml_gr_state.context.measureText(txt).width | 0;
}

//Provides: gr_text_size_h_for_wasm
//Requires: caml_gr_state
//If: wasm
function gr_text_size_h_for_wasm() {
  return caml_gr_state.text_size;
}

//Provides: gr_fill_poly_for_wasm
//Requires: caml_gr_state, caml_gr_y
//If: wasm
function gr_fill_poly_for_wasm(ar, n) {
  var s = caml_gr_state;
  s.context.beginPath();
  s.context.moveTo(ar[0], caml_gr_y(s, ar[1]));
  for (var i = 1; i < n; i++)
    s.context.lineTo(ar[i * 2], caml_gr_y(s, ar[i * 2 + 1]));
  s.context.lineTo(ar[0], caml_gr_y(s, ar[1]));
  s.context.fill();
}

//Provides: gr_create_image_for_wasm
//Requires: caml_gr_state
//If: wasm
function gr_create_image_for_wasm(x, y) {
  return caml_gr_state.context.createImageData(x, y);
}

//Provides: gr_draw_image_for_wasm
//Requires: caml_gr_state, caml_gr_y
//If: wasm
function gr_draw_image_for_wasm(im, x, y) {
  var s = caml_gr_state;
  if (!im.image) {
    var canvas = document.createElement("canvas");
    canvas.width = s.width;
    canvas.height = s.height;
    canvas.getContext("2d").putImageData(im, 0, 0);
    // a canvas is a valid drawImage source immediately, so draw it
    // directly instead of round-tripping through an Image + data URL
    // (which made the first draw asynchronous)
    im.image = canvas;
  }
  // the image's bottom row is caml_gr_y(s, y); drawImage takes the top-left
  s.context.drawImage(im.image, x, caml_gr_y(s, y) - im.height + 1);
}

//Provides: gr_blit_image_for_wasm
//Requires: caml_gr_state, caml_gr_y
//If: wasm
function gr_blit_image_for_wasm(im, x, y) {
  var s = caml_gr_state;
  // the image's bottom row is caml_gr_y(s, y); getImageData takes the top-left
  var im2 = s.context.getImageData(
    x,
    caml_gr_y(s, y) - im.height + 1,
    im.width,
    im.height,
  );
  for (var i = 0; i < im2.data.length; i += 4) {
    im.data[i] = im2.data[i];
    im.data[i + 1] = im2.data[i + 1];
    im.data[i + 2] = im2.data[i + 2];
    im.data[i + 3] = im2.data[i + 3];
  }
}

//Provides: gr_make_image_for_wasm
//Requires: caml_gr_state
//If: wasm
function gr_make_image_for_wasm(pixels, w, h) {
  var s = caml_gr_state;
  var im = s.context.createImageData(w, h);
  for (var i = 0; i < h; i++) {
    for (var j = 0; j < w; j++) {
      var c = pixels[i * w + j];
      var o = i * (w * 4) + j * 4;
      if (c === -1) {
        im.data[o + 0] = 0;
        im.data[o + 1] = 0;
        im.data[o + 2] = 0;
        im.data[o + 3] = 0;
      } else {
        im.data[o + 0] = (c >> 16) & 0xff;
        im.data[o + 1] = (c >> 8) & 0xff;
        im.data[o + 2] = (c >> 0) & 0xff;
        im.data[o + 3] = 0xff;
      }
    }
  }
  return im;
}

//Provides: gr_dump_image_width_for_wasm
//If: wasm
function gr_dump_image_width_for_wasm(im) {
  return im.width;
}

//Provides: gr_dump_image_height_for_wasm
//If: wasm
function gr_dump_image_height_for_wasm(im) {
  return im.height;
}

//Provides: gr_dump_image_pixel_for_wasm
//If: wasm
function gr_dump_image_pixel_for_wasm(im, i, j) {
  var o = i * (im.width * 4) + j * 4;
  return (im.data[o] << 16) + (im.data[o + 1] << 8) + im.data[o + 2];
}

//Provides: gr_doc_of_state_for_wasm
//If: wasm
function gr_doc_of_state_for_wasm(state) {
  if (state.canvas.ownerDocument) return state.canvas.ownerDocument;
  return null;
}
