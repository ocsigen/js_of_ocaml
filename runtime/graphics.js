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
let caml_gr_state;

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
  const info_ = caml_jsstring_of_string(info);
  function get(name) {
    const res = info.match("(^|,) *" + name + " *= *([a-zA-Z0-9_]+) *(,|$)");
    if (res) return res[2];
  }
  const specs = [];
  if (!(info_ == "")) specs.push(info_);
  let target = get("target");
  if (!target) target = "";
  const status = get("status");
  if (!status) specs.push("status=1");

  let w = get("width");
  w = w ? Number.parseInt(w) : 200;
  specs.push("width=" + w);

  let h = get("height");
  h = h ? Number.parseInt(h) : 200;
  specs.push("height=" + h);

  const win = globalThis.open("about:blank", target, specs.join(","));
  if (!win) {
    caml_failwith("Graphics.open_graph: cannot open the window");
  }
  const doc = win.document;
  const canvas = doc.createElement("canvas");
  canvas.width = w;
  canvas.height = h;
  const ctx = caml_gr_state_create(canvas, w, h);
  ctx.set_title = (title) => {
    doc.title = title;
  };
  caml_gr_state_set(ctx);
  const body = doc.body;
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
  const context = canvas.getContext("2d");
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
  const s = caml_gr_state_get();
  s.canvas.width = 0;
  s.canvas.height = 0;
  return 0;
}

//Provides: caml_gr_set_window_title
//Requires: caml_gr_state_get
//Requires: caml_jsstring_of_string
function caml_gr_set_window_title(name) {
  const s = caml_gr_state_get();
  s.title = name;
  const jsname = caml_jsstring_of_string(name);
  if (s.set_title) s.set_title(jsname);
  return 0;
}

//Provides: caml_gr_resize_window
//Requires: caml_gr_state_get
function caml_gr_resize_window(w, h) {
  const s = caml_gr_state_get();
  s.width = w;
  s.height = h;
  s.canvas.width = w;
  s.canvas.height = h;
  return 0;
}

//Provides: caml_gr_clear_graph
//Requires: caml_gr_state_get
function caml_gr_clear_graph() {
  const s = caml_gr_state_get();
  s.canvas.width = s.width;
  s.canvas.height = s.height;
  //  s.context.strokeRect (0., 0., s.width, s.height);
  return 0;
}

//Provides: caml_gr_size_x
//Requires: caml_gr_state_get
function caml_gr_size_x() {
  const s = caml_gr_state_get();
  return s.width;
}
//Provides: caml_gr_size_y
//Requires: caml_gr_state_get
function caml_gr_size_y() {
  const s = caml_gr_state_get();
  return s.height;
}

//Provides: caml_gr_set_color
//Requires: caml_gr_state_get
function caml_gr_set_color(color) {
  const s = caml_gr_state_get();
  function convert(number) {
    let str = "" + number.toString(16);
    while (str.length < 2) str = "0" + str;
    return str;
  }
  const r = (color >> 16) & 0xff;
  const g = (color >> 8) & 0xff;
  const b = (color >> 0) & 0xff;
  s.color = color;
  const c_str = "#" + convert(r) + convert(g) + convert(b);
  s.context.fillStyle = c_str;
  s.context.strokeStyle = c_str;
  return 0;
}
//Provides: caml_gr_plot
//Requires: caml_gr_state_get
function caml_gr_plot(x, y) {
  const s = caml_gr_state_get();
  const im = s.context.createImageData(1, 1);
  const d = im.data;
  const color = s.color;
  d[0] = (color >> 16) & 0xff; //r
  d[1] = (color >> 8) & 0xff; //g
  d[2] = (color >> 0) & 0xff; //b
  d[3] = 0xff; //a
  s.x = x;
  s.y = y;
  s.context.putImageData(im, x, s.height - y);
  return 0;
}

//Provides: caml_gr_point_color
//Requires: caml_gr_state_get
function caml_gr_point_color(x, y) {
  const s = caml_gr_state_get();
  const im = s.context.getImageData(x, s.height - y, 1, 1);
  const d = im.data;
  return (d[0] << 16) + (d[1] << 8) + d[2];
}
//Provides: caml_gr_moveto
//Requires: caml_gr_state_get
function caml_gr_moveto(x, y) {
  const s = caml_gr_state_get();
  s.x = x;
  s.y = y;
  return 0;
}

//Provides: caml_gr_current_x
//Requires: caml_gr_state_get
function caml_gr_current_x() {
  const s = caml_gr_state_get();
  return s.x;
}
//Provides: caml_gr_current_y
//Requires: caml_gr_state_get
function caml_gr_current_y() {
  const s = caml_gr_state_get();
  return s.y;
}
//Provides: caml_gr_lineto
//Requires: caml_gr_state_get
function caml_gr_lineto(x, y) {
  const s = caml_gr_state_get();
  s.context.beginPath();
  s.context.moveTo(s.x, s.height - s.y);
  s.context.lineTo(x, s.height - y);
  s.context.stroke();
  s.x = x;
  s.y = y;
  return 0;
}
//Provides: caml_gr_draw_rect
//Requires: caml_gr_state_get
function caml_gr_draw_rect(x, y, w, h) {
  const s = caml_gr_state_get();
  s.context.strokeRect(x, s.height - y, w, -h);
  return 0;
}

//Provides: caml_gr_arc_aux
function caml_gr_arc_aux(ctx, cx, cy, ry, rx, a1, a2) {
  while (a1 > a2) a2 += 360;
  a1 /= 180;
  a2 /= 180;
  const rot = 0;
  let xPos;
  let yPos;
  let xPos_prev;
  let yPos_prev;
  const space = 2;
  const num = (((a2 - a1) * Math.PI * ((rx + ry) / 2)) / space) | 0;
  const delta = ((a2 - a1) * Math.PI) / num;
  let i = a1 * Math.PI;
  for (let j = 0; j <= num; j++) {
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
    if (j == 0) {
      ctx.moveTo(xPos, yPos);
    } else if (xPos_prev != xPos || yPos_prev != yPos) {
      ctx.lineTo(xPos, yPos);
    }
    xPos_prev = xPos;
    yPos_prev = yPos;
    i -= delta; //ccw
  }
  return 0;
}

//Provides: caml_gr_draw_arc
//Requires: caml_gr_state_get, caml_gr_arc_aux
function caml_gr_draw_arc(x, y, rx, ry, a1, a2) {
  const s = caml_gr_state_get();
  s.context.beginPath();
  caml_gr_arc_aux(s.context, x, s.height - y, rx, ry, a1, a2);
  s.context.stroke();
  return 0;
}

//Provides: caml_gr_set_line_width
//Requires: caml_gr_state_get
function caml_gr_set_line_width(w) {
  const s = caml_gr_state_get();
  s.line_width = w;
  s.context.lineWidth = w;
  return 0;
}

//Provides: caml_gr_fill_rect
//Requires: caml_gr_state_get
function caml_gr_fill_rect(x, y, w, h) {
  const s = caml_gr_state_get();
  s.context.fillRect(x, s.height - y, w, -h);
  return 0;
}
//Provides: caml_gr_fill_poly
//Requires: caml_gr_state_get
function caml_gr_fill_poly(ar) {
  const s = caml_gr_state_get();
  s.context.beginPath();
  s.context.moveTo(ar[1][1], s.height - ar[1][2]);
  for (let i = 2; i < ar.length; i++)
    s.context.lineTo(ar[i][1], s.height - ar[i][2]);
  s.context.lineTo(ar[1][1], s.height - ar[1][2]);
  s.context.fill();
  return 0;
}

//Provides: caml_gr_fill_arc
//Requires: caml_gr_state_get, caml_gr_arc_aux
function caml_gr_fill_arc(x, y, rx, ry, a1, a2) {
  const s = caml_gr_state_get();
  s.context.beginPath();
  caml_gr_arc_aux(s.context, x, s.height - y, rx, ry, a1, a2);
  s.context.fill();
  return 0;
}

//Provides: caml_gr_draw_str
//Requires: caml_gr_state_get
function caml_gr_draw_str(str) {
  const s = caml_gr_state_get();
  const m = s.context.measureText(str);
  const dx = m.width;
  s.context.fillText(str, s.x, s.height - s.y);
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
  const s = caml_gr_state_get();
  s.font = f;
  s.context.font = s.text_size + "px " + caml_jsstring_of_string(s.font);
  return 0;
}

//Provides: caml_gr_set_text_size
//Requires: caml_gr_state_get
//Requires: caml_jsstring_of_string
function caml_gr_set_text_size(size) {
  const s = caml_gr_state_get();
  s.text_size = size;
  s.context.font = s.text_size + "px " + caml_jsstring_of_string(s.font);
  return 0;
}

//Provides: caml_gr_text_size
//Requires: caml_gr_state_get
//Requires: caml_jsstring_of_string
function caml_gr_text_size(txt) {
  const s = caml_gr_state_get();
  const w = s.context.measureText(caml_jsstring_of_string(txt)).width;
  return [0, w, s.text_size];
}

//Provides: caml_gr_make_image
//Requires: caml_gr_state_get
function caml_gr_make_image(arr) {
  const s = caml_gr_state_get();
  const h = arr.length - 1;
  const w = arr[1].length - 1;
  const im = s.context.createImageData(w, h);
  for (let i = 0; i < h; i++) {
    for (let j = 0; j < w; j++) {
      const c = arr[i + 1][j + 1];
      const o = i * (w * 4) + j * 4;
      if (c == -1) {
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
  const data = [0];
  for (let i = 0; i < im.height; i++) {
    data[i + 1] = [0];
    for (let j = 0; j < im.width; j++) {
      const o = i * (im.width * 4) + j * 4;
      const r = im.data[o + 0];
      const g = im.data[o + 1];
      const b = im.data[o + 2];
      data[i + 1][j + 1] = (r << 16) + (g << 8) + b;
    }
  }
  return data;
}
//Provides: caml_gr_draw_image
//Requires: caml_gr_state_get
function caml_gr_draw_image(im, x, y) {
  const s = caml_gr_state_get();
  if (!im.image) {
    const canvas = document.createElement("canvas");
    canvas.width = s.width;
    canvas.height = s.height;
    canvas.getContext("2d").putImageData(im, 0, 0);
    const image = new globalThis.Image();
    image.onload = () => {
      s.context.drawImage(image, x, s.height - im.height - y);
      im.image = image;
    };
    image.src = canvas.toDataURL("image/png");
  } else {
    s.context.drawImage(im.image, x, s.height - im.height - y);
  }
  return 0;
}
//Provides: caml_gr_create_image
//Requires: caml_gr_state_get
function caml_gr_create_image(x, y) {
  const s = caml_gr_state_get();
  return s.context.createImageData(x, y);
}
//Provides: caml_gr_blit_image
//Requires: caml_gr_state_get
function caml_gr_blit_image(im, x, y) {
  const s = caml_gr_state_get();
  const im2 = s.context.getImageData(
    x,
    s.height - im.height - y,
    im.width,
    im.height,
  );
  for (let i = 0; i < im2.data.length; i += 4) {
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
function caml_gr_window_id(a) {
  caml_failwith("caml_gr_window_id not Implemented");
}

//Provides: caml_gr_open_subwindow
//Requires: caml_failwith
function caml_gr_open_subwindow(a, b, c, d) {
  caml_failwith("caml_gr_open_subwindow not Implemented");
}

//Provides: caml_gr_close_subwindow
//Requires: caml_failwith
function caml_gr_close_subwindow(a) {
  caml_failwith("caml_gr_close_subwindow not Implemented");
}
