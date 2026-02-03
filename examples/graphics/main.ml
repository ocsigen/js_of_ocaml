open Js_of_ocaml
open Graphics_js
module Html = Dom_html

let palette =
  [| rgb 230 80 80
   ; rgb 80 180 230
   ; rgb 100 200 120
   ; rgb 240 180 60
   ; rgb 180 100 220
   ; rgb 230 130 80
   ; rgb 80 210 200
   ; rgb 220 100 160
  |]

let palette_css =
  [| "#e65050"
   ; "#50b4e6"
   ; "#64c878"
   ; "#f0b43c"
   ; "#b464dc"
   ; "#e68250"
   ; "#50d2c8"
   ; "#dc64a0"
  |]

let current_color = ref 0

let brush_size = ref 6

let last_pos : (int * int) option ref = ref None

let draw_scene () =
  let w = size_x () in
  let h = size_y () in
  (* 1. Background: warm white fill *)
  set_color (rgb 245 245 248);
  fill_rect 0 0 w h;
  (* Faint grid *)
  set_color (rgb 232 232 236);
  set_line_width 1;
  let step = 40 in
  let i = ref step in
  while !i < w do
    moveto !i 0;
    lineto !i h;
    i := !i + step
  done;
  i := step;
  while !i < h do
    moveto 0 !i;
    lineto w !i;
    i := !i + step
  done;
  (* Text *)
  set_color (rgb 50 50 70);
  set_text_size 18;
  moveto 248 206;
  draw_string "OCaml Graphics";
  set_text_size 12;
  set_color (rgb 100 100 120);
  moveto 256 186;
  draw_string "click to draw"

let get_class (el : Dom.element Js.t) =
  Js.Opt.case (el##getAttribute (Js.string "class")) (fun () -> "") Js.to_string

let set_class (el : Dom.element Js.t) cls =
  el##setAttribute (Js.string "class") (Js.string cls)

let has_word s w = List.mem w (String.split_on_char ' ' s)

let remove_word s w =
  s |> String.split_on_char ' ' |> List.filter (fun x -> x <> w) |> String.concat " "

let set_active_in_group (group : Html.element Js.t) (active : Html.element Js.t) =
  let children = (group :> Dom.node Js.t)##.childNodes in
  for i = 0 to children##.length - 1 do
    Js.Opt.iter
      (children##item i)
      (fun node ->
        Js.Opt.iter (Dom.CoerceTo.element node) (fun el ->
            set_class el (remove_word (get_class el) "active")))
  done;
  let el = (active :> Dom.element Js.t) in
  let cls = get_class el in
  if not (has_word cls "active") then set_class el (cls ^ " active")

let make_label doc text =
  let span = Html.createSpan doc in
  span##.className := Js.string "tool-label";
  Dom.appendChild span (doc##createTextNode (Js.string text));
  (span :> Html.element Js.t)

let make_color_swatches doc =
  let group = Html.createDiv doc in
  group##.className := Js.string "tool-group";
  Dom.appendChild group (make_label doc "Color");
  let swatches =
    Array.mapi
      (fun i css_color ->
        let swatch = Html.createDiv doc in
        swatch##.className :=
          Js.string (if i = !current_color then "color-swatch active" else "color-swatch");
        swatch##.style##.backgroundColor := Js.string css_color;
        swatch##.onclick :=
          Html.handler (fun _ ->
              current_color := i;
              set_active_in_group
                (group :> Html.element Js.t)
                (swatch :> Html.element Js.t);
              Js._false);
        Dom.appendChild group swatch;
        swatch)
      palette_css
  in
  group, swatches

let make_size_buttons doc =
  let group = Html.createDiv doc in
  group##.className := Js.string "tool-group";
  Dom.appendChild group (make_label doc "Size");
  let sizes = [| "S", 3; "M", 6; "L", 12 |] in
  let btns =
    Array.map
      (fun (label, sz) ->
        let btn = Html.createButton doc in
        btn##.className :=
          Js.string (if !brush_size = sz then "tool-btn active" else "tool-btn");
        btn##.textContent := Js.some (Js.string label);
        btn##.onclick :=
          Html.handler (fun _ ->
              brush_size := sz;
              set_active_in_group (group :> Html.element Js.t) (btn :> Html.element Js.t);
              Js._false);
        Dom.appendChild group btn;
        btn)
      sizes
  in
  group, btns

let make_clear_button doc =
  let btn = Html.createButton doc in
  btn##.className := Js.string "clear-btn";
  btn##.textContent := Js.some (Js.string "Clear");
  btn##.onclick :=
    Html.handler (fun _ ->
        last_pos := None;
        draw_scene ();
        Js._false);
  btn

let build_toolbar doc toolbar_el =
  let color_group, _ = make_color_swatches doc in
  let size_group, _ = make_size_buttons doc in
  let clear = make_clear_button doc in
  Dom.appendChild toolbar_el color_group;
  Dom.appendChild toolbar_el size_group;
  Dom.appendChild toolbar_el clear

let init canvas toolbar_el =
  let doc = Html.document in
  open_canvas canvas;
  build_toolbar doc toolbar_el;
  draw_scene ();
  let drawing = ref false in
  Graphics_js.loop
    [ Graphics_js.Button_down; Graphics_js.Button_up; Graphics_js.Mouse_motion ]
    (fun s ->
      if s.button
      then (
        set_color palette.(!current_color);
        let x = s.mouse_x in
        let y = s.mouse_y in
        let r = !brush_size in
        set_line_width (max 1 (r / 2));
        (match !last_pos with
        | Some (lx, ly) ->
            moveto lx ly;
            lineto x y
        | None -> fill_circle x y (max 1 (r / 4)));
        last_pos := Some (x, y);
        drawing := true)
      else (
        if !drawing then last_pos := None;
        drawing := false))

let () = Js.export "init" init
