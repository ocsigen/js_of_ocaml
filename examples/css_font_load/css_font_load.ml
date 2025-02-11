open Js_of_ocaml

let is_loading = ref true

let render_loop (context : Dom_html.canvasRenderingContext2D Js.t) =
  let next_time = Js.float 15. in
  let rec inner () =
    context##save;
    context##.fillStyle := Js.string "rgb(0, 0, 0)";
    context##fillRect (Js.float 0.) (Js.float 0.) (Js.float 800.) (Js.float 600.);
    context##.fillStyle := Js.string "rgb(255,255,255)";
    context##.textBaseline := Js.string "top";
    context##.lineWidth := Js.float 3.;
    if !is_loading
    then (
      context##.font := Js.string "48px caption";
      context##fillText (Js.string "Loading") (Js.float 0.) (Js.float 0.))
    else (
      context##.font := Js.string "48px caption";
      context##fillText (Js.string "ABCDEFG") (Js.float 0.) (Js.float 0.);
      context##.font := Js.string "48px 'Bebas Neue', caption";
      context##fillText (Js.string "ABCDEFG") (Js.float 0.) (Js.float 50.));
    context##restore;
    ignore @@ Dom_html.window##setTimeout (Js.wrap_callback inner) next_time
  in
  inner ()

let _ =
  let canvas =
    Option.get @@ Dom_html.getElementById_coerce "main-canvas" Dom_html.CoerceTo.canvas
  in
  let context = canvas##getContext Dom_html._2d_ in
  let bebas =
    Css_font.create_font_face
      (Js.string "Bebas Neue")
      (Js.string "url(BebasNeue-Regular.ttf)")
  in
  Css_font.load_and_then bebas (fun () -> is_loading := false);
  Dom_html.document##.fonts##add bebas;
  render_loop context
