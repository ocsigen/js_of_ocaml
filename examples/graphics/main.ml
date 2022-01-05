open Js_of_ocaml
open Graphics_js

let init canvas =
  print_endline "initializing";
  let () = open_canvas canvas in
  let () =
    Graphics_js.loop [ Graphics_js.Button_down ] (fun s ->
        Graphics_js.draw_rect (s.mouse_x - 5) (s.mouse_y - 5) 10 10)
  in
  ()

let () = Js.export "init" init
