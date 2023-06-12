open Js_of_ocaml

let optdef_to_string o =
  match Js.Optdef.to_option o with
  | Some v -> Int.to_string v
  | None -> "undefined"

let () =
  let html =
    Js.Opt.get
      (Dom_html.document##querySelector (Js.string "html"))
      (fun _ -> assert false)
  in
  html##.onwheel :=
    Dom.handler (fun (event : Dom_html.mousewheelEvent Js.t) ->
        Firebug.console##debug event;
        let deltaX = Js.to_float event##.deltaX in
        let deltaY = Js.to_float event##.deltaY in
        let deltaZ = Js.to_float event##.deltaZ in
        let deltaMode = event##.deltaMode in
        let wheelDelta = event##.wheelDelta in
        let wheelDeltaX = event##.wheelDeltaX in
        let wheelDeltaY = event##.wheelDeltaY in
        Printf.printf "deltaX: %f; " deltaX;
        Printf.printf "deltaY: %f; " deltaY;
        Printf.printf "deltaZ: %f; " deltaZ;
        Printf.printf
          "deltaMode: %s; "
          (match deltaMode with
          | Delta_pixel -> "Delta_pixel"
          | Delta_line -> "Delta_line"
          | Delta_page -> "Delta_page");
        Printf.printf "wheelDelta: %d; " wheelDelta;
        Printf.printf "wheelDeltaX: %s; " (optdef_to_string wheelDeltaX);
        Printf.printf "wheelDeltaY: %s\n" (optdef_to_string wheelDeltaY);
        Js._false)
