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
        Console.console##debug event;
        let deltaX = event##.deltaX in
        let deltaY = event##.deltaY in
        let deltaZ = event##.deltaZ in
        let deltaMode = event##.deltaMode in
        let wheelDelta = event##.wheelDelta in
        let wheelDeltaX = event##.wheelDeltaX in
        let wheelDeltaY = event##.wheelDeltaY in
        Printf.printf "deltaX: %f; " (Js.to_float deltaX);
        Printf.printf "deltaY: %f; " (Js.to_float deltaY);
        Printf.printf "deltaZ: %f; " (Js.to_float deltaZ);
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
