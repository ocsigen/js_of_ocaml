open Js_of_ocaml

let log msg =
  let el = Dom_html.document##getElementById (Js.string "log") in
  Js.Opt.iter el (fun el ->
      let line = Dom_html.document##createElement (Js.string "div") in
      line##.textContent := Js.some (Js.string msg);
      Dom.appendChild el line)

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
        let deltaX = Js.to_float event##.deltaX in
        let deltaY = Js.to_float event##.deltaY in
        let deltaZ = Js.to_float event##.deltaZ in
        let deltaMode =
          match event##.deltaMode with
          | Delta_pixel -> "Delta_pixel"
          | Delta_line -> "Delta_line"
          | Delta_page -> "Delta_page"
        in
        let wheelDelta = event##.wheelDelta in
        let wheelDeltaX = optdef_to_string event##.wheelDeltaX in
        let wheelDeltaY = optdef_to_string event##.wheelDeltaY in
        log
          (Printf.sprintf
             "deltaX: %f; deltaY: %f; deltaZ: %f; deltaMode: %s; wheelDelta: %d; \
              wheelDeltaX: %s; wheelDeltaY: %s"
             deltaX
             deltaY
             deltaZ
             deltaMode
             wheelDelta
             wheelDeltaX
             wheelDeltaY);
        Js._false)
