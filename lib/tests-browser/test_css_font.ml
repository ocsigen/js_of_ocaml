open Js_of_ocaml

let pass_count = ref 0

let fail_count = ref 0

let log_row status label detail =
  let el = Dom_html.document##getElementById (Js.string "log") in
  Js.Opt.iter el (fun el ->
      let row = Dom_html.document##createElement (Js.string "div") in
      row##.className := Js.string ("row " ^ status);
      let mark = Dom_html.document##createElement (Js.string "span") in
      mark##.className := Js.string ("mark " ^ status);
      mark##.textContent :=
        Js.some
          (Js.string
             (if String.equal status "pass" then "\xe2\x9c\x93" else "\xe2\x9c\x97"));
      let txt = Dom_html.document##createElement (Js.string "span") in
      txt##.className := Js.string "label";
      txt##.textContent := Js.some (Js.string label);
      let det = Dom_html.document##createElement (Js.string "span") in
      det##.className := Js.string "detail";
      det##.textContent := Js.some (Js.string detail);
      Dom.appendChild row mark;
      Dom.appendChild row txt;
      Dom.appendChild row det;
      Dom.appendChild el row)

let check label cond detail =
  if cond
  then (
    incr pass_count;
    log_row "pass" label detail)
  else (
    incr fail_count;
    log_row "fail" label detail)

let summarize () =
  let el = Dom_html.document##getElementById (Js.string "summary") in
  Js.Opt.iter el (fun el ->
      let status = if !fail_count = 0 then "pass" else "fail" in
      el##.className := Js.string ("summary " ^ status);
      el##.textContent :=
        Js.some
          (Js.string (Printf.sprintf "%d passed, %d failed" !pass_count !fail_count)))

let ( >>= ) p f = Promise.then_ f p

let return = Promise.resolve

let stringify any = Js.to_string (Js.Unsafe.fun_call Js.Unsafe.global##._String [| any |])

let fonts = Dom_html.document##.fonts

(* A single CSS identifier (no spaces) so [family] serializes back unquoted;
   names containing spaces come back quoted, e.g. ["Bebas Neue"]. *)
let family = "BebasNeueTest"

let bebas =
  Css_font.create_font_face (Js.string family) (Js.string "url(BebasNeue-Regular.ttf)")

let test_initial_state () =
  check
    "new FontFace family round-trips"
    (Js.to_string bebas##.family = family)
    (Js.to_string bebas##.family);
  check
    "new FontFace status is unloaded"
    (Js.to_string bebas##.status = "unloaded")
    (Js.to_string bebas##.status);
  return ()

(* Only the descriptors with a browser-stable default are asserted exactly;
   [stretch], [unicodeRange], and [src] serialize in browser-dependent ways. *)
let test_descriptor_defaults () =
  let default name value expected =
    check
      (Printf.sprintf "FontFace.%s default is %S" name expected)
      (value = expected)
      value
  in
  default "style" (Js.to_string bebas##.style) "normal";
  default "weight" (Js.to_string bebas##.weight) "normal";
  default "variant" (Js.to_string bebas##.variant) "normal";
  default "featureSettings" (Js.to_string bebas##.featureSettings) "normal";
  default "display" (Js.to_string bebas##.display) "auto";
  return ()

let test_load_success () =
  fonts##add bebas;
  bebas##load
  >>= fun loaded ->
  check
    "FontFace.load resolves with status loaded"
    (Js.to_string loaded##.status = "loaded")
    (Js.to_string loaded##.status);
  check
    "document.fonts.check is true once the font is loaded"
    (Js.to_bool (fonts##check (Js.string ("48px " ^ family)) (Js.string "ABC")))
    "";
  return ()

let test_set_load () =
  fonts##load (Js.string ("48px " ^ family)) (Js.string "ABC")
  >>= fun faces ->
  check
    "document.fonts.load resolves with the matching face"
    (faces##.length >= 1)
    (Printf.sprintf "length=%d" faces##.length);
  return ()

let test_ready () =
  fonts##.ready
  >>= fun set ->
  let s = Js.to_string set##.status in
  check "document.fonts.ready resolves" (s = "loaded" || s = "loading") s;
  return ()

let test_load_failure () =
  let missing =
    Css_font.create_font_face
      (Js.string "Missing Font")
      (Js.string "url(does-not-exist-xyz.ttf)")
  in
  Promise.catch
    (fun e ->
      check "load of a missing source rejects" true (stringify (Promise.error_to_any e));
      check
        "FontFace status is error after a failed load"
        (Js.to_string missing##.status = "error")
        (Js.to_string missing##.status);
      return ())
    (missing##load
    >>= fun _ ->
    check "load of a missing source rejects" false "unexpectedly resolved";
    return ())

(* No [has]/[size]/iteration is bound on [fontFaceSet], so this only checks
   that [delete] runs without raising. *)
let test_delete () =
  fonts##delete bebas;
  check "document.fonts.delete does not raise" true "";
  return ()

let run () =
  let _ : unit Promise.t =
    test_initial_state ()
    >>= test_descriptor_defaults
    >>= test_load_success
    >>= test_set_load
    >>= test_ready
    >>= test_load_failure
    >>= test_delete
    >>= fun () ->
    summarize ();
    return ()
  in
  ()

let () = run ()
