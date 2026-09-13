open Js_of_ocaml

let pass_count = ref 0

let fail_count = ref 0

let document = Dom_html.document

let by_id id = document##getElementById (Js.string id)

let text s = (document##createTextNode (Js.string s) :> Dom.node Js.t)

let log_row status label detail =
  Js.Opt.iter (by_id "log") (fun el ->
      let row = document##createElement (Js.string "div") in
      row##.className := Js.string ("row " ^ status);
      let mark = document##createElement (Js.string "span") in
      mark##.className := Js.string ("mark " ^ status);
      mark##.textContent :=
        Js.some
          (Js.string
             (if String.equal status "pass" then "\xe2\x9c\x93" else "\xe2\x9c\x97"));
      let txt = document##createElement (Js.string "span") in
      txt##.className := Js.string "label";
      txt##.textContent := Js.some (Js.string label);
      let det = document##createElement (Js.string "span") in
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
  Js.Opt.iter (by_id "summary") (fun el ->
      let status = if !fail_count = 0 then "pass" else "fail" in
      el##.className := Js.string ("summary " ^ status);
      el##.textContent :=
        Js.some
          (Js.string (Printf.sprintf "%d passed, %d failed" !pass_count !fail_count)))

(* Sample values: the fields are only read through the formatter *)
[@@@warning "-69"]

type point =
  { x : int
  ; name : string
  }

type shape =
  | Circle of point * int
  | Poly of point list
  | Empty

type color =
  | Red
  | Green
  | Blue

type node =
  { v : int
  ; mutable next : node option
  }

exception Custom of string * int

let samples : (string * Js.Unsafe.any) list =
  let cyclic =
    let n = { v = 1; next = None } in
    n.next <- Some n;
    n
  in
  List.map
    (fun (label, v) -> label, Js.Unsafe.inject v)
    [ "record", Obj.repr { x = 1; name = "p" }
    ; "constructor", Obj.repr (Circle ({ x = 0; name = "o" }, 1))
    ; "list of records", Obj.repr (Poly [ { x = 1; name = "a" }; { x = 2; name = "b" } ])
    ; "constants", Obj.repr (Some Green, [ Red; Blue ], Empty)
    ; "immediates and strings", Obj.repr ("abc", 'c', 3.14, -7, 1e100)
    ; ( "options and results"
      , Obj.repr (List.init 3 (fun i -> Some i), (Ok 1 : (int, string) result)) )
    ; "array", Obj.repr [| 1; 2; 3 |]
    ; "float array", Obj.repr [| 1.5; 2.5 |]
    ; "polymorphic variants", Obj.repr (`Foo 3, `Bar, `Baz (1, "x"))
    ; "reference", Obj.repr (ref [ 1; 2 ])
    ; "closure", Obj.repr (fun x -> x + 1)
    ; "exception", Obj.repr (Custom ("boom", 42))
    ; ( "lazy (forced)"
      , Obj.repr
          (let l = lazy (1 + 2) in
           ignore (Lazy.force l);
           l) )
    ; "cyclic", Obj.repr cyclic
    ; "bytes", Obj.repr (Bytes.of_string "abc")
    ; "boxed integers", Obj.repr (42L, 7l, 9n)
    ; ( "bigarray"
      , Obj.repr
          (Bigarray.Array2.of_array Bigarray.float64 Bigarray.c_layout [| [| 1.; 2. |] |])
      )
    ; "floats", Obj.repr (1.0, 3.14, -0.5, nan, infinity, [ 0.25 ])
    ; ( "JavaScript values"
      , Obj.repr
          ( Dom_html.document
          , Js.Unsafe.global##._Promise##resolve 1
          , Js.Unsafe.obj [| "a", Js.Unsafe.inject 1 |]
          , Js.null
          , Js._true ) )
    ; "long list", Obj.repr (List.init 500 Fun.id)
    ; "deep nesting", Obj.repr (List.init 8 (fun i -> i, Some (i, [ i ])))
    ]

(* The formatter installed by [Devtools.register_formatters], if any *)

class type formatter = object
  method header : Js.Unsafe.any -> Js.Unsafe.any -> Js.Unsafe.any Js.opt Js.meth

  method hasBody : Js.Unsafe.any -> Js.Unsafe.any -> bool Js.t Js.meth

  method body : Js.Unsafe.any -> Js.Unsafe.any -> Js.Unsafe.any Js.meth
end

let formatter () : formatter Js.t option =
  let formatters : formatter Js.t Js.js_array Js.t Js.optdef =
    Js.Unsafe.global##.devtoolsFormatters
  in
  match Js.Optdef.to_option formatters with
  | None -> None
  | Some formatters -> Js.Optdef.to_option (Js.array_get formatters 0)

let undefined = Js.Unsafe.inject Js.undefined

(* Render the JsonML produced by the formatter as DOM, expanding "object" nodes
   through the formatter lazily (on demand, like the console does) *)

let rec to_dom f (node : Js.Unsafe.any) : Dom.node Js.t =
  if String.equal (Js.to_string (Js.typeof node)) "string"
  then (document##createTextNode (Js.Unsafe.coerce node) :> Dom.node Js.t)
  else
    let arr : Js.Unsafe.any Js.js_array Js.t = Js.Unsafe.coerce node in
    let get i = Js.Optdef.get (Js.array_get arr i) (fun () -> assert false) in
    let attrs = get 1 in
    match Js.to_string (Js.Unsafe.coerce (get 0)) with
    | "object" ->
        object_node
          f
          (Js.Unsafe.get attrs (Js.string "object"))
          (Js.Unsafe.get attrs (Js.string "config"))
    | tag ->
        let el = document##createElement (Js.string tag) in
        let style : Js.js_string Js.t Js.optdef =
          Js.Unsafe.get attrs (Js.string "style")
        in
        Js.Optdef.iter style (fun style -> el##setAttribute (Js.string "style") style);
        for i = 2 to arr##.length - 1 do
          Dom.appendChild el (to_dom f (get i))
        done;
        (el :> Dom.node Js.t)

and object_node f obj config : Dom.node Js.t =
  match Js.Opt.to_option (f##header obj config) with
  | None ->
      let el = document##createElement (Js.string "span") in
      el##.className := Js.string "native";
      Dom.appendChild el (text "<not formatted>");
      (el :> Dom.node Js.t)
  | Some header ->
      if Js.to_bool (f##hasBody obj config)
      then (
        let details = document##createElement (Js.string "details") in
        let summary = document##createElement (Js.string "summary") in
        Dom.appendChild summary (to_dom f header);
        Dom.appendChild details summary;
        let expanded = ref false in
        ignore
          (Dom_html.addEventListener
             details
             (Dom_html.Event.make "toggle")
             (Dom_html.handler (fun _ ->
                  if not !expanded
                  then (
                    expanded := true;
                    Dom.appendChild details (to_dom f (f##body obj config)));
                  Js._true))
             Js._false);
        (details :> Dom.node Js.t))
      else to_dom f header

let render_samples f =
  Js.Opt.iter (by_id "rendering") (fun el ->
      List.iter
        (fun (label, v) ->
          let row = document##createElement (Js.string "div") in
          row##.className := Js.string "sample";
          let lbl = document##createElement (Js.string "span") in
          lbl##.className := Js.string "sample-label";
          Dom.appendChild lbl (text (label ^ ": "));
          Dom.appendChild row lbl;
          Dom.appendChild row (object_node f v undefined);
          Dom.appendChild el row)
        samples)

let log_samples () =
  List.iter (fun (label, v) -> Console.console##log_2 (Js.string label) v) samples;
  Console.console##log (Js.string "(values logged; expand them above)")

let header_text f v =
  match Js.Opt.to_option (f##header v undefined) with
  | None -> None
  | Some h ->
      let div = document##createElement (Js.string "div") in
      Dom.appendChild div (to_dom f h);
      Some (Js.Opt.case div##.textContent (fun () -> "") Js.to_string)

let run () =
  Devtools.register_formatters ();
  let js =
    match Sys.backend_type with
    | Other "js_of_ocaml" -> true
    | _ -> false
  in
  (match formatter () with
  | None ->
      check
        "formatter installed"
        (not js)
        (if js
         then "not installed: compiler built without Introcaml?"
         else "not installed (no-op outside js_of_ocaml)")
  | Some f ->
      check "formatter installed" true "devtoolsFormatters[0]";
      let record = Js.Unsafe.inject { x = 1; name = "p" } in
      let header = header_text f record in
      check
        "record header in OCaml syntax"
        (match header with
        | Some "{x = 1; name = \"p\"}" -> true
        | _ -> false)
        (Option.value header ~default:"<declined>");
      check "record has a body" (Js.to_bool (f##hasBody record undefined)) "";
      check
        "plain JavaScript object declined"
        (not (Js.Opt.test (f##header (Js.Unsafe.obj [||]) undefined)))
        "";
      check
        "JavaScript array made from an OCaml array declined"
        (not (Js.Opt.test (f##header (Js.Unsafe.inject (Js.array [| 1; 2 |])) undefined)))
        "";
      render_samples f;
      log_samples ());
  summarize ()

let () =
  Js.Opt.iter (by_id "log-button") (fun b ->
      b##.onclick :=
        Dom_html.handler (fun _ ->
            log_samples ();
            Js._false));
  Dom_html.window##.onload :=
    Dom_html.handler (fun _ ->
        run ();
        Js._false)
