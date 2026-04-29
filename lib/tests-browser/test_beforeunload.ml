open Js_of_ocaml

let log msg =
  let el = Dom_html.document##getElementById (Js.string "log") in
  Js.Opt.iter el (fun el ->
      let line = Dom_html.document##createElement (Js.string "div") in
      line##.textContent := Js.some (Js.string msg);
      Dom.appendChild el line)

let clear_log () =
  let el = Dom_html.document##getElementById (Js.string "log") in
  Js.Opt.iter el (fun el -> el##.innerHTML := Js.string "")

(* Strategy 1: Dom.handler returning Js._false (block navigation)
   - handler calls preventDefault and sets returnValue for beforeunload
   - Returns false to JS (non-undefined) — triggers dialog *)
let set_strategy_1 () =
  clear_log ();
  Dom_html.window##.onbeforeunload
  := Dom_html.handler (fun (_e : Dom_html.event Js.t) ->
      log "handler called, returning Js._false (block)";
      Js._false);
  log "Strategy 1: handler returning Js._false (block)";
  log "Expected: dialog on all browsers";
  log "Now try to navigate away (click a link or close the tab)"

(* Strategy 2: Dom.handler returning Js._true (allow navigation)
   - handler detects beforeunload and returns undefined instead of true
   - Should allow navigation on all browsers *)
let set_strategy_2 () =
  clear_log ();
  Dom_html.window##.onbeforeunload
  := Dom_html.handler (fun (_e : Dom_html.event Js.t) ->
      log "handler called, returning Js._true (allow)";
      Js._true);
  log "Strategy 2: handler returning Js._true (allow)";
  log "Expected: NO dialog on any browser";
  log "Now try to navigate away"

(* Strategy 3: Dom.handler with conditional logic
   - Demonstrates runtime decision within a single handler *)
let should_block = ref true

let set_strategy_3 () =
  clear_log ();
  Dom_html.window##.onbeforeunload
  := Dom_html.handler (fun (_e : Dom_html.event Js.t) ->
      log (Printf.sprintf "handler called, should_block=%b" !should_block);
      Js.bool (not !should_block));
  log "Strategy 3: handler with conditional logic";
  log "Currently: BLOCKING (click toggle button to change)";
  log "Now try to navigate away"

let toggle_block () =
  should_block := not !should_block;
  log
    (Printf.sprintf
       "Toggled: should_block=%b — %s"
       !should_block
       (if !should_block then "will BLOCK" else "will ALLOW"))

(* Strategy 4: Dom.no_handler (null)
   - Sets onbeforeunload to null — no function runs
   - Should allow navigation on all browsers *)
let set_strategy_4 () =
  clear_log ();
  Dom_html.window##.onbeforeunload := Dom.no_handler;
  log "Strategy 4: no_handler (null)";
  log "Expected: NO dialog on any browser";
  log "Now try to navigate away"

let button label f =
  let btn = Dom_html.createButton ~_type:(Js.string "button") Dom_html.document in
  btn##.textContent := Js.some (Js.string label);
  btn##.onclick :=
    Dom_html.handler (fun _e ->
        f ();
        Js._true);
  btn

let () =
  let doc = Dom_html.document in
  let container =
    Js.Opt.get (doc##getElementById (Js.string "controls")) (fun _ -> assert false)
  in
  let buttons =
    [ "1: block navigation", set_strategy_1
    ; "2: allow navigation", set_strategy_2
    ; "3: conditional", set_strategy_3
    ; "toggle block/allow", toggle_block
    ; "4: clear handler", set_strategy_4
    ]
  in
  List.iter
    (fun (label, f) ->
      let b = button label f in
      Dom.appendChild container b)
    buttons;
  log "Ready. Pick a strategy, then try to navigate away.";
  log "You can navigate by clicking the link below, closing the tab, or refreshing."
