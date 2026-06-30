open Js_of_ocaml
open Js_of_ocaml_toplevel

let () = initialize ()

let doc = Dom_html.document

(* Append [s] to [out] inside a [<span class=cls>] (ignoring empty chunks). *)
let append out cls s =
  if s <> ""
  then (
    let span = Dom_html.createSpan doc in
    span##.className := Js.string cls;
    Dom.appendChild span (doc##createTextNode (Js.string s));
    Dom.appendChild out span)

(* A formatter that appends what it receives to [out] with class [cls]. *)
let formatter out cls =
  let b = Buffer.create 128 in
  Format.make_formatter (Buffer.add_substring b) (fun () ->
      append out cls (Buffer.contents b);
      Buffer.clear b)

(* Render a [Wrapped] result's warnings and error into [out]. [Wrapped] returns
   them as values rather than printing them, so the consumer renders them. *)
let render out (result : _ Wrapped.result) =
  let ppf = formatter out "err" in
  let report (e : Wrapped.error) = Format.fprintf ppf "%s@." e.Wrapped.msg in
  let warnings, error =
    match result with
    | Wrapped.Success (_, w) -> w, None
    | Wrapped.Error (e, w) -> w, Some e
  in
  List.iter report warnings;
  Option.iter report error

let notebook = Dom_html.getElementById "notebook"

let rec add_cell ?(value = "") () =
  let cell = Dom_html.createDiv doc in
  cell##.className := Js.string "cell";
  let input = Dom_html.createTextarea doc in
  input##.className := Js.string "input";
  input##.value := Js.string value;
  input##.rows := 2;
  let output = Dom_html.createDiv doc in
  output##.className := Js.string "output";
  Dom.appendChild cell input;
  Dom.appendChild cell output;
  Dom.appendChild notebook cell;
  let run () =
    output##.innerHTML := Js.string "";
    (* The evaluated program's own output flows through the [stdout]/[stderr]
       channels; route them to this cell. Compiler diagnostics do not — they
       come back on the [result] and are shown by [render]. *)
    Sys_js.set_channel_flusher stdout (append output "stdout");
    Sys_js.set_channel_flusher stderr (append output "stderr");
    render
      output
      (Wrapped.execute
         ()
         ~print_outcome:true
         ~ppf_answer:(formatter output "ans")
         (Js.to_string input##.value));
    (* Move on like a notebook: focus the next cell, opening a fresh one below
       if this was the last. *)
    match Js.Opt.to_option cell##.nextSibling with
    | None -> ignore (add_cell () : unit -> unit)
    | Some next ->
        Js.Opt.iter
          (Js.Opt.bind next##.firstChild Dom_html.CoerceTo.element)
          (fun el -> el##focus)
  in
  input##.onkeydown :=
    Dom_html.handler (fun e ->
        (* Shift+Enter evaluates; plain Enter keeps inserting newlines. *)
        if e##.keyCode = 13 && Js.to_bool e##.shiftKey
        then (
          run ();
          Js._false)
        else Js._true);
  (* Clicking anywhere in the cell focuses its input. *)
  cell##.onclick := Dom_html.handler (fun _ -> input##focus; Js._true);
  input##focus;
  run

(* A few cells, pre-filled and evaluated, to show off values, stdout, warnings
   and errors; a fresh empty cell is appended once the last one runs. *)
let () =
  let samples =
    [ "let rec fib n = if n < 2 then n else fib (n - 1) + fib (n - 2)\n\
       let () = Printf.printf \"fib 10 = %d\\n\" (fib 10)"
    ; "List.map (fun n -> n * n) [ 1; 2; 3; 4; 5 ]"
    ; "print_endline \"a side effect on stdout\""
    ; "let head = function x :: _ -> x  (* non-exhaustive match: a warning *)"
    ; "let type_error = 1 + \"oops\""
    ]
  in
  List.iter (fun run -> run ()) (List.map (fun value -> add_cell ~value ()) samples)
