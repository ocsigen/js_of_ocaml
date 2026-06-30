(* Js_of_ocaml toplevel
 * http://www.ocsigen.org/js_of_ocaml/
 * Copyright (C) 2016 OCamlPro
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU Lesser General Public License as published by
 * the Free Software Foundation, with linking exception;
 * either version 2.1 of the License, or (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
 *)

(* The same interactive notebook as the [notebook] example, but the toplevel
   runs in a separate Web Worker instead of in the UI thread.

   Parsing, type-checking, byte-compilation and evaluation all happen in
   [worker.js], driven through {!Js_of_ocaml_toplevel_worker_lwt_client}; the page only
   sends source over and renders the results, so a long computation never
   freezes the UI. Evaluation is therefore asynchronous: each cell's [run]
   returns an Lwt thread. See the [notebook] example for the synchronous,
   in-process variant. *)

open Js_of_ocaml
module Async = Js_of_ocaml_toplevel_worker_lwt_client

(* The host only needs the result type, from the dependency-free protocol library —
   not the toplevel runtime. *)
module Wrapped = Js_of_ocaml_toplevel_protocol.Wrapped_intf

let ( >>= ) = Lwt.bind

let doc = Dom_html.document

(* Offer to cancel a cell once it has been running this long. Interrupting is
   manual: a computation stuck in an infinite loop blocks the worker thread, so
   the only way to stop it is to kill the worker (see {!Async.interrupt}), and
   only the user knows whether a slow cell is worth waiting for. *)
let show_cancel_after = 2.0

(* Append [s] to [out] inside a [<span class=cls>] (ignoring empty chunks). *)
let append out cls s =
  if s <> ""
  then (
    let span = Dom_html.createSpan doc in
    span##.className := Js.string cls;
    Dom.appendChild span (doc##createTextNode (Js.string s));
    Dom.appendChild out span)

(* Render a [Wrapped] result's warnings and error into [out]. [Wrapped] returns
   them as values rather than printing them, so the consumer renders them. *)
let render out (result : _ Wrapped.result) =
  let report (e : Wrapped.error) = append out "err" (e.Wrapped.msg ^ "\n") in
  let warnings, error =
    match result with
    | Wrapped.Success (_, w) -> w, None
    | Wrapped.Error (e, w) -> w, Some e
  in
  List.iter report warnings;
  Option.iter report error

(* The evaluated program's own output flows through the worker's [stdout]/
   [stderr] channels, which {!Async.create} routes globally; switch them to the
   running cell here before each evaluation. Cells are evaluated one at a time,
   so a single mutable sink is enough. Compiler diagnostics do not go through
   the channels — they come back on the [result] and are shown by [render]. *)
let cur_out = ref (fun (_cls : string) (_s : string) -> ())

(* The worker evaluates one cell at a time, and its stdout/stderr are routed
   through the single [cur_out] above. Serialize cell evaluation on the host so
   [cur_out] is only ever pointed at the cell that is actually running. *)
let chain = ref Lwt.return_unit

let notebook = Dom_html.getElementById "notebook"

let rec add_cell top ?(value = "") () =
  let cell = Dom_html.createDiv doc in
  cell##.className := Js.string "cell";
  let input = Dom_html.createTextarea doc in
  input##.className := Js.string "input";
  input##.value := Js.string value;
  input##.rows := 2;
  let output = Dom_html.createDiv doc in
  output##.className := Js.string "output";
  (* Shows the elapsed time while a cell is pending, and a Cancel button once it
     has been running for a while. *)
  let status = Dom_html.createDiv doc in
  status##.className := Js.string "status";
  Dom.appendChild cell input;
  Dom.appendChild cell output;
  Dom.appendChild cell status;
  Dom.appendChild notebook cell;
  (* Move on like a notebook: focus the next cell, opening a fresh one below if
     this was the last. *)
  let advance () =
    match Js.Opt.to_option cell##.nextSibling with
    | None -> ignore (add_cell top () : unit -> unit Lwt.t)
    | Some next ->
        Js.Opt.iter
          (Js.Opt.bind next##.firstChild Dom_html.CoerceTo.element)
          (fun el -> el##focus)
  in
  let now () = Js.to_float (new%js Js.date_now)##getTime in
  (* Guard against a second Shift+Enter while this cell is already queued or
     running, which would evaluate it twice. *)
  let pending = ref false in
  let run () =
    if !pending
    then Lwt.return_unit
    else (
      pending := true;
      (* Wait for the previously evaluated cell to finish before claiming the
         worker (and [cur_out]). *)
      let prev = !chain in
      let this_done, mark_done = Lwt.wait () in
      chain := this_done;
      prev
      >>= fun () ->
      output##.innerHTML := Js.string "";
      status##.innerHTML := Js.string "";
      cur_out := append output;
      let start = now () in
      let elapsed () = (now () -. start) /. 1000. in
      (* The work happens off the UI thread, so show a live "running… Xs" timer
         while it is pending. *)
      let label = Dom_html.createSpan doc in
      label##.className := Js.string "elapsed";
      Dom.appendChild status label;
      (* A user-driven interrupt: a hung cell can only be stopped by killing the
         worker, and only the user knows whether to. Resolve [cancel] when the
         button (shown after [show_cancel_after]) is clicked. *)
      let cancel, wake_cancel = Lwt.wait () in
      let button = Dom_html.createButton doc in
      button##.textContent := Js.some (Js.string "Cancel");
      button##.onclick :=
        Dom_html.handler (fun _ ->
            if Lwt.is_sleeping cancel then Lwt.wakeup_later wake_cancel ();
            Js._false);
      let button_shown = ref false in
      let tick () =
        label##.textContent :=
          Js.some (Js.string (Printf.sprintf "running… %.1fs" (elapsed ())));
        if (not !button_shown) && elapsed () >= show_cancel_after
        then (
          button_shown := true;
          Dom.appendChild status button)
      in
      tick ();
      let timer = Dom_html.window##setInterval (Js.wrap_callback tick) (Js.float 100.) in
      let exec =
        Async.execute
          top
          ~print_outcome:true
          ~ppf_answer:(append output "ans")
          (Js.to_string input##.value)
      in
      (* [Lwt.finalize] guarantees the timer is cleared, the chain is released
         (so the next cell can run) and the re-entrancy guard is lifted, even if
         rendering or the restart raises. *)
      Lwt.finalize
        (fun () ->
          (* Race the evaluation against the Cancel button. {!Async.interrupt}
             kills the worker, which cancels [exec]; [try_bind] turns that into
             [`Cancelled] so it is handled rather than left dangling. *)
          Lwt.choose
            [ Lwt.try_bind
                (fun () -> exec)
                (fun result -> Lwt.return (`Done result))
                (fun _ -> Lwt.return `Cancelled)
            ; (cancel >>= fun () -> Lwt.return `Aborted)
            ]
          >>= fun outcome ->
          match outcome with
          | `Done result ->
              render output result;
              advance ();
              Lwt.return_unit
          | `Cancelled ->
              append output "err" "Aborted.\n";
              Lwt.return_unit
          | `Aborted ->
              append
                output
                "err"
                (Printf.sprintf
                   "Interrupted after %.1fs. Restarting the worker; earlier definitions \
                    are lost.\n"
                   (elapsed ()));
              Async.interrupt top
              >>= fun () ->
              advance ();
              Lwt.return_unit)
        (fun () ->
          Dom_html.window##clearInterval timer;
          status##.innerHTML := Js.string "";
          pending := false;
          Lwt.wakeup_later mark_done ();
          Lwt.return_unit))
  in
  input##.onkeydown :=
    Dom_html.handler (fun e ->
        (* Shift+Enter evaluates; plain Enter keeps inserting newlines. *)
        if e##.keyCode = 13 && Js.to_bool e##.shiftKey
        then (
          Lwt.async run;
          Js._false)
        else Js._true);
  (* Clicking anywhere in the cell focuses its input. *)
  cell##.onclick :=
    Dom_html.handler (fun _ ->
        input##focus;
        Js._true);
  input##focus;
  run

let () =
  let onload _ =
    let run () =
      (* [stdlib.cmis.js] is fetched by the worker (see [Async.create]'s
         [cmis_base_url]) so the toplevel can type-check against the stdlib. *)
      Async.create
        ~pp_stdout:(fun s -> !cur_out "stdout" s)
        ~pp_stderr:(fun s -> !cur_out "stderr" s)
        ~js_file:"worker.js"
        ()
      >>= fun top ->
      (* A few cells, pre-filled and evaluated, to show off values, stdout,
         warnings and errors; a fresh empty cell is appended once the last one
         runs. They are evaluated one at a time so their output does not
         interleave. The last one loops forever, so its Cancel button appears
         and the worker can be interrupted by hand. *)
      let samples =
        [ "let rec fib n = if n < 2 then n else fib (n - 1) + fib (n - 2)\n\
           let () = Printf.printf \"fib 10 = %d\\n\" (fib 10)"
        ; "List.map (fun n -> n * n) [ 1; 2; 3; 4; 5 ]"
        ; "print_endline \"a side effect on stdout\""
        ; "let head = function x :: _ -> x  (* non-exhaustive match: a warning *)"
        ; "let type_error = 1 + \"oops\""
        ; "let () = while true do () done  (* hangs: click Cancel to interrupt *)"
        ]
      in
      Lwt_list.iter_s
        (fun run -> run ())
        (List.map (fun value -> add_cell top ~value ()) samples)
    in
    Lwt.async run;
    Js._false
  in
  Dom_html.window##.onload := Dom_html.handler onload
