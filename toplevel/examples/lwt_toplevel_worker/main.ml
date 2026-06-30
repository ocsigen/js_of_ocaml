(* Js_of_ocaml toplevel
 * http://www.ocsigen.org/js_of_ocaml/
 * Copyright (C) 2011 Jérôme Vouillon
 * Laboratoire PPS - CNRS Université Paris Diderot
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

(* The standard OCaml toplevel page, but with the toplevel running in a Web
   Worker instead of in the UI thread. The page (this file) only edits source,
   ships it to the worker and renders the results, so a long computation never
   freezes the UI; the toplevel itself lives in [worker.js] and is driven
   through {!Js_of_ocaml_toplevel_lwt.Async}.

   Because a Web Worker has no DOM, code typed here cannot touch the page
   (Dom_html, Graphics, ...); pure computations, the stdlib and Lwt all work.
   See the [lwt_toplevel] example for the in-thread version with full DOM and
   graphics access. *)

open Js_of_ocaml
open Js_of_ocaml_lwt
open Toplevel_util
open Lwt
module Async = Js_of_ocaml_toplevel_lwt.Async
(* The host only needs the result type, from the dependency-free msg library —
   not the toplevel runtime. *)
module Wrapped = Js_of_ocaml_toplevel_msg.Wrapped_intf

let compiler_name = "OCaml"

let doc = Dom_html.document

let now () = Js.to_float (new%js Js.date_now)##getTime

(* Offer to abort a phrase once it has been running this long. Aborting is
   manual: a computation stuck in an infinite loop blocks the worker thread, so
   the only way to stop it is to kill the worker (see {!Async.interrupt}). *)
let show_abort_after = 2.0

(* Phrases run once the worker's toplevel is up: open Stdlib, a minimal
   [Lwt_main] usable from the toplevel, the banner, the pretty-printing flags
   and a couple of custom printers. They are run one at a time, in order, so a
   failure in one (e.g. a directive the toplevel does not know) does not
   suppress the others. *)
let setup_phrases =
  [ "open Stdlib"
  ; {|module Lwt_main = struct
  let run t =
    match Lwt.state t with
    | Lwt.Return x -> x
    | Lwt.Fail e -> raise e
    | Lwt.Sleep -> failwith "Lwt_main.run: thread didn't return"
end|}
  ; Printf.sprintf
      {|Format.printf "        %s version %%s@." Sys.ocaml_version|}
      compiler_name
  ; {|Format.printf
  "     Compiled with Js_of_ocaml version %s@.@."
  Jsoo_runtime.Sys.version|}
  ; {|#enable "pretty"|}
  ; {|#disable "shortvar"|}
  ; {|let _print_unit fmt (_ : 'a) : 'a = Format.pp_print_string fmt "()"|}
  ; "#install_printer _print_unit"
  ]

let setup_toplevel ~report top =
  Lwt_list.iter_s
    (fun phrase ->
      Async.use top ~ppf_answer:(fun _ -> ()) phrase
      >>= function
      | Wrapped.Success _ -> Lwt.return_unit
      | Wrapped.Error (err, _) ->
          (* Surface setup failures instead of silently dropping them. *)
          report (err.Wrapped.msg ^ "\n");
          Lwt.return_unit)
    setup_phrases

(* The examples sidebar: fetch the source over HTTP and fill the sidebar. *)
let setup_examples ~container ~textbox =
  (* Cache-bust so an edited examples list is picked up on reload rather than
     served stale from the browser cache. *)
  let url = Printf.sprintf "examples.ml?v=%.0f" (Js.to_float (new%js Js.date_now)##getTime) in
  XmlHttpRequest.get url
  >>= fun frame ->
  let content = if frame.XmlHttpRequest.code = 200 then frame.XmlHttpRequest.content else "" in
  Toplevel_util.setup_examples
    ~container_id:"toplevel-examples"
    ~on_pick:(fun acc ->
      textbox##.value := (Js.string acc)##trim;
      fit ~container ~textbox;
      textbox##focus)
    content;
  Lwt.return_unit

let run () =
  let container = by_id "toplevel-container" in
  let output = by_id "output" in
  let textbox : 'a Js.t = by_id_coerce "userinput" Dom_html.CoerceTo.textarea in
  (* The evaluated program's own output flows through the worker's stdout/stderr
     channels. Route them to the block of the phrase currently running, so each
     phrase's output sits right under its echoed code; before the first phrase
     (e.g. the startup banner) it goes straight to the output. *)
  let cur_out = ref (fun (cls : string) (s : string) -> append Colorize.text output cls s) in
  Async.create
    ~pp_stdout:(fun s -> !cur_out "stdout" s)
    ~pp_stderr:(fun s -> !cur_out "stderr" s)
    ~after_init:(setup_toplevel ~report:(append Colorize.text output "stderr"))
    ~js_file:"worker.js"
    ()
  >>= fun top ->
  (* [Wrapped] returns errors and warnings as values; render them into the
     phrase's [block] and highlight their source locations in the echoed code. *)
  let report block (e : Wrapped.error) =
    append Colorize.text block "stderr" (e.Wrapped.msg ^ "\n");
    (* The echoed lines are the leading [sharp] children of the block. *)
    Js.Opt.iter block##.firstChild (fun first ->
        List.iter (fun loc -> highlight_location ~first loc) e.Wrapped.locs)
  in
  (* The worker runs one phrase at a time, so serialize on the host: each
     phrase waits for the previous one before it is sent. [chain] resolves when
     the last queued phrase is done, and is what the next phrase waits on. *)
  let chain = ref Lwt.return_unit in
  let execute () =
    let content = Js.to_string textbox##.value##trim in
    textbox##.value := Js.string "";
    History.push content;
    (* Each phrase gets its own block, so its echoed code, output and result
       stay grouped together. *)
    let block = Dom_html.createDiv doc in
    block##.className := Js.string "phrase";
    Dom.appendChild output block;
    (* Echo the code immediately — one node per line, so error locations can be
       highlighted — rather than waiting for the worker to echo it. A typed-ahead
       phrase is thus visible at once, even before the worker gets to it. *)
    List.iter
      (fun line -> append Colorize.ocaml block "sharp" line)
      (String.split_on_char '\n' content);
    (* The pending indicator: [queued…] until the worker reaches this phrase,
       then a [running… Xs] timer with an Abort button. *)
    let status = Dom_html.createDiv doc in
    status##.className := Js.string "status";
    let label = Dom_html.createSpan doc in
    label##.className := Js.string "elapsed";
    Dom.appendChild status label;
    Dom.appendChild block status;
    let abort, do_abort = Lwt.wait () in
    let button = Dom_html.createButton doc in
    button##.textContent := Js.some (Js.string "Abort");
    button##.onclick :=
      Dom_html.handler (fun _ ->
          if Lwt.is_sleeping abort then Lwt.wakeup_later do_abort ();
          Js._false);
    let running_since = ref None in
    let button_shown = ref false in
    let tick () =
      match !running_since with
      | None -> label##.textContent := Js.some (Js.string "queued…")
      | Some start ->
          let s = (now () -. start) /. 1000. in
          label##.textContent
          := Js.some (Js.string (Printf.sprintf "running… %.1fs" s));
          if (not !button_shown) && s >= show_abort_after
          then (
            button_shown := true;
            Dom.appendChild status button)
    in
    tick ();
    let timer = Dom_html.window##setInterval (Js.wrap_callback tick) (Js.float 100.) in
    let prev = !chain in
    let this_done, mark_done = Lwt.wait () in
    chain := this_done;
    let finish () =
      Dom_html.window##clearInterval timer;
      Dom.removeChild block status;
      resize ~container ~textbox ()
      >>= fun () ->
      container##.scrollTop := Js.float (float container##.scrollHeight);
      textbox##focus;
      Lwt.return_unit
    in
    (* Wait our turn (or be cancelled while still queued, which just drops us
       from the queue without touching the worker). *)
    Lwt.choose
      [ (prev >>= fun () -> Lwt.return `Ready)
      ; (abort >>= fun () -> Lwt.return `Dropped)
      ]
    >>= function
    | `Dropped ->
        append Colorize.text block "stderr" "Cancelled (was queued).\n";
        (* Keep the chain intact: our successor must still wait for [prev]. *)
        Lwt.async (fun () ->
            prev >>= fun () -> Lwt.wakeup_later mark_done (); Lwt.return_unit);
        finish ()
    | `Ready ->
        running_since := Some (now ());
        tick ();
        (* Route this phrase's program output to its block while it runs. *)
        cur_out := (fun cls s -> append Colorize.text block cls s);
        (* [Async.execute] normalizes the source, so a trailing [;;] is
           optional. The code is already echoed, so no [ppf_code]. *)
        let exec =
          Async.execute
            top
            ~print_outcome:true
            ~ppf_answer:(append Colorize.ocaml block "caml")
            content
        in
        (* Race evaluation against the Abort button. {!Async.interrupt} kills the
           worker, which cancels [exec]; [try_bind] turns that into [`Cancelled]
           (e.g. from a Ctrl+K reset) so it is not mistaken for a normal exit. *)
        Lwt.choose
          [ Lwt.try_bind
              (fun () -> exec)
              (fun result -> Lwt.return (`Done result))
              (fun _ -> Lwt.return `Cancelled)
          ; (abort >>= fun () -> Lwt.return `Aborted)
          ]
        >>= fun outcome ->
        (match outcome with
         | `Done result ->
             let warnings, error =
               match result with
               | Wrapped.Success (_, warnings) -> warnings, None
               | Wrapped.Error (err, warnings) -> warnings, Some err
             in
             List.iter (report block) warnings;
             Option.iter (report block) error;
             Lwt.return_unit
         | `Cancelled ->
             append Colorize.text block "stderr" "Aborted.\n";
             Lwt.return_unit
         | `Aborted ->
             let secs =
               match !running_since with
               | Some s -> (now () -. s) /. 1000.
               | None -> 0.
             in
             append
               Colorize.text
               block
               "stderr"
               (Printf.sprintf
                  "Aborted after %.1fs. Restarting the toplevel; earlier \
                   definitions are lost.\n"
                  secs);
             Async.interrupt top)
        >>= fun () ->
        (* Only now let the next queued phrase run: after an abort, the worker
           has finished restarting. *)
        Lwt.wakeup_later mark_done ();
        finish ()
  in
  (* Ctrl/Cmd+K resets the worker's toplevel (clears all bindings and re-runs
     the setup phrases). *)
  setup_input_handlers
    ~container
    ~textbox
    ~output
    ~execute
    ~reset:(fun () -> Lwt.async (fun () -> Async.reset top ()));
  (Lwt.async_exception_hook :=
     fun exc -> append Colorize.text output "stderr" (Printexc.to_string exc ^ "\n"));
  Lwt.async (fun () ->
      resize ~container ~textbox ()
      >>= fun () ->
      textbox##focus;
      Lwt.return_unit);
  setup_examples ~container ~textbox
  >>= fun () ->
  History.setup ();
  textbox##.value := Js.string "";
  Lwt.return_unit

let () =
  Dom_html.window##.onload :=
    Dom_html.handler (fun _ ->
        Lwt.async run;
        Js._false)
