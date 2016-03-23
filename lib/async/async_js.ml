open Core_kernel.Std

module Time_ns = Core_kernel.Time_ns
module Clock_ns = Async_kernel.Clock_ns
module Scheduler = Async_kernel.Scheduler

let sleep d = Clock_ns.after (Time_ns.Span.of_sec d)

let yield () = Scheduler.yield (Scheduler.t ())

let run =
  let module State = struct type t = Idle | Will_run_soon | Running end in
  let state = ref State.Idle in
  let rec loop () =
    let t = Scheduler.t () in
    match !state, Scheduler.uncaught_exn t with
    | _, Some _ | State.Running, None -> ()
    | (State.Idle | State.Will_run_soon), None ->
      state := State.Running;
      Scheduler.run_cycle t;
      let next_wakeup, next_state =
        if Scheduler.can_run_a_job t
        then (Some 0., State.Will_run_soon)
        else
          match Scheduler.next_upcoming_event t with
          | None -> (None, State.Idle)
          | Some next ->
            (* Negative spans are apparently treated like 0. by [Dom_html.setTimeout], so
               no need to handle them specially. *)
            let now = Time_ns.now () in
            let d   = Time_ns.diff next now in
            let d   = Time_ns.Span.to_ms d in
            (Some d, if d <= 0. then State.Will_run_soon else State.Idle)
      in
      Option.iter (Scheduler.uncaught_exn_unwrapped t) ~f:(fun (exn,_sexp) ->
        match Async_kernel.Monitor.extract_exn exn with
        | Js.Error err -> Js.raise_js_error err
        | exn -> raise exn);
      Option.iter next_wakeup ~f:run_after;
      state := next_state
  and run_after span =
    ignore (Dom_html.setTimeout loop span : Dom_html.timeout_id_safe)
  in
  fun () ->
    match !state with
    | State.Idle -> run_after 0.; state := State.Will_run_soon
    | State.Running | State.Will_run_soon -> ()

let initialization = lazy (
  let t = Scheduler.t () in
  Scheduler.set_job_queued_hook t (fun _ -> run ());
  Scheduler.set_event_added_hook t (fun _ -> run ());
  Scheduler.set_thread_safe_external_job_hook t run;
  Async_kernel.Monitor0.try_with_log_exn := (fun exn ->
    Firebug.console##error_2 (
      Js.string "Async_kernel.Monitor0.try_with_log_exn",
      exn));
  run ()
)

let init () = force initialization
