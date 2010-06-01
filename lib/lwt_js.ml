
(*FIX: cancel? *)
let sleep d =
  let (t, w) = Lwt.task () in
  ignore
    (Dom_html.window##setTimeout ((fun () -> Lwt.wakeup w ()), d *. 1000.));
  t

let yield () = sleep 0.
