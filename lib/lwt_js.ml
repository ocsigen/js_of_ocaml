
(*FIX: cancel? *)
let sleep d =
  let (t, w) = Lwt.task () in
  ignore
    (Dom.HTML.window##setTimeout ((fun () -> Lwt.wakeup w ()), d *. 1000.));
  t

let yield () = sleep 0.
