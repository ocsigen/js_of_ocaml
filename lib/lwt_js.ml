
external set_timeout : (unit -> unit) -> float -> unit = "setTimeout"

let sleep d =
  let (t, w) = Lwt.task () in
  set_timeout (fun () -> Lwt.wakeup w ()) (d *. 1000.);
  t

let yield () = sleep 0.
