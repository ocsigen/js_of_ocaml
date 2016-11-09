(** Overview *)
let x = 10+10
let y = x * 3
let c = String.make x 'a'
let sin1 = sin 1.
let rec fact n = if n = 0 then 1. else float n *. fact (n - 1)
let _ = Printf.printf "fact 20 = %f\n" (fact 20)
let _ = "abc" < "def"

(** Mutually recursive function *)
let rec even n =
  match n with
  | 0 -> true
  | x -> odd (x-1)
and odd n =
  match n with
  | 0 -> false
  | x -> even (x-1)

(** Mutually recursive module *)
module rec Odd : sig
  val odd : int -> bool
end = struct
  let odd x = if x = 0 then false else Even.even (pred x)
end and Even : sig
  val even : int -> bool
end = struct
  let even x = if x = 0 then true else Odd.odd (pred x)
end

(** Reactive dom *)
let display x =
  Dom.appendChild
    (Dom_html.getElementById "output")
    (Tyxml_js.To_dom.of_element x)
module RList = ReactiveData.RList
let rl,rhandle = RList.create []
let li_rl = RList.map (fun x -> Tyxml_js.Html.(li [pcdata x])) rl

let ul_elt = Tyxml_js.R.Html.ul li_rl
let init =
  let _ = RList.snoc "# cons \"some string\"" rhandle in
  let _ = RList.snoc "# snoc \"some other\"" rhandle in
  let _ = RList.snoc "# insert \"anywhere\" 1"  rhandle in
  let _ = RList.snoc "# remove 1" rhandle in
  ()
let snoc s = RList.snoc s rhandle
let cons s = RList.cons s rhandle
let insert s pos = RList.insert s pos rhandle
let remove pos = RList.remove pos rhandle
let time_signal =
  let s,set = React.S.create (Sys.time ()) in
  let rec loop () =
    set (Sys.time ());
    Lwt.bind (Lwt_js.sleep 1.) loop in
  Lwt.async loop;
  s
let div_elt =
  Tyxml_js.(Html.(
    div [
      h4 [pcdata "Uptime is ";
	  R.Html.pcdata (React.S.map (fun s -> string_of_int (int_of_float s)) time_signal);
	  pcdata " s"];
      ul_elt
    ]))
let _ = display div_elt

(** Graphics: Draw *)
open Graphics_js
loop [Mouse_motion] (function {mouse_x=x;mouse_y=y} -> fill_circle x y 5);;

(** Graphics: Draw chars*)
open Graphics_js
loop [Mouse_motion;Key_pressed]
  (function {mouse_x=x;mouse_y=y;key} ->
    moveto x y; draw_char key);;

(** Graphics: PingPong *)
open Graphics_js
let c = 3
let x0 = 0
and x1 = size_x ()
and y0 = 0
and y1 = size_y ()

let draw_ball x y =
 set_color foreground;
 fill_circle x y c


let state = ref (Lwt.task ())
let wait () = fst(!state)

let rec pong_aux x y dx dy =
 draw_ball x y;
 let new_x = x + dx
 and new_y = y + dy in
 let new_dx =
  if new_x - c <= x0 || new_x + c >= x1 then (- dx) else dx
 and new_dy =
  if new_y - c <= y0 || new_y + c >= y1 then (- dy) else dy in
 Lwt.bind (wait ()) (fun () ->
     pong_aux new_x new_y new_dx new_dy)

let rec start () =
  let t = Lwt.task () in
  let _,w = !state in
  state := t;
  clear_graph();
  Lwt.wakeup w ();
  Lwt.bind (Lwt_js.sleep (1./.60.)) start

let pong x y dx dy = pong_aux x y dx dy

let _ = pong 111 87 2 3
let _ = pong 28  57 5 3
let _ = start ()


(** Async_kernel *)
open Core_kernel.Std
open Async_kernel.Std
let rec countdown span =
  if Time_ns.Span.(span < zero)
  then begin
      print_endline "done";
      Deferred.unit
    end
  else begin
      Printf.printf "%fsec" (Time_ns.Span.to_sec span);
      let delay = Time_ns.Span.of_sec 1. in
      Clock_ns.after delay >>= fun () -> countdown Time_ns.Span.(span - delay)
    end;;
