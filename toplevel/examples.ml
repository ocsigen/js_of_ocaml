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


(** Graphics: PingPong *)
open Graphics;;
let c = 5;;
let x0 = c
and x1 = size_x () - c
and y0 = c
and y1 = size_y () - c;;

let draw_ball x y =
 set_color foreground;
 fill_circle x y c;;


let state = ref (Lwt.task ())
let wait () = fst(!state)

let rec pong_aux x y dx dy =
 draw_ball x y;
 let new_x = x + dx
 and new_y = y + dy in
 let new_dx =
  if new_x - c <= x0 + 1 || new_x + c >= x1 - 1 then (- dx) else dx
 and new_dy =
  if new_y - c <= y0 + 1 || new_y + c >= y1 - 1 then (- dy) else dy in
 Lwt.bind (wait ()) (fun () ->
     pong_aux new_x new_y new_dx new_dy);;

let rec start () =
  let t = Lwt.task () in
  let _,w = !state in
  state := t;
  clear_graph();
  Lwt.wakeup w ();
  Lwt.bind (Lwt_js.sleep (1./.60.)) start

let pong x y dx dy = pong_aux x y dx dy;;

pong 111 87 2 3;;
pong 28  57 5 3;;
start ();;
