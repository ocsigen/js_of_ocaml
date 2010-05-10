open Js

let (>>=) = Lwt.bind

let box_style = "border: 1px black solid; background-color: white ;
                 display: inline ; padding-right: .5em; padding-left: .5em;"
let loading_style = "background-color: red; color: white; display:inline;
                     position: absolute; top:0; right:0;"
let loading parent =
  let div = Html.div ~style:loading_style [Html.string "LOADING..."] in
    Node.append parent div ;
    (fun () -> Node.remove parent div)

let clock_div () =
  let t0 = ref (Sys.time ()) in
  let div = Html.div ~style:box_style [Html.string "--:--:--"] in
  let stopped = ref true in
  let rec update_cb () =
    let dt = Sys.time () -. !t0 in
      if not !stopped then (
	let txt =
	  Node.text
	    (let secs = int_of_float dt in
	       Printf.sprintf "%02d:%02d:%02d" (secs / 3600) ((secs / 60) mod 60) (secs mod 60)
	    ) in
	  Node.empty div ; Node.append div txt ) ;
      Lwt_js.sleep 1. >>= fun () -> update_cb ()
  in
    ignore (update_cb ()) ;
    (div,
     (fun () -> t0 := Sys.time () ; stopped := false),
     (fun () -> stopped := true))

type cell = Empty | Grass | Diamond | Boulder | Door | End | Guy | Wall | Bam
and state = {
  map : cell array array ;      imgs : Node.t array array ;
  mutable pos : int * int ;     mutable endpos : int * int ;
  mutable rem : int ;            mutable dead : bool ;
  mutable map_mutex : Lwt_mutex.t ; mutable events_mutex : bool ;
  mutable pending_out_cb : (unit -> unit) option ref ;
}
exception Death

let img_assoc = 
  [ (Empty, "sprites/empty.png"); (Bam, "sprites/bam.png"); (Grass, "sprites/grass.png"); (Diamond, "sprites/diamond.png");
    (Boulder, "sprites/boulder.png"); (End, "sprites/end.png"); (Door, "sprites/door.png"); (Guy, "sprites/guy.png"); (Wall, "sprites/wall.png")]

let set_cell state x y v = 
  state.map.(y).(x) <- v ;
  Node.set_attribute state.imgs.(y).(x) "src" (List.assoc v img_assoc)

let walkable = function | Empty | Grass | Diamond | End -> true | _-> false

let rec fall state =
  (* assumes wall borders *)
  let changed = ref false in
    for y = Array.length state.map - 2 downto 1 do
      for x = 1 to Array.length state.map.(y) - 2 do
	let sustaining = state.map.(y + 1).(x) = Guy && state.map.(y).(x) = Boulder in
	  if (state.map.(y).(x) = Empty
	      && state.map.(y - 1).(x) = Boulder) then (
	    set_cell state x (y - 1) Empty ;
	    set_cell state x y Boulder ;
	    changed := true
	  ) ;
	  if (state.map.(y).(x) = Empty
	      && state.map.(y - 1).(x) = Empty
	      && state.map.(y).(x - 1) = Boulder
	      && state.map.(y - 1).(x - 1) = Boulder) then (
	    set_cell state (x - 1) (y - 1) Empty ;
	    set_cell state x y Boulder ;
	    changed := true
	  ) ;
	  if (state.map.(y).(x) = Empty
	      && state.map.(y - 1).(x) = Empty
	      && state.map.(y).(x + 1) = Boulder
	      && state.map.(y - 1).(x + 1) = Boulder) then (
	    set_cell state (x + 1) (y - 1) Empty ;
	    set_cell state x y Boulder ;
	    changed := true
	  ) ;
	  if (not sustaining) && state.map.(y + 1).(x) = Guy && state.map.(y).(x) = Boulder then (
	    set_cell state x (y + 1) Bam ;
	    raise Death
	  )
      done
    done ;
    if !changed then begin
      Lwt_js.sleep 0.05 >>= fun () ->
      fall state
    end else
      Lwt.return ()

let rec build_interaction state show_rem ((_,_, clock_stop) as clock) =
  Lwt_mutex.lock state.map_mutex >>= fun () ->
    for y = 0 to Array.length state.map - 1 do
      for x = 0 to Array.length state.map.(y) - 1 do
	Node.clear_event state.imgs.(y).(x) "onmouseover" ;
	Node.clear_event state.imgs.(y).(x) "onmouseout" ;
	Node.clear_event state.imgs.(y).(x) "onclick"
      done
    done ;
    let inhibit f () =
      if not state.events_mutex then begin
        state.events_mutex <- true;
        f () >>= fun () ->
        state.events_mutex <- false;
        Lwt.return ()
      end else
        Lwt.return ()
    in
    let set_pending_out f out () =
      f () >>= fun () -> state.pending_out_cb := Some out; Lwt.return ()
    in
    let with_pending_out f () =
      match !(state.pending_out_cb) with
	| None -> f ()
	| Some out -> out () ; state.pending_out_cb := None ; f ()
    in
    let rec update (x, y) next img over_cont out_cont click_cont =
      if walkable state.map.(y).(x) then (
	let cur_img = Node.get_attribute state.imgs.(y).(x) "src" in
	let over () = Node.set_attribute state.imgs.(y).(x) "src" img ; over_cont ()
	and out () = Node.set_attribute state.imgs.(y).(x) "src" cur_img ; out_cont ()
	and click' () =
	  click_cont () >>= fun () ->
	  if state.map.(y).(x) = Diamond then state.rem <- state.rem - 1 ;
	  set_cell state x y Guy ;
	  Lwt_js.sleep 0.05 >>= fun () ->
	  fall state >>= fun () ->
	  set_cell state x y Empty;
          Lwt.return ()
	in
	let click () =
	  let gx, gy = state.pos in
	    set_cell state gx gy Empty ;
	    (Lwt.catch (fun () ->
	       click_cont () >>= fun () ->
	       if state.map.(y).(x) = Diamond then state.rem <- state.rem - 1 ;
	       set_cell state x y Guy ;
	       state.pos <- (x,y) ;
	       fall state)
               (fun e ->
                  match e with
	            Death -> state.dead <- true; Lwt.return ()
                  | _     -> Lwt.fail e)) >>= fun () ->
	    build_interaction state show_rem clock
	in
	  Node.register_event state.imgs.(y).(x) "onmouseover"
	    (inhibit (set_pending_out (with_pending_out over) out)) () ;
	  Node.register_event state.imgs.(y).(x) "onmouseout"
	    (inhibit (with_pending_out (fun () -> Lwt.return ()))) () ;
	  Node.register_event state.imgs.(y).(x) "onclick"
	    (inhibit (with_pending_out click)) () ;
	  if state.map.(y).(x) <> End then
	    update (next (x,y)) next img over out click'
      )
    in
    let update_push ((x, y) as pos) next img img_guy=
      let ((x', y') as pos') = next pos in
      let (x'', y'') = next pos' in
	if (try
	      state.map.(y').(x') = Boulder && state.map.(y'').(x'') = Empty
	    with Invalid_argument "index out of bounds" -> false) then (
	  let over () =
	    Node.set_attribute state.imgs.(y).(x) "src" img_guy ;
	    Node.set_attribute state.imgs.(y').(x') "src" img;
            Lwt.return ()
	  in
	  let out () =
	    Node.set_attribute state.imgs.(y).(x) "src" "sprites/guy.png" ;
	    Node.set_attribute state.imgs.(y').(x') "src" "sprites/boulder.png"
	  in
	  let click () =
	    set_cell state x y Empty ;
	    set_cell state x' y' Guy ;
	    state.pos <- pos' ;
	    set_cell state x'' y'' Boulder ;
	    Lwt.catch
              (fun () -> fall state)
              (fun e ->
                 match e with
                   Death -> state.dead <- true; Lwt.return ()
                 | e     -> Lwt.fail e) >>= fun () ->
	    build_interaction state show_rem clock
	  in
	    Node.register_event state.imgs.(y').(x') "onmouseover"
	      (inhibit (set_pending_out (with_pending_out over) out)) () ;
	    Node.register_event state.imgs.(y').(x') "onmouseout"
	      (inhibit (with_pending_out (fun () -> Lwt.return ()))) () ;
	    Node.register_event state.imgs.(y').(x') "onclick"
	      (inhibit (with_pending_out click)) () ;
	)
    in
      if state.pos = state.endpos then (
	clock_stop () ; alert "YOU WIN !"
      ) else
	if state.dead then (
	  clock_stop () ; alert "YOU LOSE !"
	) else ( 
	  if state.rem = 0 then (
	    let x,y = state.endpos in
	      Node.set_attribute state.imgs.(y).(x) "src" "sprites/end.png" ;
	      state.map.(y).(x) <- End  	
	  ) ;
	  let r (x, y) = succ x, y and l (x, y) = pred x, y in
	  let u (x, y) = x, pred y and d (x, y) = x, succ y in
	  let nil_cont () = () in
	  let nil_cont_async () = Lwt.return () in
	    update (r state.pos) r "sprites/R.png" nil_cont_async nil_cont nil_cont_async ;
	    update (l state.pos) l "sprites/L.png" nil_cont_async nil_cont nil_cont_async ;
	    update (u state.pos) u "sprites/U.png" nil_cont_async nil_cont nil_cont_async ;
	    update (d state.pos) d "sprites/D.png" nil_cont_async nil_cont nil_cont_async ;
	    update_push state.pos r "sprites/bR.png" "sprites/push_r.png" ;
	    update_push state.pos l "sprites/bL.png" "sprites/push_l.png" ;
	    show_rem state.rem
	) ;
      Lwt_mutex.unlock state.map_mutex;
      Lwt.return ()


let _ =
  let body = Js.get_element_by_id "body" in
  let board_div = Html.div [] in
  let (clock_div,clock_start,_) as clock = clock_div () in
  let load_data name process=
    let loading_end = loading body in
    let data = http_get name in
    let res = process data in
      loading_end () ;
      res
  in
  let rem_div, show_rem =
    let div = Html.div ~style:box_style [Html.string "--"] in
      (div, (fun v -> Node.replace_all div (Html.int v)))
  in
  let levels =
    load_data
      "maps.txt" 
      (fun txt ->
	 let find_string st =
	   let sz = String.length txt in
	   let rec find_string_start s =
	     if s >= sz then
	       failwith "eos"
	     else
	       if txt.[s] == '"' then
		 find_string_end (s + 1) (s + 2)
	       else
		 find_string_start (s + 1)
	   and find_string_end s e =
	     if s >= sz then
	       failwith "eos"
	     else
	       if txt.[e] == '"' then
		 (String.sub txt s (e - s), e + 1)
	       else
		 find_string_end s (e + 1)
	   in find_string_start st
	 in
	 let rec scan_pairs st acc =
	   match
	     try
	       let fst, st = find_string st in
	       let snd, st = find_string st in
		 Some ((fst, snd), st)
	     with Failure "eos" -> None
	   with
	     | Some (elt, st) -> scan_pairs st (elt :: acc)
	     | None -> acc
	 in List.rev (scan_pairs 0 []))
  in
  let load_level file =
    load_data file
      (fun data ->
	 let map, cells =
	   let res = ref [] and row = ref [] in
	     for i = 0 to String.length data - 1 do
	       match data.[i] with
		 | '\n' -> res := List.rev (!row) :: !res ; row := []
		 | '#' -> row := Wall :: !row
		 | '.' -> row := Grass :: !row
		 | ' ' -> row := Empty :: !row
		 | '+' -> row := Diamond :: !row
		 | 'X' -> row := Boulder :: !row
		 | 'W' -> row := Guy :: !row
		 | 'E' -> row := Door :: !row | 'S' -> row := Guy :: !row
		 | _ -> failwith "malformed level"
	     done ;
	     let map = Array.of_list (List.map Array.of_list (List.rev !res)) in
	       map, Array.map (Array.map
				 (fun c -> Html.img ~src:(List.assoc c img_assoc) ())) map
	 in 
	 let gx = ref 0 and gy = ref 0 and ex = ref 0 and ey = ref 0 and rem = ref 0 in
	 let table =
	   Html.map_table
	     ~style:"border-collapse:collapse;line-height: 0; opacity: 0" ~attrs:["align", "center"]
	     ~td_style:"padding: 0; width: 20px; height: 20px;"
	     (fun y x cell ->
		(match map.(y).(x) with
		   | Guy -> gx := x ; gy := y
		   | Diamond -> incr rem
		   | Door -> ex := x ; ey := y
		   | _ -> ()) ; cell)
	     cells
	 in
	   Node.replace_all board_div table ;
	   build_interaction
	     { map = map; imgs = cells ; pos = (!gx, !gy) ; endpos = (!ex, !ey) ;
	       map_mutex = Lwt_mutex.create () ; events_mutex = false ;
	       dead = false ; rem = !rem ; pending_out_cb = ref None }
	     show_rem clock >>= fun () ->
	   let t0 = Sys.time () in
	   let rec fade () =
	     let t = Sys.time () in
	       if t -. t0 >= 1. then (
		 Node.set_attribute table "style"
		   "border-collapse:collapse;line-height: 0; opacity:1";
                 Lwt.return ()
	       ) else (
		 Lwt_js.sleep 0.05 >>= fun () ->
		 Node.set_attribute table "style"
		   (Printf.sprintf "border-collapse:collapse;line-height: 0; opacity:%g" (t -. t0)) ;
		 fade ()
	       )
	   in fade () >>= fun () -> clock_start (); Lwt.return ()
      )
  in
    Node.set_attribute body "style"
      "font-family: sans-serif; text-align: center; background-color: #e8e8e8;" ;
    Node.append body (Html.h1 [Html.string "Boulder Dash in Ocaml "]) ;
    Node.append body
      (Html.div
	 [Html.string "Elapsed time: " ; clock_div ; Html.string " Remaining diamonds: " ; rem_div ;
	  Html.string " " ;
	  Html.select
	    (Html.option [Html.string "Choose a level"]
	     :: (List.map (fun (f, n) -> Html.option ~onclick:(fun () -> load_level f) [Html.string n]) levels)) ;
	  Html.br () ; Html.br () ; board_div ])
      
