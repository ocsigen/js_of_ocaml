open Js

let box_style = "border: 1px black solid; background-color: white ;
                 display: inline ; padding-right: .5em; padding-left: .5em;"
let loading_style = "background-color: red; color: white; display:inline;
                     position: absolute; top:0; right:0;"
let loading parent =
  let div = Html.div ~style:loading_style [Html.string "LOADING..."] in
    Node.append parent div ;
    (fun () -> Node.remove parent div)

(*
let clock_div () =
  let t0 = ref (Sys.time ()) in
  let div = Html.div ~style:box_style [Html.string "--:--:--"] in
  let stopped = ref true in
    (div,
     (fun () -> t0 := Sys.time () ; stopped := false),
     (fun () -> stopped := true))
*)

type cell = Empty | Grass | Diamond | Boulder | Door | End | Guy | Wall | Bam
and state = {
  map : cell array array ;      imgs : Node.t array array ;
  mutable pos : int * int ;     mutable endpos : int * int ;
  mutable rem : int ;            mutable dead : bool ;
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
    if !changed then (
      fall state
    )

let rec build_interaction state show_rem =
    for y = 0 to Array.length state.map - 1 do
      for x = 0 to Array.length state.map.(y) - 1 do
	Node.clear_event state.imgs.(y).(x) "onmouseover" ;
	Node.clear_event state.imgs.(y).(x) "onmouseout" ;
	Node.clear_event state.imgs.(y).(x) "onclick"
      done
    done ;
    let inhibit f () =
      f ()
    in
    let set_pending_out f out () =
      f () ; state.pending_out_cb := Some out
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
	  click_cont () ;
	  if state.map.(y).(x) = Diamond then state.rem <- state.rem - 1 ;
	  set_cell state x y Guy ;
	  fall state ;
	  set_cell state x y Empty
	in
	let click () =
	  let gx, gy = state.pos in
	    set_cell state gx gy Empty ;
	    (try
	       click_cont () ;
	       if state.map.(y).(x) = Diamond then state.rem <- state.rem - 1 ;
	       set_cell state x y Guy ;
	       state.pos <- (x,y) ;
	       fall state ;
	     with Death -> state.dead <- true) ;
	    build_interaction state show_rem
	in
	  Node.register_event state.imgs.(y).(x) "onmouseover"
	    (inhibit (set_pending_out (with_pending_out over) out)) () ;
	  Node.register_event state.imgs.(y).(x) "onmouseout"
	    (inhibit (with_pending_out (fun () -> ()))) () ;
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
	    Node.set_attribute state.imgs.(y').(x') "src" img
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
	    (try fall state with Failure "DEAD" -> state.dead <- true) ;
	    build_interaction state show_rem
	  in
	    Node.register_event state.imgs.(y').(x') "onmouseover"
	      (inhibit (set_pending_out (with_pending_out over) out)) () ;
	    Node.register_event state.imgs.(y').(x') "onmouseout"
	      (inhibit (with_pending_out (fun () -> ()))) () ;
	    Node.register_event state.imgs.(y').(x') "onclick"
	      (inhibit (with_pending_out click)) () ;
	)
    in
      if state.pos = state.endpos then (
	alert "YOU WIN !"
      ) else
	if state.dead then (
	  alert "YOU LOSE !"
	) else ( 
	  if state.rem = 0 then (
	    let x,y = state.endpos in
	      Node.set_attribute state.imgs.(y).(x) "src" "sprites/end.png" ;
	      state.map.(y).(x) <- End  	
	  ) ;
	  let r (x, y) = succ x, y and l (x, y) = pred x, y in
	  let u (x, y) = x, pred y and d (x, y) = x, succ y in
	  let nil_cont () = () in
	    update (r state.pos) r "sprites/R.png" nil_cont nil_cont nil_cont ;
	    update (l state.pos) l "sprites/L.png" nil_cont nil_cont nil_cont ;
	    update (u state.pos) u "sprites/U.png" nil_cont nil_cont nil_cont ;
	    update (d state.pos) d "sprites/D.png" nil_cont nil_cont nil_cont ;
	    update_push state.pos r "sprites/bR.png" "sprites/push_r.png" ;
	    update_push state.pos l "sprites/bL.png" "sprites/push_l.png" ;
	    show_rem state.rem
	)


let _ =
  let body = Js.get_element_by_id "body" in
  let board_div = Html.div [] in
(*
  let (clock_div,clock_start,_) as clock = clock_div () in
*)
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
	     ~style:"border-collapse:collapse;line-height: 0; opacity: 1." ~attrs:["align", "center"]
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
	       dead = false ; rem = !rem ; pending_out_cb = ref None }
	     show_rem
      )
  in
    Node.set_attribute body "style"
      "font-family: sans-serif; text-align: center; background-color: #e8e8e8;" ;
    Node.append body (Html.h1 [Html.string "Boulder Dash in Ocaml "]) ;
    Node.append body
      (Html.div
	 [Html.string " Remaining diamonds: " ; rem_div ;
	  Html.string " " ;
	  Html.select
	    (Html.option [Html.string "Choose a level"]
	     :: (List.map (fun (f, n) -> Html.option ~onclick:(fun () -> load_level f) [Html.string n]) levels)) ;
	  Html.br () ; Html.br () ; board_div ])
      
