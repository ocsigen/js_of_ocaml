open Javascript


(* let rec interfer x = function *)
(*   | Statement st -> *)
(*   | Function_declaration f -> *)


(* let rec graph_of_sources l = *)
(*   List.fold_left graph_of_source [] l *)

(* and graph_of_source g s = *)
(*   match s with *)
(*     | Statement st -> g *)
(*     | Function_declaration f -> graph_of_fun_decl g f *)
(* and graph_of_fun_decl g (ident, params, body, _) = *)
(*   let vars = compute_var body in *)
(*   List.iter (interfer ident) vars; *)

module G = Graph.Pack.Graph
(* module G = struct *)
(*   include Graph.Imperative.Matrix.Graph *)
(*   module Mark = struct *)
(*       let h = Hashtbl.create 17 *)
(*       let get x = *)
(*         try *)
(*           Hashtbl.find h x *)
(*         with _ -> *)
(*           Hashtbl.add h x 0; *)
(*           0 *)
(*       let set x v = Hashtbl.add h x v *)
(*   end *)
(* end *)
(*
module D = struct
let round f = truncate (f +. 0.5)
let pi = 4.0 *. atan 1.0

open Graphics
let () = open_graph " 800x600"

let vertex_radius = 5

let draw_arrow ?(color=black) ?(width=1) (xu,yu) (xv,yv) =
  set_color color;
  set_line_width width;
  let dx = float (xv - xu) in
  let dy = float (yv - yu) in
  let alpha = atan2 dy dx in
  let r = sqrt (dx *. dx +. dy *. dy) in
  let ra = float vertex_radius *. 1.5 in
  let d = float vertex_radius +. 3. in
  let xs, ys = float xu +. d *. dx /. r, float yu +. d *. dy /. r in
  let xd, yd = float xv -. d *. dx /. r, float yv -. d *. dy /. r in
  let coords theta =
    round (xd +. ra *. cos (pi +. alpha +. theta)),
    round (yd +. ra *. sin (pi +. alpha +. theta))
  in
  moveto (round xs) (round ys);
  lineto (round xd) (round yd);
  let x1,y1 = coords (pi /. 6.) in
  moveto (round xd) (round yd); lineto x1 y1;
  let x2,y2 = coords (-. pi /. 6.) in
  moveto (round xd) (round yd); lineto x2 y2

let color_vertex v color =
  let x,y = G.V.label v in
  set_color color;
  fill_circle x y vertex_radius

let draw_graph g =
  set_color red;
  set_line_width 1;
  G.iter_vertex
    (fun v ->
       let (x,y) = G.V.label v in
       draw_circle x y vertex_radius)
    g;
  set_color black;
  G.iter_edges
    (fun v1 v2 -> draw_arrow (G.V.label v1) (G.V.label v2))
    g;
end

  *)

module V = struct

  module S = Code.VarSet
  module Map = Code.VarMap

  type t = {
    def : S.t;
    use : S.t;
    g : G.t;
    count : int Map.t
  }

  let incr_count (x : Code.Var.t) (map : int Map.t) n =
    let v = try Map.find x map with _ -> 0 in
    Map.add x (v + n) map

  let use_var t = function
    | S _ -> t
    | V i -> { t with
      use = S.add i t.use;
      count = incr_count i t.count 1 }

  let def_var t = function
    | S _ -> t
    | V i -> { t with
      def = S.add i t.def;
      count = incr_count i t.count 1}

  let rm_var t = function
    | S _ -> t
    | V i -> S.remove i t

  let merge_count f t =  Map.fold (fun k v map -> incr_count k map v) f t


  let empty g = {
    def = S.empty;
    use = S.empty;
    count = Map.empty;
    g
  }

  let vertex, vertex_tbl =
    let h = Hashtbl.create 17 in
    (fun v ->
      let idx = Code.Var.idx v in
      try Hashtbl.find h v with
        | Not_found ->
          let r = (G.V.create idx) in
          Hashtbl.add h v r;
          r),h

  let max_ = ref 0

  let mark g ~free ~use ~def =
    S.iter (fun u -> G.add_vertex g (vertex u)) def;
    let u = S.union def (S.union free use) in
    max_:= max !max_ (S.cardinal u);
    let f a b =
      S.iter (fun u1 ->
        S.iter (fun u2 ->
          if u1 <> u2
          then
            G.add_edge
              g
              (vertex u1)
              (vertex u2)
        ) a
      ) b
    in
    f use use;
    f use free;
    f def use;
    f def free

  let free t = S.diff t.use t.def

  let create () =
    (* empty (G.make (Code.Var.count ())) *)
    empty (G.create ())
  let rec expression t e = match e with
    | ECond (e1,e2,e3) ->
      expression
        (expression
           (expression t e1)
           e2
        )
        e3
    | ESeq (e1,e2)
    | EAccess (e1,e2)
    | EBin (_,e1,e2) ->
      expression (expression t e1) e2
    | EUn (_,e1)
    | EDot (e1,_)
    | ENew (e1,None) -> expression t e1
    | ECall (e,args)
    | ENew (e,Some args) ->
      List.fold_left (fun acc x ->
        expression acc x) (expression t e) args
    | EVar v -> use_var t v
    | EFun ((ident,params,body),_) ->
      let tbody = List.fold_left def_var (empty t.g) params in
      let tbody = match ident with
        | None -> tbody
        | Some v -> def_var tbody v in
      let tbody = source_elts tbody body in
      let tfree = free tbody in
      mark t.g ~free:tfree ~use:tbody.use ~def:tbody.def;
      {t with use = S.union t.use tfree ; count = merge_count t.count tbody.count}
    | EStr _
    | EBool _
    | ENum _
    | EQuote _ -> t
    | EObj l ->
      List.fold_left (fun acc (_,e) ->
        expression acc e) t l
    | EArr l ->
      List.fold_left (fun acc x ->
        match x with
          | None -> acc
          | Some e -> expression acc e) t l

  and source_elts t l =
    List.fold_left (fun acc s ->
      source_elt acc s) t l

  and source_elt t e = match e with
    | Statement s -> statement t s
    | Function_declaration (id,params, body, _) ->
      let tbody = List.fold_left def_var (empty t.g) params in
      let tbody = def_var tbody id in
      let tbody = source_elts tbody body in
      let tfree = free tbody in
      mark t.g ~free:tfree ~use:tbody.use ~def:tbody.def;
      def_var { t with use = S.union t.use tfree ; count = merge_count t.count tbody.count} id

  and statements t l = List.fold_left statement t l

  and statement t s = match s with
    | Block l -> List.fold_left statement t l
    | Variable_statement l ->
      List.fold_left (fun t (id,eopt) ->
        let t = def_var t id in
        match eopt with
          | None -> t
          | Some e -> expression t e) t l
    | Expression_statement (e,_) -> expression t e
    | If_statement(e1,s2,e3opt) ->
      let t = statement (expression t e1) s2 in
      begin
        match e3opt with
          | None -> t
          | Some e -> statement t e
      end
    | Do_while_statement (s,e)
    | While_statement (e,s) ->
      statement (expression t e) s
    | For_statement (e1,e2,e3,s,_) ->
      let t = List.fold_left (fun acc x ->
        match x with
          | None -> acc
          | Some e -> expression acc e ) t [e1;e2;e3] in
      statement t s
    | Continue_statement _
    | Break_statement _ -> t
    | Return_statement None -> t
    | Return_statement (Some e) -> expression t e
    | Labelled_statement (_,s) -> statement t s
    | Switch_statement(e,cl,sl) ->
      let t = expression t e in
      let t = List.fold_left (fun t (e, sl) ->
        let t = expression t e in
        statements t sl) t cl in
      begin match sl with
        | None -> t
        | Some sl -> statements t sl
      end
    | Throw_statement e ->
      expression t e
    | Try_statement (b,w,f,_) ->
      let t = statements t b in
      let t = match w with
        | None -> t
        | Some (id,block) ->
          let t' = statements (empty t.g) block in
          let t'' = def_var t' id in
          let t''free = free t'' in
          mark t.g ~free:t''free ~use:t''.use ~def:t''.def;
          { t with
            use = S.union t.use (rm_var t'.use id) ;
            def = S.union t.def t'.def;
            count = merge_count t.count t''.count}
            in
      let t = match f with
        | None -> t
        | Some block -> statements t block
      in t

  module M = Graph.Coloring.Mark(G)

  let program p : unit =
    let t = source_elts (create()) p in
    let freevar = free t in
    assert(S.cardinal freevar = 0);
    mark t.g ~free:freevar ~use:t.use ~def:t.def;
    Printf.printf "compute graph degree\n%!";
    let d = G.fold_vertex (fun v acc -> max acc (G.in_degree t.g v)) t.g 0 in
    let percent x all =
      float_of_int x /. float_of_int all *. 100. in
    let nb_vertex = (G.nb_vertex t.g) in
    Printf.printf "max degree is %d/%d/%d  %.2f\n%!" d (!max_) nb_vertex (percent (nb_vertex - !max_) nb_vertex);
    (* D.draw_graph t.g; *)

    let rec loop n max =
      let size = int_of_float (53.** (float_of_int n)) in
      let size = if n = 0 then !max_  else size in
      if n > max
      then raise Not_found
      else
        try
          Printf.printf "try coloring with %d (%d)\n%!" size n;
          M.coloring t.g size
        with _ -> loop (succ n) max in
    loop 0 3;
    let h = Hashtbl.create 17 in
    let color_count = Hashtbl.create 17 in
    Hashtbl.iter (fun k v ->

      let color = G.Mark.get v in
      let count = Map.find k t.count in

      let cc = try Hashtbl.find color_count color with
        | _ -> 0 in
      Hashtbl.replace color_count color (cc + count);
      Hashtbl.add h k color)
      vertex_tbl;
    (* Hashtbl.iter (fun k v -> *)
    (*   Printf.printf "var %d # %d\n" k v)  color_count; *)
    Code.Var.set_mapping(fun x ->
      let c = Hashtbl.find h x in c)

(* Printf.printf "freevar(%d) = %s " *)
(*   (V.S.cardinal freevar) *)
(*   (V.S.fold (fun id s -> s ^ ", " ^ (Code.Var.to_string id)) freevar ""); *)

end
