open Javascript

let debug = Option.Debug.find "shortvar"

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

module S = Code.VarSet
module V = Code.Var
module VM = Code.VarMap

type t = {
  def : S.t;
  use : S.t;
  g : G.t;
  count : int VM.t;
  biggest : int;
  vertex : (Code.Var.t, G.V.t) Hashtbl.t;
}

let incr_count (x : Code.Var.t) (map : int VM.t) n =
  let v = try VM.find x map with _ -> 0 in
  VM.add x (v + n) map

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

let merge_count f t =  VM.fold (fun k v map -> incr_count k map v) f t


let empty t = {
  t with
    def = S.empty;
    use = S.empty;
    count = VM.empty;
    biggest = 0;
}

let vertex t v =
  let idx = Code.Var.idx v in
  try
    Hashtbl.find t.vertex v
  with Not_found ->
    let r = (G.V.create idx) in
    Hashtbl.add t.vertex v r;
    r

let get_free t = S.diff t.use t.def

let mark g =
  let u = S.union g.def g.use in
  S.iter (fun u -> G.add_vertex g.g (vertex g u)) u;
  S.fold (fun u1 set ->
    let set = S.remove u1 set in
    S.iter (fun u2 ->
      if u1 <> u2
      then
        G.add_edge
          g.g
          (vertex g u1)
          (vertex g u2)
    ) set;
    set
  ) u u;
  {g with biggest = max g.biggest (S.cardinal u)}

let create () = (* empty (G.make (Code.Var.count ())) *)
  {
    def = S.empty;
    use = S.empty;
    count = VM.empty;
    biggest = 0;
    vertex = Hashtbl.create 17;
    g = G.create ()
  }

let merge_info ~from ~into =
  let free = get_free from in
  {into with
    count = merge_count from.count into.count;
    biggest = max from.biggest into.biggest;
    use = S.union into.use free }

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
    let tbody = List.fold_left def_var (empty t) params in
    let tbody = match ident with
      | None -> tbody
      | Some v -> def_var tbody v in
    let tbody = source_elts tbody body in
    let tbody = mark tbody in
    merge_info ~from:tbody ~into:t
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
    let tbody = List.fold_left def_var (empty t) params in
    let tbody = def_var tbody id in
    let tbody = source_elts tbody body in
    let tbody = mark tbody in
    def_var (merge_info ~from:tbody ~into:t) id

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
        let t = statements t block in
        let t = def_var t id in
        t
        (* let tbody = statements (empty t) block in *)
        (* let tbody = def_var tbody id in *)
        (* let tbody = mark tbody in *)
        (* let t = merge_info ~from:tbody ~into:t in *)
        (* { t with *)
        (*   use = S.union t.use (rm_var t.use id) ; *)
        (*   def = S.union t.def (rm_var t.def id) } *)
    in
    let t = match f with
      | None -> t
      | Some block -> statements t block
    in t

module M = Graph.Coloring.Mark(G)

let program p =
  let t = source_elts (create()) p in
  assert(S.cardinal (get_free t) = 0);
  let t = mark t in
  if debug ()
  then Printf.eprintf "compute graph degree\n%!";
  let degree = G.fold_vertex (fun v acc -> max acc (G.in_degree t.g v)) t.g 0 in
  let percent x all =
    float_of_int x /. float_of_int all *. 100. in
  let nb_vertex = (G.nb_vertex t.g) in

  if debug ()
  then Printf.eprintf "degree:%d; optimal:%d #:%d gain:%.2f%%\n%!" degree t.biggest nb_vertex
    (percent (nb_vertex - t.biggest) nb_vertex);

  let rec loop = function
    | [] -> raise Not_found
    | k :: rem ->
      try
        if debug ()
        then Printf.eprintf "try coloring with %d\n%!" k;
        M.coloring t.g k
      with _ -> loop rem in
  loop [t.biggest;degree];

  (* build the mapping function *)
  let color_map = Hashtbl.fold (fun var vertex map ->
    let color = G.Mark.get vertex in
    let varset  = S.add var (try VM.find (V.from_idx color) map with _ -> S.empty) in
    let map = VM.add (V.from_idx color) varset map in
    map
  ) t.vertex VM.empty in
  let arr = Array.of_list (VM.bindings color_map) in
  Array.sort (fun (_,i) (_,j) -> (S.cardinal j) - (S.cardinal i)) arr;
  let _,map = Array.fold_left (fun (i,map) (_,varset) ->
    (* let count = S.cardinal varset in *)
    let name = V.to_string (V.from_idx i) in
    succ i,
    S.fold(fun var map ->
      VM.add var name map) varset map) (0,VM.empty) arr
  in
  (fun v -> VM.find v map)
