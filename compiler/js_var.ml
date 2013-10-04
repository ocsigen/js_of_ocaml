
(*
We are trying to achieve the following goals:
(1) variable names should be as short as possible
(2) one should reuse as much as possible a small subsets of variable
    names
(3) function parameters should be: function(a,b,...){...}
(4) for longer variable names, variable which are closed from one
    another should share a same prefix

Point (1) minimizes the size of uncompressed files, while point (2) to
(4) improve compression.

We use the following strategy. We maintain the constraint that
variables occurring in a function should keep different names.
We first assign names a, b, ... (in order) to function parameters,
starting from inner functions, skipping variables which have a
conflict with a previously names variable (goal 3). Then, we order
the remaining variables by their number of occurrences, then by
their index (goal 4), and greedily assigned name to them. For that,
we use for each variable the smallest possible name still available
(goal 1/2).

This algorithm seems effective. Here are some statistics gathered
while compiling the OCaml toplevel:
(1) We get 132025 occurrences of one-char variables out of 169728
    occurrences while the optimal number (determined using a mixed
    integer linear programming solver) is 132105 occurrences (80 more
    occurrences).
(2) Variable names are heavily biased toward character a: among
    variables, we have about 34000 occurrences of character a, less
    than 5000 occurrences of character i (9th character, out of the 54
    characters that can start an identifier), and about 1500
    occurrences of character A.
(3) About 6% of the function parameters are not assigned as wanted;
    it is not clear we can do any better: there are a lot of nested
    functions.
(4) We save 8181 bytes on the compressed file (1.8%) by sorting
    variables using their index as a secondary key rather that just
    based on their weights (the size of the uncompressed file remains
    unchanged)
*)

open Util
open Javascript

let debug = Option.Debug.find "shortvar"

module S = Code.VarSet
module V = Code.Var
module VM = Code.VarMap

type alloc =
  { mutable first_free : int;
    mutable used : bool array }

let make_alloc_table () =
  { first_free = 0;
    used = Array.make 32 false }

let next_available a i =
  let i = ref (max i a.first_free) in
  let len = Array.length a.used in
  while !i < len && a.used.(!i) do incr i done;
  !i

let allocate a i =
  let len = Array.length a.used in
  if i >= len then begin
    let l = ref len in
    while l := 2 * !l; i >= !l do () done;
    let u = Array.make !l false in
    Array.blit a.used 0 u 0 len;
    a.used <- u
  end;
  assert (not a.used.(i));
  a.used.(i) <- true;
  if a.first_free = i then begin
    let i = ref a.first_free in
    let len = Array.length a.used in
    while !i < len && a.used.(!i) do incr i done;
    a.first_free <- !i
  end

let is_available l i =
  List.for_all (fun a -> Array.length a.used <= i || not a.used.(i)) l

let first_available l =
  let rec find_rec n =
    let n' = List.fold_left (fun n a -> next_available a n) n l in
    if n = n' then n else find_rec n'
  in
  find_rec 0

let mark_allocated l i = List.iter (fun a -> allocate a i) l

type g = {
  weight : int array;        (* Number of occurrences of each variable *)
  constr : alloc list array; (* Constraints on variables *)
  mutable parameters : V.t list array; (* Function parameters *)
  mutable constraints : S.t list }     (* For debugging *)

type t = {
  def : S.t;
  use : S.t;
  def_name: StringSet.t;
  use_name: StringSet.t;
  global: g
}

let use_name s t = { t with use_name = StringSet.add s t.use_name  }

let def_name s t = { t with def_name = StringSet.add s t.def_name  }

let bump_weight t i =
  let idx = Code.Var.idx i in
  let weight = t.global.weight in
  weight.(idx) <- weight.(idx) + 1

let use_var t = function
  | S s -> use_name s t
  | V i -> bump_weight t i;
           { t with use = S.add i t.use }

let def_var t = function
  | S s -> def_name s t
  | V i -> bump_weight t i;
           { t with def = S.add i t.def }

let empty t = {
  t with
    def = S.empty;
    use = S.empty;
    def_name = StringSet.empty;
    use_name = StringSet.empty }

let get_free t = S.diff t.use t.def

let get_free_name t = StringSet.diff t.use_name t.def_name

let add_constraints params g =
  if Option.Optim.shortvar () then begin
    let u = S.union g.def g.use in
    let constr = g.global.constr in
    let c = make_alloc_table () in
    S.iter
      (fun v -> let i = Code.Var.idx v in constr.(i) <- c :: constr.(i)) u;
    let params = Array.of_list params in
    let len = Array.length params in
    if Array.length g.global.parameters < len then begin
      let a = Array.make (2 * len) [] in
      Array.blit g.global.parameters 0 a 0 (Array.length g.global.parameters);
      g.global.parameters <- a
    end;
    for i = 0 to len - 1 do
      match params.(i) with
        Javascript.V x ->
          g.global.parameters.(i) <- x :: g.global.parameters.(i)
      | _ ->
          ()
    done;
    g.global.constraints <- u :: g.global.constraints
  end

let create () =
  let nv = Code.Var.count () in
  { def = S.empty;
    use = S.empty;
    use_name = StringSet.empty;
    def_name = StringSet.empty;
    global =
      { weight = Array.create nv 0;
        constr = Array.create nv [];
        parameters = [|[]|];
        constraints = [] } }

let merge_info ~from ~into =
  let free = get_free from in
  let free_name = get_free_name from in
  { into with
    use = S.union into.use free;
    use_name = StringSet.union into.use_name free_name }

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
    add_constraints params tbody;
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
    let tbody = source_elts tbody body in
    add_constraints params tbody;
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
  | Empty_statement -> t
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
  | ForIn_statement (e1,e2,s,_) ->
    let t = List.fold_left expression t [e1;e2] in
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

let output_debug_information t =
  let usage =
    List.fold_left
      (fun u s ->
         S.fold
           (fun v u -> VM.add v (try 1 + VM.find v u with Not_found -> 1) u)
           s u)
      VM.empty t.global.constraints
  in

  let l = List.map fst (VM.bindings usage) in

  let ch = open_out "/tmp/weights.txt" in
  List.iter
    (fun v ->
       Printf.fprintf ch "%d / %d / %d\n" (t.global.weight.(Code.Var.idx v))
         (VM.find v usage) (Code.Var.idx v))
    l;
  close_out ch;

  let ch = open_out "/tmp/problem.txt" in
  Printf.fprintf ch "Maximize\n";
  let a = Array.of_list l in
  Printf.fprintf ch "  ";
  for i = 0 to Array.length a - 1 do
    let v = a.(i) in
    let w = t.global.weight.(Code.Var.idx v) in
    if i > 0 then Printf.fprintf ch " + ";
    Printf.fprintf ch "%d x%d" w (Code.Var.idx v)
  done;
  Printf.fprintf ch "\n";
  Printf.fprintf ch "Subject To\n";
  List.iter
    (fun s ->
      if S.cardinal s > 0 then begin
        Printf.fprintf ch "  ";
        let a = Array.of_list (S.elements s) in
        for i = 0 to Array.length a - 1 do
          if i > 0 then Printf.fprintf ch " + ";
          Printf.fprintf ch "x%d" (Code.Var.idx a.(i))
        done;
        Printf.fprintf ch "<= 54\n"
      end)
    t.global.constraints;
  Printf.fprintf ch "Binary\n  ";
  List.iter (fun v -> Printf.fprintf ch " x%d" (Code.Var.idx v)) l;
  Printf.fprintf ch "\nEnd\n";
  close_out ch;

  let ch = open_out "/tmp/problem2" in
  let var x = string_of_int (Code.Var.idx x) in
  let a = List.map (fun v -> (var v, t.global.weight.(Code.Var.idx v))) l in
  let b =
    List.map (fun s -> List.map var (S.elements s)) t.global.constraints in
  let c = List.map var l in
  output_value ch
    ((a, b, c) : (string * int) list * string list list * string list);
  close_out ch

let allocate_variables t =
  let weight = t.global.weight in
  let constr = t.global.constr in
  let len = Array.length weight in
  let idx = Array.make len 0 in
  for i = 0 to len - 1 do
    idx.(i) <- i
  done;
  Array.stable_sort (fun i j -> compare weight.(j) weight.(i)) idx;
  let name = Array.make len "" in
  let n0 = ref 0 in
  let n1 = ref 0 in
  let n2 = ref 0 in
  let n3 = ref 0 in
  let stats i n =
    incr n0;
    if n < 54 then begin incr n1; n2 := !n2 + weight.(i) end;
    n3 := !n3 + weight.(i)
  in
  let names = Hashtbl.create 1024 in
  let nm n =
    try
      Hashtbl.find names n
    with Not_found ->
      let nm = V.to_string (V.of_idx n) in
      Hashtbl.add names n nm;
      nm
  in
  let total = ref 0 in
  let bad = ref 0 in
  for i = 0 to Array.length t.global.parameters - 1 do
    List.iter
      (fun x ->
         incr total;
         let l = constr.(V.idx x) in
         if is_available l i then begin
           name.(V.idx x) <- nm i;
           mark_allocated l i;
           stats (V.idx x) i
         end else
           incr bad)
      (List.rev t.global.parameters.(i))
  done;
  if debug () then
    Format.eprintf
      "Function parameter properly assigned: %d/%d@." (!total - !bad) !total;
  for i = 0 to len - 1 do
    let l = constr.(idx.(i)) in
    if l <> [] && String.length name.(idx.(i)) = 0 then begin
      let n = first_available l in
      name.(idx.(i)) <- nm n;
      mark_allocated l n;
      stats idx.(i) n
    end;
    if l = [] then assert (weight.(idx.(i)) = 0);
  done;
  if debug () then begin
    Format.eprintf "short variable count: %d/%d@." !n1 !n0;
    Format.eprintf "short variable occurrences: %d/%d@." !n2 !n3
  end;
  name

let program p =
  let t = source_elts (create()) p in
  add_constraints [] t;

  assert (S.cardinal (get_free t) = 0);

  let free_name = get_free_name t in
  StringSet.iter (fun s ->
    (* Printf.eprintf "use %s\n%!" s; *)
    Reserved.add s;
    Primitive.mark_used s;
  ) free_name;
  if Option.Optim.shortvar () then begin
    let name = allocate_variables t in
    if debug () then output_debug_information t;
    (fun v -> name.(Code.Var.idx v))
  end else
    (fun v -> Code.Var.to_string v)
