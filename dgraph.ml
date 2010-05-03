
module Make (N : Set.OrderedType) = struct
  module NSet = Set.Make (N)
  module NMap = Map.Make (N)

  type t =
    { domain : NSet.t;
      fold_children : 'a . (N.t -> 'a -> 'a) -> N.t -> 'a -> 'a }

  let successors g x  = try NMap.find x g with Not_found -> NSet.empty

  let add_edge g x y =
    let l = successors g x in
    NMap.add x (NSet.add y l) g

  let invert g =
    let h =
      NSet.fold
        (fun x h -> g.fold_children (fun y h -> add_edge h y x) x h)
        g.domain NMap.empty
    in
    { domain = g.domain;
      fold_children = fun f x a -> NSet.fold f (successors h x) a }

  module type DOMAIN = sig type t val equal : t -> t -> bool val bot : t end

  module Solver (D : DOMAIN) = struct

    type stack = { stack : N.t Stack.t; mutable set : NSet.t }
    let is_empty st = NSet.is_empty st.set
    let pop st =
      let x = Stack.pop st.stack in
      st.set <- NSet.remove x st.set;
      x
    let push x st =
      if not (NSet.mem x st.set) then begin
        Stack.push x st.stack;
        st.set <- NSet.add x st.set
      end

    let rec iterate g f v w =
      if is_empty w then v else begin
        let x = pop w in
        let a = NMap.find x v in
        let b = f v x in
        let v = NMap.add x b v in
        if not (D.equal a b) then begin
          g.fold_children (fun y () -> push y w) x ();
          iterate g f v w
        end else
          iterate g f v w
     end

    let rec traverse g visited stack x =
      if not (NSet.mem x visited) then begin
        let visited = NSet.add x visited in
        let visited =
          g.fold_children
            (fun y visited -> traverse g visited stack y) x visited
        in
        Stack.push x stack;
        visited
      end else
        visited

    let traverse_all g =
      let stack = Stack.create () in
      let visited =
        NSet.fold (fun x visited -> traverse g visited stack x)
          g.domain NSet.empty
      in
      assert (NSet.equal g.domain visited);
      stack

    let f g f =
      let v = NSet.fold (fun x v -> NMap.add x D.bot v) g.domain NMap.empty in
      let w = { set = g.domain; stack = traverse_all g } in
      iterate g f v w

  end
end
