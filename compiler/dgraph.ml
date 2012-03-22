(* Js_of_ocaml compiler
 * http://www.ocsigen.org/js_of_ocaml/
 * Copyright (C) 2010 Jérôme Vouillon
 * Laboratoire PPS - CNRS Université Paris Diderot
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU Lesser General Public License as published by
 * the Free Software Foundation, with linking exception;
 * either version 2.1 of the License, or (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
 *)

module Make (N : sig type t end)
            (NSet : Set.S with type elt = N.t)
            (NMap : Map.S with type key = N.t) = struct

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
    let n = ref 0
    let m = ref 0

    type stack = { stack : N.t Stack.t; mutable set : NSet.t }
    let is_empty st = Stack.is_empty st.stack
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
        incr m;
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
      n := 0; m := 0;
(*
let t1 = Util.Timer.make () in
*)
      let v = NSet.fold (fun x v -> incr n; NMap.add x D.bot v) g.domain NMap.empty in
(*
let t1 = Util.Timer.get t1 in
let t2 = Util.Timer.make () in
*)
      let w = { set = g.domain; stack = traverse_all g } in
(*
let t2 = Util.Timer.get t2 in
let t3 = Util.Timer.make () in
*)
      let res = iterate g f v w in
(*
let t3 = Util.Timer.get t3 in
      Format.eprintf "YYY %.2f %.2f %.2f@." t1 t2 t3;
      Format.eprintf "YYY %d %d (%f)@." !m !n (float !m /. float !n);
*)
      res

  end
end

module type ISet = sig
  type t
  type elt

  val iter : (elt -> unit) -> t -> unit
  val mem : t -> elt -> bool
  val add : t -> elt -> unit
  val remove : t -> elt -> unit
  val copy : t -> t
end

module type Tbl = sig
  type 'a t
  type key
  type size
  val get : 'a t -> key -> 'a
  val set : 'a t -> key -> 'a -> unit
  val make : size -> 'a -> 'a t
end

module Make_Imperative
  (N : sig type t end)
  (NSet : ISet with type elt = N.t)
  (NTbl : Tbl with type key = N.t) =
struct

  type t = { domain : NSet.t; iter_children : (N.t -> unit) -> N.t -> unit }

  let successors g x  = NTbl.get g x

  let add_edge g x y = NTbl.set g x (y :: successors g x)

  let invert size g =
    let h = NTbl.make size [] in
    NSet.iter (fun x -> g.iter_children (fun y -> add_edge h y x) x) g.domain;
    { domain = g.domain;
      iter_children = fun f x -> List.iter f (successors h x) }

  module type DOMAIN = sig type t val equal : t -> t -> bool val bot : t end

  module Solver (D : DOMAIN) = struct
    let n = ref 0
    let m = ref 0

    type stack = { stack : N.t Stack.t; mutable set : NSet.t }
    let is_empty st = Stack.is_empty st.stack
    let pop st =
      let x = Stack.pop st.stack in
      NSet.add st.set x;
      x
    let push x st =
      if NSet.mem st.set x then begin
        Stack.push x st.stack;
        NSet.remove st.set x
      end

    let rec iterate g f v w =
      if is_empty w then v else begin
        let x = pop w in
        let a = NTbl.get v x in
        incr m;
        let b = f v x in
        NTbl.set v x b;
        if not (D.equal a b) then begin
          g.iter_children (fun y -> push y w) x;
          iterate g f v w
        end else
          iterate g f v w
     end

    let rec traverse g to_visit stack x =
      if NSet.mem to_visit x then begin
        NSet.remove to_visit x;
        incr n;
        g.iter_children (fun y -> traverse g to_visit stack y) x;
        Stack.push x stack
      end

    let traverse_all g =
      let stack = Stack.create () in
      let to_visit = NSet.copy g.domain in
      NSet.iter (fun x -> traverse g to_visit stack x) g.domain;
      { stack = stack; set = to_visit }

    let f size g f =
      n := 0; m := 0;
(*
let t1 = Util.Timer.make () in
*)
      let v = NTbl.make size D.bot in
(*
let t1 = Util.Timer.get t1 in
let t2 = Util.Timer.make () in
*)
      let w = traverse_all g in
(*
let t2 = Util.Timer.get t2 in
let t3 = Util.Timer.make () in
*)
      let res = iterate g f v w in
(*
let t3 = Util.Timer.get t3 in
      Format.eprintf "YYY %.2f %.2f %.2f@." t1 t2 t3;
      Format.eprintf "YYY %d %d (%f)@." !m !n (float !m /. float !n);
*)
      res

  end
end
