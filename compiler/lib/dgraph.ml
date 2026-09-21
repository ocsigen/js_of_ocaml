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
open! Stdlib

module type SET = sig
  type elt

  type t

  val empty : t

  val add : elt -> t -> t

  val remove : elt -> t -> t

  val fold : (elt -> 'a -> 'a) -> t -> 'a -> 'a

  val mem : elt -> t -> bool

  val equal : t -> t -> bool
end

module type MAP = sig
  type key

  type 'a t

  val empty : 'a t

  val find : key -> 'a t -> 'a

  val find_opt : key -> 'a t -> 'a option

  val add : key -> 'a -> 'a t -> 'a t
end

module Make
    (N : sig
      type t
    end)
    (NSet : SET with type elt = N.t)
    (NMap : MAP with type key = N.t) =
struct
  type t =
    { domain : NSet.t
    ; fold_children : 'a. (N.t -> 'a -> 'a) -> N.t -> 'a -> 'a
    }

  let successors g x = NMap.find_opt x g |> Option.value ~default:NSet.empty

  let add_edge g x y =
    let l = successors g x in
    NMap.add x (NSet.add y l) g

  let invert g =
    let h =
      NSet.fold
        (fun x h -> g.fold_children (fun y h -> add_edge h y x) x h)
        g.domain
        NMap.empty
    in
    { domain = g.domain; fold_children = (fun f x a -> NSet.fold f (successors h x) a) }

  module type DOMAIN = sig
    type t

    val equal : t -> t -> bool

    val bot : t
  end

  module Solver (D : DOMAIN) = struct
    let n = ref 0

    let m = ref 0

    type queue =
      { queue : N.t Queue.t
      ; mutable set : NSet.t
      }

    let is_empty st = Queue.is_empty st.queue

    let pop st =
      let x = Queue.pop st.queue in
      st.set <- NSet.remove x st.set;
      x

    let push x st =
      if not (NSet.mem x st.set)
      then (
        Queue.push x st.queue;
        st.set <- NSet.add x st.set)

    let rec iterate g f v w =
      if is_empty w
      then v
      else
        let x = pop w in
        let a = NMap.find x v in
        incr m;
        let b = f v x in
        let v = NMap.add x b v in
        if not (D.equal a b)
        then (
          g.fold_children (fun y () -> push y w) x ();
          iterate g f v w)
        else iterate g f v w

    let rec traverse g visited lst x =
      if not (NSet.mem x visited)
      then (
        let visited = NSet.add x visited in
        let visited =
          g.fold_children (fun y visited -> traverse g visited lst y) x visited
        in
        lst := x :: !lst;
        visited)
      else visited

    let traverse_all g =
      let lst = ref [] in
      let visited =
        NSet.fold (fun x visited -> traverse g visited lst x) g.domain NSet.empty
      in
      assert (NSet.equal g.domain visited);
      let queue = Queue.create () in
      List.iter ~f:(fun x -> Queue.push x queue) !lst;
      queue

    let f g f =
      n := 0;
      m := 0;
      (*
let t1 = Timer.make () in
*)
      let v =
        NSet.fold
          (fun x v ->
            incr n;
            NMap.add x D.bot v)
          g.domain
          NMap.empty
      in
      (*
let t1 = Timer.get t1 in
let t2 = Timer.make () in
*)
      let w = { set = g.domain; queue = traverse_all g } in
      (*
let t2 = Timer.get t2 in
let t3 = Timer.make () in
*)
      let res = iterate g f v w in
      (*
let t3 = Timer.get t3 in
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

(* Worklist of the imperative solvers. All the nodes are initially
   queued, in reverse postorder; a node is queued again (with [push])
   each time the value of one of its predecessors changes, until a
   fixpoint is reached. A node that is already in the queue is not
   queued a second time, so the queue never holds more nodes than the
   domain: we can use a circular buffer of a fixed size, rather than a
   list and then a [Queue.t], which would both be promoted to the major
   heap for large graphs. *)
module Worklist
    (N : sig
      type t
    end)
    (NSet : ISet with type elt = N.t) : sig
  type t

  val create : NSet.t -> ((N.t -> unit) -> N.t -> unit) -> t
  (** [create domain iter_children] queues all the nodes of [domain],
      in reverse postorder of the graph defined by [iter_children]. *)

  val length : t -> int

  val is_empty : t -> bool

  val pop : t -> N.t

  val push : N.t -> t -> unit
  (** Does nothing when the node is already in the queue, or is not in
      the domain. *)
end = struct
  type t =
    { data : N.t array
    ; mutable head : int
    ; mutable length : int
    ; set : NSet.t (* Nodes of the domain which are not in the queue *)
    }

  let create domain iter_children =
    (* [ISet] has no [cardinal] *)
    let count = ref 0 in
    let first = ref None in
    NSet.iter
      (fun x ->
        if !count = 0 then first := Some x;
        incr count)
      domain;
    let to_visit = NSet.copy domain in
    match !first with
    | None -> { data = [||]; head = 0; length = 0; set = to_visit }
    | Some x ->
        let data = Array.make !count x in
        let pos = ref !count in
        let rec traverse x =
          if NSet.mem to_visit x
          then (
            NSet.remove to_visit x;
            iter_children traverse x;
            decr pos;
            data.(!pos) <- x)
        in
        NSet.iter traverse domain;
        assert (!pos = 0);
        { data; head = 0; length = !count; set = to_visit }

  let length w = w.length

  let is_empty w = w.length = 0

  let pop w =
    let x = w.data.(w.head) in
    let head = w.head + 1 in
    w.head <- (if head = Array.length w.data then 0 else head);
    w.length <- w.length - 1;
    NSet.add w.set x;
    x

  let push x w =
    if NSet.mem w.set x
    then (
      let size = Array.length w.data in
      let i = w.head + w.length in
      w.data.(if i >= size then i - size else i) <- x;
      w.length <- w.length + 1;
      NSet.remove w.set x)
end

module Make_Imperative
    (N : sig
      type t
    end)
    (NSet : ISet with type elt = N.t)
    (NTbl : Tbl with type key = N.t) =
struct
  type t =
    { domain : NSet.t
    ; iter_children : (N.t -> unit) -> N.t -> unit
    }

  let successors g x = NTbl.get g x

  let add_edge g x y = NTbl.set g x (y :: successors g x)

  let invert size g =
    let h = NTbl.make size [] in
    NSet.iter (fun x -> g.iter_children (fun y -> add_edge h y x) x) g.domain;
    { domain = g.domain; iter_children = (fun f x -> List.iter ~f (successors h x)) }

  module type DOMAIN = sig
    type t

    val equal : t -> t -> bool

    val bot : t
  end

  module Solver (D : DOMAIN) = struct
    let n = ref 0

    let m = ref 0

    module W = Worklist (N) (NSet)

    let is_empty = W.is_empty

    let pop = W.pop

    let push = W.push

    let rec iterate g ~update f v w =
      if is_empty w
      then v
      else
        let x = pop w in
        let a = NTbl.get v x in
        incr m;
        let b = f ~update v x in
        if not (D.equal a b)
        then (
          NTbl.set v x b;
          g.iter_children (fun y -> push y w) x);
        iterate g ~update f v w

    let traverse_all g =
      let w = W.create g.domain g.iter_children in
      n := W.length w;
      w

    let check g v f report =
      let update ~children:_ _ = () in
      NSet.iter
        (fun x ->
          let a = NTbl.get v x in
          let b = f ~update v x in
          if not (D.equal a b)
          then (
            NTbl.set v x b;
            report x a b))
        g.domain

    let f' size g f =
      n := 0;
      m := 0;
      (*
let t1 = Timer.make () in
*)
      let v = NTbl.make size D.bot in
      (*
let t1 = Timer.get t1 in
let t2 = Timer.make () in
*)
      let w = traverse_all g in
      (*
let t2 = Timer.get t2 in
let t3 = Timer.make () in
*)
      let update ~children x =
        if children then g.iter_children (fun y -> push y w) x else push x w
      in
      let res = iterate g ~update f v w in
      (*
let t3 = Timer.get t3 in
      Format.eprintf "YYY %.2f %.2f %.2f@." t1 t2 t3;
      Format.eprintf "YYY %d %d (%f)@." !m !n (float !m /. float !n);
*)
      res

    let f size g f = f' size g (fun ~update:_ v x -> f v x)
  end
end

module type ACTION = sig
  type t
end

module type DOMAIN = sig
  type t

  val equal : t -> t -> bool

  val bot : t

  val top : t

  val join : t -> t -> t
end

module Solver
    (N : sig
      type t
    end)
    (NSet : ISet with type elt = N.t)
    (NTbl : Tbl with type key = N.t)
    (A : ACTION)
    (D : DOMAIN) =
struct
  type t =
    { domain : NSet.t
    ; iter_children : (N.t -> A.t -> unit) -> N.t -> unit
    }

  module W = Worklist (N) (NSet)

  let is_empty = W.is_empty

  let pop = W.pop

  let push = W.push

  let rec iterate g f ~state w =
    if not (is_empty w)
    then (
      let dep = pop w in
      if not (D.equal (NTbl.get state dep) D.bot)
      then
        g.iter_children
          (fun target action ->
            let a = NTbl.get state target in
            if not (D.equal a D.top)
            then
              let b = D.join a (f ~state ~dep ~target ~action) in
              if not (D.equal a b)
              then (
                NTbl.set state target b;
                push target w))
          dep;
      iterate g f ~state w)

  let traverse_all g = W.create g.domain (fun f x -> g.iter_children (fun y _ -> f y) x)

  let f ~state g f =
    let w = traverse_all g in
    iterate g f ~state w
end
