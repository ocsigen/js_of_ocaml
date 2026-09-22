(* Js_of_ocaml compiler
 * http://www.ocsigen.org/js_of_ocaml/
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

type key = int

let bits = 5

let width = 1 lsl bits

let mask = width - 1

(* We do not want a function call here, even without cross-module
   inlining. *)
external phys_eq : 'a -> 'a -> bool = "%eq"

(* A node is an array of length [width]. The elements of a leaf (at
   shift 0) are the values; the elements of an inner node are its
   children. The unique value [absent] marks a missing binding or
   child. Nodes are never mutated once they are reachable from a
   map. *)
type node = Obj.t array

let absent : Obj.t = Obj.repr (ref 0)

let empty_node : node = Array.make width absent

(* [shift] is the number of bits the key is shifted by at the root:
   the map can hold the keys strictly below [1 lsl (shift + bits)]. *)
type +'a t =
  { shift : int
  ; root : node
  ; size : int
  }

let empty = { shift = 0; root = empty_node; size = 0 }

let in_range k shift =
  k >= 0 && (shift + bits >= Sys.int_size - 1 || k lsr (shift + bits) = 0)

(****)

let rec find_node (node : node) shift k : Obj.t =
  let c = Array.unsafe_get node ((k lsr shift) land mask) in
  if phys_eq c absent || shift = 0 then c else find_node (Obj.obj c) (shift - bits) k

let find k t =
  if not (in_range k t.shift)
  then raise Not_found
  else
    let c = find_node t.root t.shift k in
    if phys_eq c absent then raise Not_found else Obj.obj c

let find_opt k t =
  if not (in_range k t.shift)
  then None
  else
    let c = find_node t.root t.shift k in
    if phys_eq c absent then None else Some (Obj.obj c)

let mem k t = in_range k t.shift && not (phys_eq (find_node t.root t.shift k) absent)

(****)

let rec grow t k =
  if in_range k t.shift
  then t
  else
    let root =
      if t.size = 0
      then empty_node
      else
        let n = Array.make width absent in
        n.(0) <- Obj.repr t.root;
        n
    in
    grow { t with shift = t.shift + bits; root } k

let rec add_node (node : node) shift k (v : Obj.t) added : node =
  let i = (k lsr shift) land mask in
  let c = Array.unsafe_get node i in
  if shift = 0
  then
    if phys_eq c v
    then node
    else (
      if phys_eq c absent then added := true;
      let node' = Array.copy node in
      Array.unsafe_set node' i v;
      node')
  else
    let child : node = if phys_eq c absent then empty_node else Obj.obj c in
    let child' = add_node child (shift - bits) k v added in
    if phys_eq child' child
    then node
    else
      let node' = Array.copy node in
      Array.unsafe_set node' i (Obj.repr child');
      node'

let add k v t =
  if k < 0 then invalid_arg "Int_trie.add: negative key";
  let t = grow t k in
  let added = ref false in
  let root = add_node t.root t.shift k (Obj.repr v) added in
  if phys_eq root t.root
  then t
  else { t with root; size = (if !added then t.size + 1 else t.size) }

let singleton k v = add k v empty

let node_is_empty (node : node) =
  let rec loop i =
    i = width || (phys_eq (Array.unsafe_get node i) absent && loop (i + 1))
  in
  loop 0

let rec remove_node (node : node) shift k : node =
  let i = (k lsr shift) land mask in
  let c = Array.unsafe_get node i in
  if phys_eq c absent
  then node
  else if shift = 0
  then (
    let node' = Array.copy node in
    Array.unsafe_set node' i absent;
    node')
  else
    let child : node = Obj.obj c in
    let child' = remove_node child (shift - bits) k in
    if phys_eq child' child
    then node
    else
      let node' = Array.copy node in
      Array.unsafe_set node' i (if node_is_empty child' then absent else Obj.repr child');
      node'

let remove k t =
  if not (in_range k t.shift)
  then t
  else
    let root = remove_node t.root t.shift k in
    if phys_eq root t.root then t else { t with root; size = t.size - 1 }

let update k f t =
  match f (find_opt k t) with
  | None -> remove k t
  | Some v -> add k v t

let cardinal t = t.size

(****)

let rec iter_node f (node : node) shift prefix =
  for i = 0 to mask do
    let c = Array.unsafe_get node i in
    if not (phys_eq c absent)
    then
      if shift = 0
      then f (prefix lor i) (Obj.obj c)
      else iter_node f (Obj.obj c) (shift - bits) (prefix lor (i lsl shift))
  done

let iter f t = iter_node f t.root t.shift 0

let fold f t acc =
  let acc = ref acc in
  iter (fun k v -> acc := f k v !acc) t;
  !acc

let rec min_node (node : node) shift prefix i =
  if i = width
  then raise Not_found
  else
    let c = Array.unsafe_get node i in
    if phys_eq c absent
    then min_node node shift prefix (i + 1)
    else if shift = 0
    then prefix lor i, Obj.obj c
    else min_node (Obj.obj c) (shift - bits) (prefix lor (i lsl shift)) 0

let choose t = min_node t.root t.shift 0 0

let rec max_node (node : node) shift prefix i =
  if i < 0
  then raise Not_found
  else
    let c = Array.unsafe_get node i in
    if phys_eq c absent
    then max_node node shift prefix (i - 1)
    else if shift = 0
    then prefix lor i, Obj.obj c
    else max_node (Obj.obj c) (shift - bits) (prefix lor (i lsl shift)) mask

let max_binding t = max_node t.root t.shift 0 mask

(****)

(* Returns [absent] when the resulting node is empty *)
let rec filter_map_node f (node : node) shift prefix count : Obj.t =
  let node' = ref empty_node in
  for i = 0 to mask do
    let c = Array.unsafe_get node i in
    if not (phys_eq c absent)
    then
      let c' =
        if shift = 0
        then (
          match f (prefix lor i) (Obj.obj c) with
          | None -> absent
          | Some v ->
              incr count;
              Obj.repr v)
        else filter_map_node f (Obj.obj c) (shift - bits) (prefix lor (i lsl shift)) count
      in
      if not (phys_eq c' absent)
      then (
        if phys_eq !node' empty_node then node' := Array.make width absent;
        Array.unsafe_set !node' i c')
  done;
  if phys_eq !node' empty_node then absent else Obj.repr !node'

let filter_map f t =
  let count = ref 0 in
  let root = filter_map_node f t.root t.shift 0 count in
  if phys_eq root absent
  then empty
  else { shift = t.shift; root = Obj.obj root; size = !count }

let filter p t = filter_map (fun k v -> if p k v then Some v else None) t

let rec mapi_node f (node : node) shift prefix : node =
  let node' = Array.make width absent in
  for i = 0 to mask do
    let c = Array.unsafe_get node i in
    if not (phys_eq c absent)
    then
      Array.unsafe_set
        node'
        i
        (if shift = 0
         then Obj.repr (f (prefix lor i) (Obj.obj c))
         else Obj.repr (mapi_node f (Obj.obj c) (shift - bits) (prefix lor (i lsl shift))))
  done;
  node'

let mapi f t =
  if t.size = 0 then empty else { t with root = mapi_node f t.root t.shift 0 }

let map f t = mapi (fun _ v -> f v) t

(****)

let rec equal_node eq (n1 : node) (n2 : node) shift =
  phys_eq n1 n2
  ||
  let rec loop i =
    i = width
    ||
    let c1 = Array.unsafe_get n1 i in
    let c2 = Array.unsafe_get n2 i in
    (if phys_eq c1 absent || phys_eq c2 absent
     then phys_eq c1 c2
     else if shift = 0
     then eq (Obj.obj c1) (Obj.obj c2)
     else equal_node eq (Obj.obj c1) (Obj.obj c2) (shift - bits))
    && loop (i + 1)
  in
  loop 0

let bindings_rev t = fold (fun k v acc -> (k, v) :: acc) t []

let equal eq t1 t2 =
  t1.size = t2.size
  &&
  if t1.shift = t2.shift
  then equal_node eq t1.root t2.root t1.shift
  else
    List.equal
      ~eq:(fun (k1, v1) (k2, v2) -> k1 = k2 && eq v1 v2)
      (bindings_rev t1)
      (bindings_rev t2)

let to_rev_seq t = List.to_seq (bindings_rev t)

let bindings t = List.rev (bindings_rev t)
