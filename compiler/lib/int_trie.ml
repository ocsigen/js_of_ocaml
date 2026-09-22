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

(* We want this to be inlined even without cross-module inlining. *)
external phys_eq : 'a -> 'a -> bool = "%eq"

(* A node is an array of [width] slots. The slots of a leaf hold the
   values; the slots of an inner node hold its children. The unique
   value [absent] marks a missing binding or child. Nodes are never
   modified once they are part of a map.

   Slots are given a concrete (non-float) type (any such type would
   do), so that array accesses are compiled as plain pointer accesses
   rather than having to check at runtime whether the array is a flat
   float array. This is sound as the arrays are created with [absent]
   as initial value, and thus are never flat float arrays, whatever
   the values stored in them. *)
type slot = private int list

type node = slot array

external slot : 'a -> slot = "%identity"

external unslot : slot -> 'a = "%identity"

let absent : slot = slot (ref 0)

let empty_node : node = Array.make width absent

let is_absent (s : slot) = phys_eq s absent

(* [shift] is the number of bits the key is shifted by at the root;
   the map can hold the keys strictly below [1 lsl (shift + bits)]. It
   grows with the largest key, so that small maps remain shallow. *)
type +'a t =
  { shift : int
  ; root : node
  ; size : int
  }

let empty = { shift = 0; root = empty_node; size = 0 }

let in_range k shift =
  k >= 0 && (shift + bits >= Sys.int_size - 1 || k lsr (shift + bits) = 0)

let index k shift = (k lsr shift) land mask

let cardinal t = t.size

let is_empty t = t.size = 0

(* Lookups *)

let rec find_node (node : node) shift k =
  let c = Array.unsafe_get node (index k shift) in
  if is_absent c || shift = 0 then c else find_node (unslot c) (shift - bits) k

let find k t =
  if not (in_range k t.shift)
  then raise Not_found
  else
    let c = find_node t.root t.shift k in
    if is_absent c then raise Not_found else unslot c

let find_opt k t =
  if not (in_range k t.shift)
  then None
  else
    let c = find_node t.root t.shift k in
    if is_absent c then None else Some (unslot c)

let mem k t = in_range k t.shift && not (is_absent (find_node t.root t.shift k))

(* Updates *)

let rec grow t k =
  if in_range k t.shift
  then t
  else
    let root =
      if t.size = 0
      then empty_node
      else
        let n = Array.make width absent in
        n.(0) <- slot t.root;
        n
    in
    grow { t with shift = t.shift + bits; root } k

let rec add_node (node : node) shift k (v : slot) added : node =
  let i = index k shift in
  let c = Array.unsafe_get node i in
  if shift = 0
  then
    if phys_eq c v
    then node
    else (
      if is_absent c then added := true;
      let node' = Array.copy node in
      Array.unsafe_set node' i v;
      node')
  else
    let child : node = if is_absent c then empty_node else unslot c in
    let child' = add_node child (shift - bits) k v added in
    if phys_eq child' child
    then node
    else
      let node' = Array.copy node in
      Array.unsafe_set node' i (slot child');
      node'

let add k v t =
  if k < 0 then invalid_arg "Int_trie.add";
  let t = grow t k in
  let added = ref false in
  let root = add_node t.root t.shift k (slot v) added in
  if phys_eq root t.root
  then t
  else { t with root; size = (if !added then t.size + 1 else t.size) }

let singleton k v = add k v empty

let node_is_empty (node : node) =
  let rec loop i = i = width || (is_absent (Array.unsafe_get node i) && loop (i + 1)) in
  loop 0

(* Children are never left empty, so that the structure of a map only
   depends on its keys (and its height); [equal] relies on this. *)
let rec remove_node (node : node) shift k : node =
  let i = index k shift in
  let c = Array.unsafe_get node i in
  if is_absent c
  then node
  else if shift = 0
  then (
    let node' = Array.copy node in
    Array.unsafe_set node' i absent;
    node')
  else
    let child : node = unslot c in
    let child' = remove_node child (shift - bits) k in
    if phys_eq child' child
    then node
    else
      let node' = Array.copy node in
      Array.unsafe_set node' i (if node_is_empty child' then absent else slot child');
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

(* Traversals *)

let rec iter_node f (node : node) shift prefix =
  for i = 0 to mask do
    let c = Array.unsafe_get node i in
    if not (is_absent c)
    then
      if shift = 0
      then f (prefix lor i) (unslot c)
      else iter_node f (unslot c) (shift - bits) (prefix lor (i lsl shift))
  done

let iter f t = iter_node f t.root t.shift 0

let rec fold_node f (node : node) shift prefix acc =
  let acc = ref acc in
  for i = 0 to mask do
    let c = Array.unsafe_get node i in
    if not (is_absent c)
    then
      acc :=
        if shift = 0
        then f (prefix lor i) (unslot c) !acc
        else fold_node f (unslot c) (shift - bits) (prefix lor (i lsl shift)) !acc
  done;
  !acc

let fold f t acc = fold_node f t.root t.shift 0 acc

let rec min_node (node : node) shift prefix i =
  if i = width
  then raise Not_found
  else
    let c = Array.unsafe_get node i in
    if is_absent c
    then min_node node shift prefix (i + 1)
    else if shift = 0
    then prefix lor i, unslot c
    else min_node (unslot c) (shift - bits) (prefix lor (i lsl shift)) 0

let min_binding t = min_node t.root t.shift 0 0

let choose = min_binding

let rec max_node (node : node) shift prefix i =
  if i < 0
  then raise Not_found
  else
    let c = Array.unsafe_get node i in
    if is_absent c
    then max_node node shift prefix (i - 1)
    else if shift = 0
    then prefix lor i, unslot c
    else max_node (unslot c) (shift - bits) (prefix lor (i lsl shift)) mask

let max_binding t = max_node t.root t.shift 0 mask

let bindings t = List.rev (fold (fun k v acc -> (k, v) :: acc) t [])

let rec to_seq_node (node : node) shift prefix i (rest : (key * 'a) Seq.t) () =
  if i = width
  then rest ()
  else
    let c = Array.unsafe_get node i in
    let rest' = to_seq_node node shift prefix (i + 1) rest in
    if is_absent c
    then rest' ()
    else if shift = 0
    then Seq.Cons ((prefix lor i, unslot c), rest')
    else to_seq_node (unslot c) (shift - bits) (prefix lor (i lsl shift)) 0 rest' ()

let to_seq t = to_seq_node t.root t.shift 0 0 Seq.empty

let rec to_rev_seq_node (node : node) shift prefix i (rest : (key * 'a) Seq.t) () =
  if i < 0
  then rest ()
  else
    let c = Array.unsafe_get node i in
    let rest' = to_rev_seq_node node shift prefix (i - 1) rest in
    if is_absent c
    then rest' ()
    else if shift = 0
    then Seq.Cons ((prefix lor i, unslot c), rest')
    else
      to_rev_seq_node (unslot c) (shift - bits) (prefix lor (i lsl shift)) mask rest' ()

let to_rev_seq t = to_rev_seq_node t.root t.shift 0 mask Seq.empty

(* Whole-map transformations *)

let rec mapi_node f (node : node) shift prefix : node =
  let node' = Array.make width absent in
  for i = 0 to mask do
    let c = Array.unsafe_get node i in
    if not (is_absent c)
    then
      Array.unsafe_set
        node'
        i
        (if shift = 0
         then slot (f (prefix lor i) (unslot c))
         else slot (mapi_node f (unslot c) (shift - bits) (prefix lor (i lsl shift))))
  done;
  node'

let mapi f t =
  if t.size = 0 then empty else { t with root = mapi_node f t.root t.shift 0 }

let map f t = mapi (fun _ v -> f v) t

(* Returns [empty_node] when the resulting node is empty. (This
   function returns a node rather than a slot, so that the root of the
   resulting map is not reinterpreted from a slot: OxCaml's flambda2
   rejects this.) *)
let rec filter_map_node f (node : node) shift prefix count : node =
  let node' = ref empty_node in
  for i = 0 to mask do
    let c = Array.unsafe_get node i in
    if not (is_absent c)
    then
      let c' =
        if shift = 0
        then (
          match f (prefix lor i) (unslot c) with
          | None -> absent
          | Some v ->
              incr count;
              slot v)
        else
          let child =
            filter_map_node f (unslot c) (shift - bits) (prefix lor (i lsl shift)) count
          in
          if phys_eq child empty_node then absent else slot child
      in
      if not (is_absent c')
      then (
        if phys_eq !node' empty_node then node' := Array.make width absent;
        Array.unsafe_set !node' i c')
  done;
  !node'

let filter_map f t =
  let count = ref 0 in
  let root = filter_map_node f t.root t.shift 0 count in
  if phys_eq root empty_node then empty else { shift = t.shift; root; size = !count }

let filter p t = filter_map (fun k v -> if p k v then Some v else None) t

(* Comparison *)

let rec equal_node eq (n1 : node) (n2 : node) shift =
  phys_eq n1 n2
  ||
  let rec loop i =
    i = width
    ||
    let c1 = Array.unsafe_get n1 i in
    let c2 = Array.unsafe_get n2 i in
    (if is_absent c1 || is_absent c2
     then phys_eq c1 c2
     else if shift = 0
     then eq (unslot c1) (unslot c2)
     else equal_node eq (unslot c1) (unslot c2) (shift - bits))
    && loop (i + 1)
  in
  loop 0

let equal eq t1 t2 =
  t1.size = t2.size
  &&
  if t1.shift = t2.shift
  then equal_node eq t1.root t2.root t1.shift
  else
    (* The maps have different heights: compare their bindings *)
    let rec loop s1 s2 =
      match s1 (), s2 () with
      | Seq.Nil, Seq.Nil -> true
      | Seq.Cons ((k1, v1), s1), Seq.Cons ((k2, v2), s2) ->
          k1 = k2 && eq v1 v2 && loop s1 s2
      | Seq.Nil, Seq.Cons _ | Seq.Cons _, Seq.Nil -> false
    in
    loop (to_seq t1) (to_seq t2)

(* Bulk construction. All the nodes are freshly allocated and not yet
   part of any map, so they can be modified in place. *)

let rec add_mut (node : node) shift k (v : slot) added =
  let i = index k shift in
  if shift = 0
  then (
    if is_absent (Array.unsafe_get node i) then added := true;
    Array.unsafe_set node i v)
  else
    let c = Array.unsafe_get node i in
    let child : node =
      if is_absent c
      then (
        let child = Array.make width absent in
        Array.unsafe_set node i (slot child);
        child)
      else unslot c
    in
    add_mut child (shift - bits) k v added

let of_seq s =
  let shift = ref 0 in
  let root = ref (Array.make width absent) in
  let size = ref 0 in
  let added = ref false in
  Seq.iter
    (fun (k, v) ->
      if k < 0 then invalid_arg "Int_trie.of_seq";
      while not (in_range k !shift) do
        let n = Array.make width absent in
        if !size > 0 then n.(0) <- slot !root;
        root := n;
        shift := !shift + bits
      done;
      added := false;
      add_mut !root !shift k (slot v) added;
      if !added then incr size)
    s;
  if !size = 0 then empty else { shift = !shift; root = !root; size = !size }
