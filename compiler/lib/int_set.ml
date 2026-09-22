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

module type S = sig
  type elt

  type t

  val empty : t

  val is_empty : t -> bool

  val mem : elt -> t -> bool

  val add : elt -> t -> t

  val singleton : elt -> t

  val remove : elt -> t -> t

  val union : t -> t -> t

  val inter : t -> t -> t

  val diff : t -> t -> t

  val equal : t -> t -> bool

  val compare : t -> t -> int

  val subset : t -> t -> bool

  val disjoint : t -> t -> bool

  val iter : (elt -> unit) -> t -> unit

  val fold : (elt -> 'a -> 'a) -> t -> 'a -> 'a

  val for_all : (elt -> bool) -> t -> bool

  val exists : (elt -> bool) -> t -> bool

  val filter : (elt -> bool) -> t -> t

  val map : (elt -> elt) -> t -> t

  val cardinal : t -> int

  val elements : t -> elt list

  val min_elt : t -> elt

  val max_elt : t -> elt

  val choose : t -> elt

  val of_list : elt list -> t

  val to_seq : t -> elt Seq.t

  val compare_cardinal_with : t -> int -> int

  val to_list_bounded : int -> t -> elt list option
end

type elt = int

(* Leaves are bitmaps of [leaf_width] consecutive elements (on 32-bit
   platforms, integers only have 31 bits, so we use smaller leaves).
   The prefix of a leaf is its first element. Branches have a single
   bit set in [mask]: the elements of [left] have this bit unset, and
   the ones of [right] set; the bits of [prefix] above [mask] are
   common to all elements, and the other ones are zero. All elements
   being non-negative, the order of the elements coincides with the
   left-to-right order. *)
let leaf_bits = if Sys.int_size >= 32 then 5 else 4

let leaf_width = 1 lsl leaf_bits

let leaf_mask = leaf_width - 1

type t =
  | Empty
  | Leaf of
      { prefix : int
      ; bits : int
      }
  | Branch of
      { prefix : int
      ; mask : int
      ; left : t
      ; right : t
      ; size : int
      }

external phys_eq : 'a -> 'a -> bool = "%eq"

let empty = Empty

let popcount b =
  (* Only up to 32 bits are set *)
  let b = b - ((b lsr 1) land 0x55555555) in
  let b = (b land 0x33333333) + ((b lsr 2) land 0x33333333) in
  let b = (b + (b lsr 4)) land 0x0F0F0F0F in
  ((b * 0x01010101) lsr 24) land 0xFF

(* Index of the lowest set bit *)
let lowest_bit_index b = popcount ((b land -b) - 1)

let highest_bit x =
  let x = x lor (x lsr 1) in
  let x = x lor (x lsr 2) in
  let x = x lor (x lsr 4) in
  let x = x lor (x lsr 8) in
  let x = x lor (x lsr 16) in
  let x = x lor (x lsr 32) in
  x - (x lsr 1)

let cardinal t =
  match t with
  | Empty -> 0
  | Leaf { bits; _ } -> popcount bits
  | Branch { size; _ } -> size

let is_empty t =
  match t with
  | Empty -> true
  | Leaf _ | Branch _ -> false

(* Bits of [k] above [mask] *)
let mask_above k mask = k land lnot ((mask lsl 1) - 1)

let match_prefix k prefix mask = mask_above k mask = prefix

let zero_bit k mask = k land mask = 0

let leaf_prefix k = k land lnot leaf_mask

let leaf_bit k = 1 lsl (k land leaf_mask)

let branch prefix mask left right =
  Branch { prefix; mask; left; right; size = cardinal left + cardinal right }

(* Same as [branch], but drops empty children and preserves sharing
   with an existing branch [t] *)
let branch' t prefix mask left right left0 right0 =
  match left, right with
  | Empty, t' | t', Empty -> t'
  | _ ->
      if phys_eq left left0 && phys_eq right right0
      then t
      else branch prefix mask left right

(* Combines two non-empty subtrees with distinct prefixes [p0] and [p1] *)
let join p0 t0 p1 t1 =
  let mask = highest_bit (p0 lxor p1) in
  let prefix = mask_above p0 mask in
  if zero_bit p0 mask then branch prefix mask t0 t1 else branch prefix mask t1 t0

(* Lookups *)

let rec mem k t =
  match t with
  | Empty -> false
  | Leaf { prefix; bits } -> leaf_prefix k = prefix && bits land leaf_bit k <> 0
  | Branch { prefix; mask; left; right; _ } ->
      match_prefix k prefix mask && mem k (if zero_bit k mask then left else right)

(* Updates *)

(* Adds all the elements of the leaf [lp, bits] *)
let rec add_leaf lp lbits t =
  match t with
  | Empty -> Leaf { prefix = lp; bits = lbits }
  | Leaf { prefix; bits } ->
      if prefix = lp
      then
        let bits' = bits lor lbits in
        if bits' = bits then t else Leaf { prefix; bits = bits' }
      else join lp (Leaf { prefix = lp; bits = lbits }) prefix t
  | Branch { prefix; mask; left; right; _ } ->
      if match_prefix lp prefix mask
      then
        if zero_bit lp mask
        then
          let left' = add_leaf lp lbits left in
          if phys_eq left' left then t else branch prefix mask left' right
        else
          let right' = add_leaf lp lbits right in
          if phys_eq right' right then t else branch prefix mask left right'
      else join lp (Leaf { prefix = lp; bits = lbits }) prefix t

let add k t =
  if k < 0 then invalid_arg "Int_set.add";
  add_leaf (leaf_prefix k) (leaf_bit k) t

let singleton k = add k Empty

let of_list l = List.fold_left l ~init:Empty ~f:(fun t k -> add k t)

let rec remove k t =
  match t with
  | Empty -> t
  | Leaf { prefix; bits } ->
      if leaf_prefix k <> prefix
      then t
      else
        let bits' = bits land lnot (leaf_bit k) in
        if bits' = bits
        then t
        else if bits' = 0
        then Empty
        else Leaf { prefix; bits = bits' }
  | Branch { prefix; mask; left; right; _ } ->
      if not (match_prefix k prefix mask)
      then t
      else if zero_bit k mask
      then branch' t prefix mask (remove k left) right left right
      else branch' t prefix mask left (remove k right) left right

(* Traversals *)

let iter_leaf f prefix bits =
  let b = ref bits in
  while !b <> 0 do
    let low = !b land - !b in
    f (prefix lor lowest_bit_index low);
    b := !b lxor low
  done

let rec iter f t =
  match t with
  | Empty -> ()
  | Leaf { prefix; bits } -> iter_leaf f prefix bits
  | Branch { left; right; _ } ->
      iter f left;
      iter f right

let rec fold_leaf f prefix bits acc =
  if bits = 0
  then acc
  else
    let low = bits land -bits in
    fold_leaf f prefix (bits lxor low) (f (prefix lor lowest_bit_index low) acc)

let rec fold f t acc =
  match t with
  | Empty -> acc
  | Leaf { prefix; bits } -> fold_leaf f prefix bits acc
  | Branch { left; right; _ } -> fold f right (fold f left acc)

(* Short-circuiting, without going through [iter] *)
let rec exists p t =
  match t with
  | Empty -> false
  | Leaf { prefix; bits } ->
      let rec loop b =
        b <> 0
        &&
        let low = b land -b in
        p (prefix lor lowest_bit_index low) || loop (b lxor low)
      in
      loop bits
  | Branch { left; right; _ } -> exists p left || exists p right

let rec for_all p t =
  match t with
  | Empty -> true
  | Leaf { prefix; bits } ->
      let rec loop b =
        b = 0
        ||
        let low = b land -b in
        p (prefix lor lowest_bit_index low) && loop (b lxor low)
      in
      loop bits
  | Branch { left; right; _ } -> for_all p left && for_all p right

let elements t = List.rev (fold (fun k acc -> k :: acc) t [])

let rec to_seq_aux t (rest : elt Seq.t) () =
  match t with
  | Empty -> rest ()
  | Leaf { prefix; bits } ->
      let rec leaf bits () =
        if bits = 0
        then rest ()
        else
          let low = bits land -bits in
          Seq.Cons (prefix lor lowest_bit_index low, leaf (bits lxor low))
      in
      leaf bits ()
  | Branch { left; right; _ } -> to_seq_aux left (to_seq_aux right rest) ()

let to_seq t = to_seq_aux t Seq.empty

let rec min_elt t =
  match t with
  | Empty -> raise Not_found
  | Leaf { prefix; bits } -> prefix lor lowest_bit_index bits
  | Branch { left; _ } -> min_elt left

let choose = min_elt

let rec max_elt t =
  match t with
  | Empty -> raise Not_found
  | Leaf { prefix; bits } -> prefix lor popcount (highest_bit bits - 1)
  | Branch { right; _ } -> max_elt right

let rec filter p t =
  match t with
  | Empty -> t
  | Leaf { prefix; bits } ->
      let bits' = ref 0 in
      iter_leaf (fun k -> if p k then bits' := !bits' lor leaf_bit k) prefix bits;
      if !bits' = bits
      then t
      else if !bits' = 0
      then Empty
      else Leaf { prefix; bits = !bits' }
  | Branch { prefix; mask; left; right; _ } ->
      branch' t prefix mask (filter p left) (filter p right) left right

let map f t = fold (fun k acc -> add (f k) acc) t Empty

let compare_cardinal_with t n = Int.compare (cardinal t) n

let to_list_bounded n t = if cardinal t <= n then Some (elements t) else None

(* Set operations (Okasaki and Gill, "Fast mergeable integer maps") *)

let rec union s t =
  if phys_eq s t
  then s
  else
    match s, t with
    | Empty, _ -> t
    | _, Empty -> s
    | Leaf { prefix = p; bits = b }, Leaf { prefix = q; bits = c } when p = q ->
        let d = b lor c in
        if d = b then s else if d = c then t else Leaf { prefix = p; bits = d }
    | Leaf { prefix; bits }, _ -> add_leaf prefix bits t
    | _, Leaf { prefix; bits } -> add_leaf prefix bits s
    | ( Branch { prefix = p; mask = m; left = s0; right = s1; _ }
      , Branch { prefix = q; mask = n; left = t0; right = t1; _ } ) ->
        if m = n && p = q
        then
          let left = union s0 t0 and right = union s1 t1 in
          if phys_eq left s0 && phys_eq right s1
          then s
          else if phys_eq left t0 && phys_eq right t1
          then t
          else branch p m left right
        else if m > n && match_prefix q p m
        then
          if zero_bit q m
          then
            let left = union s0 t in
            if phys_eq left s0 then s else branch p m left s1
          else
            let right = union s1 t in
            if phys_eq right s1 then s else branch p m s0 right
        else if m < n && match_prefix p q n
        then
          if zero_bit p n
          then
            let left = union s t0 in
            if phys_eq left t0 then t else branch q n left t1
          else
            let right = union s t1 in
            if phys_eq right t1 then t else branch q n t0 right
        else join p s q t

(* The bitmap of the leaf of [t] with prefix [lp] (0 if there is none) *)
let rec leaf_bits_at lp t =
  match t with
  | Empty -> 0
  | Leaf { prefix; bits } -> if prefix = lp then bits else 0
  | Branch { prefix; mask; left; right; _ } ->
      if not (match_prefix lp prefix mask)
      then 0
      else leaf_bits_at lp (if zero_bit lp mask then left else right)

let inter_leaf s lp lbits t =
  let bits = lbits land leaf_bits_at lp t in
  if bits = lbits then s else if bits = 0 then Empty else Leaf { prefix = lp; bits }

let rec inter s t =
  if phys_eq s t
  then s
  else
    match s, t with
    | Empty, _ | _, Empty -> Empty
    | Leaf { prefix; bits }, _ -> inter_leaf s prefix bits t
    | _, Leaf { prefix; bits } -> inter_leaf t prefix bits s
    | ( Branch { prefix = p; mask = m; left = s0; right = s1; _ }
      , Branch { prefix = q; mask = n; left = t0; right = t1; _ } ) ->
        if m = n && p = q
        then branch' s p m (inter s0 t0) (inter s1 t1) s0 s1
        else if m > n && match_prefix q p m
        then inter (if zero_bit q m then s0 else s1) t
        else if m < n && match_prefix p q n
        then inter s (if zero_bit p n then t0 else t1)
        else Empty

(* Removes the elements of the leaf [lp, lbits] from [t] *)
let rec remove_leaf lp lbits t =
  match t with
  | Empty -> t
  | Leaf { prefix; bits } ->
      if prefix <> lp
      then t
      else
        let bits' = bits land lnot lbits in
        if bits' = bits
        then t
        else if bits' = 0
        then Empty
        else Leaf { prefix; bits = bits' }
  | Branch { prefix; mask; left; right; _ } ->
      if not (match_prefix lp prefix mask)
      then t
      else if zero_bit lp mask
      then branch' t prefix mask (remove_leaf lp lbits left) right left right
      else branch' t prefix mask left (remove_leaf lp lbits right) left right

let rec diff s t =
  if phys_eq s t
  then Empty
  else
    match s, t with
    | Empty, _ -> Empty
    | _, Empty -> s
    | Leaf { prefix; bits }, _ ->
        let bits' = bits land lnot (leaf_bits_at prefix t) in
        if bits' = bits
        then s
        else if bits' = 0
        then Empty
        else Leaf { prefix; bits = bits' }
    | _, Leaf { prefix; bits } -> remove_leaf prefix bits s
    | ( Branch { prefix = p; mask = m; left = s0; right = s1; _ }
      , Branch { prefix = q; mask = n; left = t0; right = t1; _ } ) ->
        if m = n && p = q
        then branch' s p m (diff s0 t0) (diff s1 t1) s0 s1
        else if m > n && match_prefix q p m
        then
          if zero_bit q m
          then branch' s p m (diff s0 t) s1 s0 s1
          else branch' s p m s0 (diff s1 t) s0 s1
        else if m < n && match_prefix p q n
        then diff s (if zero_bit p n then t0 else t1)
        else s

let rec subset s t =
  phys_eq s t
  ||
  (* The size is cached, so this also prunes inside the recursion *)
  cardinal s <= cardinal t
  &&
  match s, t with
  | Empty, _ -> true
  | _, Empty -> false
  | Leaf { prefix; bits }, _ -> bits land lnot (leaf_bits_at prefix t) = 0
  | Branch _, Leaf _ -> false
  | ( Branch { prefix = p; mask = m; left = s0; right = s1; _ }
    , Branch { prefix = q; mask = n; left = t0; right = t1; _ } ) ->
      if m = n && p = q
      then subset s0 t0 && subset s1 t1
      else if m < n && match_prefix p q n
      then subset s (if zero_bit p n then t0 else t1)
      else false

let rec disjoint s t =
  (not (phys_eq s t))
  &&
  match s, t with
  | Empty, _ | _, Empty -> true
  | Leaf { prefix; bits }, _ -> bits land leaf_bits_at prefix t = 0
  | _, Leaf { prefix; bits } -> bits land leaf_bits_at prefix s = 0
  | ( Branch { prefix = p; mask = m; left = s0; right = s1; _ }
    , Branch { prefix = q; mask = n; left = t0; right = t1; _ } ) ->
      if m = n && p = q
      then disjoint s0 t0 && disjoint s1 t1
      else if m > n && match_prefix q p m
      then disjoint (if zero_bit q m then s0 else s1) t
      else if m < n && match_prefix p q n
      then disjoint s (if zero_bit p n then t0 else t1)
      else true

(* Comparisons. The structure of a set only depends on its elements,
   so sets can be compared structurally. *)

let rec equal s t =
  phys_eq s t
  ||
  match s, t with
  | Empty, Empty -> true
  | Leaf { prefix = p; bits = b }, Leaf { prefix = q; bits = c } -> p = q && b = c
  | ( Branch { prefix = p; mask = m; left = s0; right = s1; size = a }
    , Branch { prefix = q; mask = n; left = t0; right = t1; size = b } ) ->
      a = b && p = q && m = n && equal s0 t0 && equal s1 t1
  | (Empty | Leaf _ | Branch _), _ -> false

(* Lexicographic order on the sorted elements. The leaves are
   enumerated in order; two leaves with the same prefix are compared
   by their first differing element. *)
let rec push t stack =
  match t with
  | Empty -> stack
  | Leaf _ -> t :: stack
  | Branch { left; right; _ } -> push left (right :: stack)

let rec compare_stacks s1 s2 =
  match s1, s2 with
  | [], [] -> 0
  | [], _ :: _ -> -1
  | _ :: _, [] -> 1
  | Leaf { prefix = p; bits = b } :: r1, Leaf { prefix = q; bits = c } :: r2 ->
      if p <> q
      then Int.compare p q
      else if b = c
      then compare_stacks (push_list r1) (push_list r2)
      else
        (* Let [x] be the first differing element, which belongs to
           only one of the sets. If it belongs to [s], then [s] is
           smaller, unless [t] has no element after [x]. *)
        let low = b lxor c land -(b lxor c) in
        let rest bits r =
          bits land lnot ((low lsl 1) - 1) <> 0 || not (List.is_empty r)
        in
        if b land low <> 0
        then if rest c r2 then -1 else 1
        else if rest b r1
        then 1
        else -1
  | (Empty | Branch _) :: _, _ | _, (Empty | Branch _) :: _ -> assert false

and push_list stack =
  match stack with
  | [] -> []
  | t :: rest -> push t rest

let compare s t = if phys_eq s t then 0 else compare_stacks (push s []) (push t [])
