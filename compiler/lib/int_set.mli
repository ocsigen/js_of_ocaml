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

(** Persistent sets of non-negative integers.

    The set is a Patricia tree (a binary trie indexed by the bits of
    the elements, with path compression) whose leaves are small
    bitmaps. A set of a few elements is just a few small blocks,
    whatever the elements, and dense sets are packed 32 elements per
    leaf. Set operations ([union], [inter], [diff], [equal]) work
    structurally. The interface is a subset of [Set.S] (extended as
    in {!Stdlib.Set.S}), with the same semantics; in particular, the
    elements are always visited in increasing order. The only
    difference is that the elements must be non-negative: [add]
    raises [Invalid_argument] on a negative one. *)

module type S = sig
  type elt

  type t

  val empty : t

  val is_empty : t -> bool

  val mem : elt -> t -> bool

  val add : elt -> t -> t
  (** Returns the set itself when the element is already present. *)

  val singleton : elt -> t

  val remove : elt -> t -> t
  (** Returns the set itself when the element is not present. *)

  val union : t -> t -> t

  val inter : t -> t -> t

  val diff : t -> t -> t

  val equal : t -> t -> bool

  val compare : t -> t -> int
  (** A total order, compatible with [equal]. *)

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
  (** Returns the smallest element. *)

  val of_list : elt list -> t

  val to_seq : t -> elt Seq.t

  val compare_cardinal_with : t -> int -> int
  (** [compare_cardinal_with s n] is [compare (cardinal s) n]. *)

  val to_list_bounded : int -> t -> elt list option
  (** [to_list_bounded n s] returns [Some (elements s)] if [s] has at
      most [n] elements, and [None] otherwise. *)
end

include S with type elt = int
