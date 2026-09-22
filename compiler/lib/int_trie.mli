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

(** PROTOTYPE. Persistent maps with small non-negative integer keys,
    implemented as a 32-way trie. Compared to [Map.Make (Int)], a
    lookup follows 3 or 4 pointers rather than about 17 for a map with
    100 000 bindings. The functions have the same semantics as the
    ones from [Map.S]; in particular, the bindings are always visited
    in increasing key order. *)

type key = int

type +'a t

val empty : 'a t

val singleton : key -> 'a -> 'a t

val add : key -> 'a -> 'a t -> 'a t

val remove : key -> 'a t -> 'a t

val update : key -> ('a option -> 'a option) -> 'a t -> 'a t

val find : key -> 'a t -> 'a

val find_opt : key -> 'a t -> 'a option

val mem : key -> 'a t -> bool

val cardinal : 'a t -> int

val choose : 'a t -> key * 'a

val max_binding : 'a t -> key * 'a

val iter : (key -> 'a -> unit) -> 'a t -> unit

val fold : (key -> 'a -> 'b -> 'b) -> 'a t -> 'b -> 'b

val map : ('a -> 'b) -> 'a t -> 'b t

val mapi : (key -> 'a -> 'b) -> 'a t -> 'b t

val filter : (key -> 'a -> bool) -> 'a t -> 'a t

val filter_map : (key -> 'a -> 'b option) -> 'a t -> 'b t

val equal : ('a -> 'a -> bool) -> 'a t -> 'a t -> bool

val to_rev_seq : 'a t -> (key * 'a) Seq.t

val bindings : 'a t -> (key * 'a) list
