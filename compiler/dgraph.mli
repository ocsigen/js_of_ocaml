(* Js_of_ocaml compiler
 * http://www.ocsigen.org/js_of_ocaml/
 * Copyright (C) 2010 Jérôme Vouillon
 * Laboratoire PPS - CNRS Université Paris Diderot
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
 *)

module Make (N : sig type t end)
            (NSet : Set.S with type elt = N.t)
            (NMap : Map.S with type key = N.t) : sig

  type t =
    { domain : NSet.t;
      fold_children : 'a . (N.t -> 'a -> 'a) -> N.t -> 'a -> 'a }

  val invert : t -> t

  module type DOMAIN = sig type t val equal : t -> t -> bool val bot : t end

  module Solver (D : DOMAIN) : sig
    val f : t -> (D.t NMap.t -> N.t -> D.t) -> D.t NMap.t
  end

end
