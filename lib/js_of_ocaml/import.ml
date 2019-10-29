(* Js_of_ocaml
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

module Poly = struct
  external ( < ) : 'a -> 'a -> bool = "%lessthan"

  external ( <= ) : 'a -> 'a -> bool = "%lessequal"

  external ( <> ) : 'a -> 'a -> bool = "%notequal"

  external ( = ) : 'a -> 'a -> bool = "%equal"

  external ( > ) : 'a -> 'a -> bool = "%greaterthan"

  external ( >= ) : 'a -> 'a -> bool = "%greaterequal"

  external compare : 'a -> 'a -> int = "%compare"

  external equal : 'a -> 'a -> bool = "%equal"
end

module Int_replace_polymorphic_compare = struct
  let ( < ) (x : int) y = x < y

  let ( <= ) (x : int) y = x <= y

  let ( <> ) (x : int) y = x <> y

  let ( = ) (x : int) y = x = y

  let ( > ) (x : int) y = x > y

  let ( >= ) (x : int) y = x >= y

  let compare (x : int) y = compare x y

  let equal (x : int) y = x = y

  let max (x : int) y = if x >= y then x else y

  let min (x : int) y = if x <= y then x else y
end

module String = struct
  include String

  let equal (x : string) (y : string) = Poly.equal x y
end

module Char = struct
  include Char

  let equal (x : char) (y : char) = Poly.equal x y
end

include Int_replace_polymorphic_compare
