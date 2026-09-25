(* Js_of_ocaml tests
 * http://www.ocsigen.org/js_of_ocaml/
 * Copyright (C) 2026 Ocsigen team
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

(* The exception bound by a handler is not defined before the loop
   containing the handler: its untagging must not be hoisted out of the
   loop. An exception is never an integer, so the untagging is on a dead
   path. *)

let[@inline never] g c n =
  let s = ref 0 in
  for i = 1 to n do
    try if i > 0 then raise Exit
    with e -> if c then s := !s + (Obj.magic e : int) else incr s
  done;
  !s

let () =
  let n = Array.length Sys.argv + 2 in
  assert (g (n > 100) n = n)
