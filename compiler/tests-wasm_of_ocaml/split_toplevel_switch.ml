(* Wasm_of_ocaml compiler
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

(* Regression test for switch arms assigning a merge local before
   branching to the join, with [toplevel_split_size=5]. The counter
   prevents constant folding; the additions make the arms large enough
   to outline. Avoid printing, which would keep [stdout] live across
   the toplevel and prevent splitting. *)

let counter = ref 0

let next () =
  incr counter;
  !counter

let check ~expected v = assert (v = expected)

(* [next ()] returns 1, 2, 3 and 4 in the successive switches, so each
   arm is selected once, in order. *)

let () =
  check
    ~expected:11
    (match next () with
    | 1 -> !counter + 10
    | 2 -> !counter + 20
    | 3 -> !counter + 30
    | _ -> !counter + 40)

let () =
  check
    ~expected:22
    (match next () with
    | 1 -> !counter + 10
    | 2 -> !counter + 20
    | 3 -> !counter + 30
    | _ -> !counter + 40)

let () =
  check
    ~expected:33
    (match next () with
    | 1 -> !counter + 10
    | 2 -> !counter + 20
    | 3 -> !counter + 30
    | _ -> !counter + 40)

let () =
  check
    ~expected:44
    (match next () with
    | 1 -> !counter + 10
    | 2 -> !counter + 20
    | 3 -> !counter + 30
    | _ -> !counter + 40)
