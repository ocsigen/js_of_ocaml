(* Js_of_ocaml compiler
 * http://www.ocsigen.org/js_of_ocaml/
 * Copyright (C) 2017 Hugo Heuzard
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

open Common
let log_stop = log_start "Side effect test suite"
let i = ref 0

let side_effect yes label =
  if yes
  then begin
    Printf.printf "Side effect: %s\n%!" label;
    incr i
  end;
  0

let _ = side_effect false "this is only to avoid inlining"

let f = match side_effect true "Should only see this once" with
  | 0 | 1 | 2 -> Printf.printf "Please don't optimize this away\n%!"
  | _ -> Printf.printf "Or this\n%!"

let _ = if !i = 1
  then log_success ()
  else log_failure "side effect computed twice"

let _ = log_stop ()
