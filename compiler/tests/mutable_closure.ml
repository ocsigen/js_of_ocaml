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
let log_stop = log_start "Closure test suite (2)"

let direct = ref []
let indirect = ref []

let () =
  for i = 0 to 3 do
    let rec f = function
      | 0 -> i
      | -1 -> g (-2) (* deadcode or infinite loop *)
      | n -> g (pred n)
    and g = function
      | 0 -> i
      | -1 -> f (-2)  (* deadcode or infinite loop *)
      | n -> f (pred n)
    in
    direct   := f i :: !direct;
    indirect := (fun () -> f i) :: !indirect
  done;
  let indirect = List.map (fun f -> f ()) !indirect in
  let direct = !direct in
  assert (indirect = direct)

let () =
  let delayed = ref (fun () -> ()) in
  for i = 1 to 2 do
    let rec f n = function
      | 0 -> assert (i = n)
      | j -> delayed :=
         let prev = !delayed in
         (fun () -> prev (); f (succ n + i - i) (pred j))
    in f 0 i
  done;
  !delayed ();;



let _ =
  let l_fun = ref [] in
  let l_var = ref [] in
  let l_base = [0;1;2;3;4;5;6;7;8;9;10] in
  for i=0 to 10
  do
    l_fun := (fun () -> i)::!l_fun;
    l_var := i::!l_var;
  done;

  let sum l = List.fold_left (+) 0 l in

  let sum_base = sum l_base in
  if (sum !l_var) <> sum_base
  then log_failure "l_var"
  else if (sum (List.map (fun f -> f()) !l_fun)) <> sum_base
  then log_failure "l_fun"
  else log_success ()


let () = log_stop ()
