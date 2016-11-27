(* Js_of_ocaml example
 * http://www.ocsigen.org/js_of_ocaml/
 * Copyright (C) 2013 Hugo Heuzard
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2 of the License, or
 * (at your option) any later version.
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

let log_stop = log_start "Closure test suite"


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
