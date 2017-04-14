(* Js_of_ocaml example
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
 * GNU Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
 *)

let success_count_all = ref 0
let test_count_all = ref 0

let success_count = ref 0
let test_count = ref 0

let log_success () = incr success_count; incr test_count
let log_failure s =
  incr test_count;
  Format.printf "\tFAILURE: %s" s

let log_start s =
  Format.printf "START: %s\n" s;
  let log_stop () : unit =
    success_count_all := !success_count_all + !success_count;
    test_count_all := !test_count_all + !test_count;
    Format.printf "STOP: %s\n" s
  in log_stop

let raw_log x =
  Format.printf "\t\t %s" x

let log s = raw_log s
