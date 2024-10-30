(* Js_of_ocaml library
 * http://www.ocsigen.org/js_of_ocaml/
 * Copyright (C) 2010 Raphaël Proust, Jérôme Vouillon
 * Laboratoire PPS - CNRS Université Paris Diderot
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
open! Import

type regexp = Js.regExp Js.t

type result = Js.match_result Js.t

let regexp s = new%js Js.regExp_withFlags (Js.bytestring s) (Js.string "g")

let regexp_case_fold s = new%js Js.regExp_withFlags (Js.bytestring s) (Js.string "gi")

let regexp_with_flag s f =
  new%js Js.regExp_withFlags (Js.bytestring s) (Js.string ("g" ^ f))

let blunt_str_array_get a i =
  Js.to_bytestring (Js.Optdef.get (Js.array_get a i) (fun () -> assert false))

let string_match r s i =
  r##.lastIndex := i;
  Js.Opt.to_option (Js.Opt.map (r##exec (Js.bytestring s)) Js.match_result)

let search r s i =
  r##.lastIndex := i;
  Js.Opt.to_option
    (Js.Opt.map
       (r##exec (Js.bytestring s))
       (fun res_pre ->
         let res = Js.match_result res_pre in
         res##.index, res))

let search_forward = search

let matched_string r = blunt_str_array_get r 0

let matched_group r i =
  Js.Optdef.to_option (Js.Optdef.map (Js.array_get r i) Js.to_bytestring)

let quote_repl_re = new%js Js.regExp_withFlags (Js.string "[$]") (Js.string "g")

let quote_repl s = (Js.bytestring s)##replace quote_repl_re (Js.string "$$$$")

let global_replace r s s_by =
  r##.lastIndex := 0;
  Js.to_bytestring (Js.bytestring s)##(replace r (quote_repl s_by))

let replace_first r s s_by =
  let flags =
    match Js.to_bool r##.ignoreCase, Js.to_bool r##.multiline with
    | false, false -> Js.string ""
    | false, true -> Js.string "m"
    | true, false -> Js.string "i"
    | true, true -> Js.string "mi"
  in
  let r' = new%js Js.regExp_withFlags r##.source flags in
  Js.to_bytestring (Js.bytestring s)##(replace r' (quote_repl s_by))

let list_of_js_array a =
  let rec aux accu idx =
    if idx < 0 then accu else aux (blunt_str_array_get a idx :: accu) (idx - 1)
  in
  aux [] (a##.length - 1)

let split r s =
  r##.lastIndex := 0;
  list_of_js_array (Js.str_array (Js.bytestring s)##(split_regExp r))

let bounded_split r s i =
  r##.lastIndex := 0;
  list_of_js_array (Js.str_array (Js.bytestring s)##(split_regExpLimited r i))

(* More constructors *)

let quote_re = regexp "[\\][()\\\\|+*.?{}^$]"

let quote s = Js.to_bytestring (Js.bytestring s)##(replace quote_re (Js.string "\\$&"))

let regexp_string s = regexp (quote s)

let regexp_string_case_fold s = regexp_case_fold (quote s)
