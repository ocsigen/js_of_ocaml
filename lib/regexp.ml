(* Js_of_ocaml library
 * http://www.ocsigen.org/js_of_ocaml/
 * Copyright (C) 2010 Jérôme Vouillon
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

(* Types *)
type regexp =
    {
      simple : Js.regExp Js.t Lazy.t;
      global : Js.regExp Js.t Lazy.t;
    }
type result = Js.match_result Js.t


(* Constructors *)
let regexp s =
  {
    simple = lazy (jsnew Js.regExp (Js.string s));
    global = lazy (jsnew Js.regExp_withFlags (Js.string s, Js.string "g"));
  }
let regexp_case_fold s =
  {
    simple = lazy (jsnew Js.regExp_withFlags (Js.string s, Js.string "i"));
    global = lazy (jsnew Js.regExp_withFlags (Js.string s, Js.string "ig"));
  }


(* Helpers (not exported) *)
let string_from s i = String.sub s i (String.length s - i)
let string_to s i = String.sub s 0 i
let app_opt f = function
  | None -> None
  | Some x -> Some (f x)
let blunt_str_array_get a i =
  Js.to_string (Js.Optdef.get (Js.array_get a i) (fun () -> (assert false)))
let (!!) = Lazy.force

let string_match re s i =
  app_opt
    Js.match_result
    (Js.Opt.to_option !!(re.simple)##exec(Js.string (string_from s i)))

let search re s i =
  app_opt
    (fun res_pre -> let res = Js.match_result res_pre in (res##index, res))
    (Js.Opt.to_option !!(re.simple)##exec(Js.string (string_from s i)))

let matched_string r = blunt_str_array_get r 0

let matched_group r i =
  app_opt Js.to_string (Js.Optdef.to_option (Js.array_get r i))

let global_replace re s s_by =
  Js.to_string (Js.string s)##replace(!!(re.global), Js.string s_by)
let replace_first re s s_by =
  Js.to_string (Js.string s)##replace(!!(re.simple), Js.string s_by)

let list_of_js_array a =
  let rec aux accu idx =
    if idx < 0
    then accu
    else aux (blunt_str_array_get a idx :: accu) (pred idx)
  in
  aux [] (pred a##length)

let split re s =
  list_of_js_array
    (Js.str_array (Js.string s)##split_regExp( !!(re.simple) ))
let bounded_split re s i =
  list_of_js_array
    (Js.str_array (Js.string s)##split_regExpLimited( !!(re.simple), i ))


(* More constructors *)

let quote s = 
  List.fold_left
    (fun s c -> global_replace (regexp c) s c)
    s
    ["\\\\"; "\\."; "\\("; "\\)"] (*TODO: complete*)
let regexp_string s = regexp (quote s)
let regexp_string_case_fold s = regexp_case_fold (quote s)
