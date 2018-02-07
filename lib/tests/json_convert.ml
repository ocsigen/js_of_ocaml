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

(* Json conversion *)
open! Common
(*
let log_stop = log_start "Json"

let str =
  let b = Buffer.create 256 in
  for i = 0 to 255 do
    Buffer.add_char b (Char.chr i)
  done;
  Buffer.contents b

type t = int list * float option * string [@@deriving json]

let test t v =
  if v = Json.unsafe_input (Json.output v)
  then log_success ()
  else log_failure "Not equal";
  if v = Deriving_Json.from_string t (Js.to_string (Json.output v))
  then log_success ()
  else log_failure "Not equal";
  if v = Json.unsafe_input (Js.string (Deriving_Json.to_string t v))
  then log_success ()
  else log_failure "Not equal";
  if v = Deriving_Json.from_string t (Deriving_Json.to_string t v)
  then log_success ()
  else log_failure "Not equal"

let _ = test Json.t<t> ([1;2;3], Some 1., str)

type intseq = Z | S of int * intseq [@@deriving json]

let _ = test Json.t<intseq> (S (1, S (2, S (3, Z))))

type 'a seq = ZZ | SS of 'a * 'a seq [@@deriving json]

let _ = test Json.t<int seq> (SS (1, SS (2, SS (3, ZZ))))


module T = struct

  type 'a t = (string * 'a) array [@@deriving json]

  module StringMap = Map.Make(String)
  module Json_string_map_t(A : Deriving_Json.Json) : Deriving_Json.Json = struct
    module S = Json_t(A)
    include Deriving_Json.Convert(struct
        type a = A.a t
        type b = A.a StringMap.t
        let t = S.t
        let to_ : b -> A.a t = fun a -> Array.of_list (StringMap.bindings a)
        let from_ : A.a t -> b = fun l ->
          Array.fold_left
            (fun map (x,v) -> StringMap.add x v map)
            StringMap.empty
            l
      end)
  end
end

(* module T2 = T.Json_string_map_t(Json_t);; *)
(* type u = int StringMap.t deriving (Json) *)

let () = log_stop()
*)
