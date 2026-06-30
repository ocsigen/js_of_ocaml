(* Js_of_ocaml toplevel
 * http://www.ocsigen.org/js_of_ocaml/
 * Copyright (C) 2011 Jérôme Vouillon
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

(* Toplevel input history, persisted in [localStorage]. *)

open Js_of_ocaml

let data = ref [| "" |]

let idx = ref 0

let get_storage () =
  match Js.Optdef.to_option Dom_html.window##.localStorage with
  | exception _ -> raise Not_found
  | None -> raise Not_found
  | Some t -> t

let setup () =
  try
    let s = get_storage () in
    match Js.Opt.to_option (s##getItem (Js.string "history")) with
    | None -> raise Not_found
    | Some s ->
        let a = Json.unsafe_input s in
        data := a;
        idx := Array.length a - 1
  with _ -> ()

let push text =
  let l = Array.length !data in
  let n = Array.make (l + 1) "" in
  !data.(l - 1) <- text;
  Array.blit !data 0 n 0 l;
  data := n;
  idx := l;
  try
    let s = get_storage () in
    let str = Json.output !data in
    s##setItem (Js.string "history") str
  with Not_found -> ()

let current text = !data.(!idx) <- text

let previous textbox =
  if !idx > 0
  then (
    decr idx;
    textbox##.value := Js.string !data.(!idx))

let next textbox =
  if !idx < Array.length !data - 1
  then (
    incr idx;
    textbox##.value := Js.string !data.(!idx))
