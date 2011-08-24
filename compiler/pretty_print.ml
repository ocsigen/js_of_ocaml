(* Js_of_ocaml compiler
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

type elt =
    Text of string
  | Break of string * int
  | Start_group of int
  | End_group

type t =
  { mutable indent : int;
    mutable box_indent : int;
    mutable prev_indents : (int * int) list;
    mutable limit : int;
    mutable cur : int;

    mutable l : elt list;

    mutable n : int;
    mutable w : int;

    mutable compact : bool;
    output : string -> int -> int -> unit }

let spaces = String.make 80 ' '

let rec output_spaces st n =
  st.output spaces 0 (min n 80);
  if n > 80 then output_spaces st (n - 80)

let output_newline st = st.output "\n" 0 1

let rec flat_render st l =
  match l with
    Text s :: r | Break (s, _) :: r ->
      st.output s 0 (String.length s); flat_render st r
  | _ :: r ->
      flat_render st r
  | [] ->
      ()

let rec push st e =
  if st.n = 0 then begin
    (* Vertical rendering *)
    match e with
      Text s ->
        st.output s 0 (String.length s);
        st.cur <- st.cur + String.length s
    | Break (_, offs) ->
        output_newline st;
        let indent = st.box_indent + offs in
        st.indent <- indent;
        output_spaces st indent;
        st.limit <- max (indent + 60) 78;
        st.cur <- st.indent
    | Start_group n ->
        st.n <- 1;
        st.w <- st.limit - st.cur;
        st.prev_indents <- (st.box_indent, st.indent) :: st.prev_indents;
        st.indent <- st.indent + n;
        st.box_indent <- st.indent
    | End_group ->
        st.box_indent <- fst (List.hd st.prev_indents);
        st.indent <- snd (List.hd st.prev_indents);
        st.prev_indents <- List.tl st.prev_indents
  end else begin
    (* Fits? *)
    st.l <- e :: st.l;
    match e with
      Text s | Break (s, _) ->
        let w = st.w - String.length s in
        st.w <- w;
        if w < 0 then begin
          let l = List.rev st.l in
          st.l <- [];
          st.n <- 0;
          List.iter (fun e -> push st e) l
        end
    | Start_group _ ->
        st.n <- st.n + 1
    | End_group ->
        st.n <- st.n - 1;
        if st.n = 0 then begin
          flat_render st (List.rev st.l);
          st.box_indent <- fst (List.hd st.prev_indents);
          st.indent <- snd (List.hd st.prev_indents);
          st.prev_indents <- List.tl st.prev_indents;
          st.cur <- st.cur + st.w;
          st.l <- []
        end
  end

(****)

let string st s =
  if st.compact then st.output s 0 (String.length s) else push st (Text s)

let genbreak st s n =
  if st.compact then st.output s 0 (String.length s) else push st (Break (s, n))

let break_token = Break ("", 0)
let break st = if not st.compact then push st break_token

let space_token = Break (" ", 0)
let space st = if st.compact then st.output " " 0 1 else push st space_token

let start_group st n = if not st.compact then push st (Start_group n)
let end_group st = if not st.compact then push st End_group

(*

let render l =
  let st = { indent = 0; box_indent = 0; prev_indents = [];
             limit = 78; cur = 0; l = []; n = 0; w = 0;
             output = fun s i l -> output stdout s i l } in
  push st (Start_group 0);
  List.iter (fun e -> push st e) l;
  push st End_group;
  output_newline st

let rec tree n =
  if n = 0 then [Text "Leaf"] else
  [Start_group 10; Text "Node.... ("] @ tree (n - 1) @
  [Text ","; Break (" ", 0)] @ tree (n - 1) @ [Text ")"; End_group]

let _ =
for i = 1 to 10 do render (tree i) done

*)

let newline st =
  output_newline st;
  st.indent <- 0; st.box_indent <- 0; st.prev_indents <- [];
  st.cur <- 0; st.l <- []; st.n <- 0; st.w <- 0

let to_out_channel ch =
  { indent = 0; box_indent = 0; prev_indents = [];
    limit = 78; cur = 0; l = []; n = 0; w = 0;
    compact = false; output = fun s i l -> output ch s i l }

let to_buffer b =
  { indent = 0; box_indent = 0; prev_indents = [];
    limit = 78; cur = 0; l = []; n = 0; w = 0;
    compact = false; output = fun s i l -> Buffer.add_substring b s i l }

let set_compact st v = st.compact <- v
